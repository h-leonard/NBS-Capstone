# Load packages and functions for Newborn Screening Hospital Reporting
# Do not access this file directly; "run_file_and_variable_setting.R"
# will automatically run this file.

libs <- c('xtable',
          'data.table',
          'stringr',
          'reshape2',
          'knitr',
          'markdown',
          'zoo',
          'ggplot2',
          'grid',
          'reshape',
          'lubridate',
          'dplyr',
          'pander',
          'shiny',
          'lazyeval',
          'toOrdinal',
          # 'mailR', - add this back in if we figure out solution for PC
          'readxl',
          'rmarkdown')

for (l in libs) {
  if(!is.element(l, .packages(all.available = TRUE)) ) {
    install.packages(l)
  }
  suppressPackageStartupMessages(library(l, character.only=TRUE))
}

# Reformat start date and end date as dates
start_date <- as.Date(start_date, "%m/%d/%Y")
end_date <- as.Date(end_date, "%m/%d/%Y")

get_file_list <- function(folder) {
  
  # Returns list of files in a folder
  
  # get list of files within folder
  files <- list.files(folder)
  temp <- paste0(folder, "/", files)
  
  return(temp)
  
}

get_file_extension <- function(folder) {
  
  # Returns the file extension of files in a folder. Assumes that all files have
  # the same extension, so do not use with folders that have multiple types of files.
  
  # get list of files within folder
  files <- get_file_list(folder)
  
  # find out what type of data files we have by getting the file (assumes all data files are same file type)
  data_type <- substr(files[1], as.numeric(regexpr("\\.([[:alnum:]]+)$", files[1])[1]), nchar(files[1]))
  
  # return file extension
  return(data_type)
  
}

read_data <- function(folder, ...) {
  
  # Returns dataframe of data. Optional arguments are columns to be reformatted as dates
  # (for use with csv and txt files). When reading Excel files, all columns featuring
  # POSIXct data are automatically reformatted as dates.
  
  # Make list of columns to be reformatted as dates
  date_reformat = list(...)
  
  # Get file list
  temp <- get_file_list(folder)
  
  # Get file extension
  ext <- get_file_extension(folder)
  
  # read in data using different methods depending on what type of data files we have (e.g., .xls vs. .txt)
  if (ext == ".xlsx" | ext == ".xls") {
    initial_dd <- do.call(rbind, lapply(temp, function(x) read_excel(x, sheet = 1)))
  } else {
    initial_dd <- do.call(rbind, lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=separator)))
  }
  
  # Reformat any POSIXct columns as dates
  initial_dd[] <- lapply(initial_dd, function(x) {
    if (inherits(x, "POSIXct")) as.Date(x) else x
    })
  
  # Reformat any specified columns as dates
  if (!is.null(date_reformat)) {
    for (i in date_reformat) {
      if (class(initial_dd[,eval(i)][[1]]) != "Date") {
        initial_dd[,i] <- as.Date(initial_dd[,i], "%m/%d/%Y", origin = "1904-01-01")
      }
    }
  }
  
  # Reformat SUBMITTERID as character
  if("SUBMITTERID" %in% names(initial_dd)) {
    initial_dd$SUBMITTERID <- as.character(initial_dd$SUBMITTERID)
  }
  
  df$species[df$depth<10]  <- "unknown" 
  
  # Replace 9999 values in transit time column with NA
  if("TRANSITTIME" %in% names(initial_dd)) {
    initial_dd$TRANSIT_TIME[initial_dd$TRANSIT_TIME == 9999] <- NA
  }
  
  
  # If dataframe has LINKID column, change this to PATIENTID - this will only be
  # an issue for the version of the data being used by UVA
  if("LINKID" %in% names(initial_dd)) {
    colnames(initial_dd)[which(names(initial_dd) == "LINKID")] <- "PATIENTID"
  }
  
  # If dataframe has CATEGORY column, remove any records that have category listed as "Proficiency", 
  # "Treatment", or "Treatment - PKU"
  remove_cats <- c("Proficiency","Treatment","Treatment - PKU")
  if (!is.null(initial_dd$CATEGORY)) {initial_dd <- filter(initial_dd, !(CATEGORY %in% remove_cats))}
  
  return(initial_dd)
  
}

create_filt_dfs <- function(df, type=c("sample","diagnosis"), s_date=start_date, e_date=end_date, period=line_chart) {
  
  # Given a dataframe, start date, and end date, returns 2 data frames filtered by
  # start date and end date:
  #   1) period_df - filtered by period of interest (using start_date and end_date)
  #   2) year_df - filtered for one year prior to end_date (using end of period, either the month or 
  #      the quarter (depending on line_chart value)
  
  # Define column that will be used to filter data
  filt_col <- ifelse(type == "sample", "BIRTHDATE", "DIAGNOSISDATE")
  
  # Obtain first dataframe, filtering data by start_date and end_date
  period_df <- df %>%
    filter_(interp(~ as.Date(filt_col, format="%m/%d/%Y") >= s_date
                   & as.Date(filt_col, format="%m/%d/%Y") <= e_date,
                   filt_col=as.name(filt_col)))
  
  # Determine end date for period, depending on whether line_plot
  # is defined as "monthly" or "quarterly"
  period_end <- as.Date(ifelse(period == "quarterly", as.Date(as.yearqtr(e_date), frac=1), 
                               as.Date(as.yearmon(e_date), frac=1)))
  
  # Filter df to include one year of data (dated backwards from end date)
  year_df <- df %>%
    filter_(interp(~ as.Date(filt_col, format="%m/%d/%Y") > (period_end - years(1)) 
                   & as.Date(filt_col, format="%m/%d/%Y") <= period_end,
                   filt_col=as.name(filt_col)))
  
  # Add period information to year dataframe if type == "sample"
  if(type == "sample") {
    if(period == "quarterly") {
      year_df$PERIOD <- as.yearqtr(year_df[[filt_col]], format="%Y%m")
    } else {
      year_df$PERIOD  <- as.yearmon(year_df[[eval(filt_col)]], format="%Y%m")
    }
  }
    
  return(list(period_df, year_df))
  
}

stopQuietly <- function(...) {
  
  # Stops a source file quietly (without printing an error message), used in cases
  # where we have multiple files that need to stop running, but only have one of them
  # throw an error.
  
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
  
} 
