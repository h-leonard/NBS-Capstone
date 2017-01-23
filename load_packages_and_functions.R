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
          'toOrdinal',
          'mailR',
          'readxl')

for (l in libs) {
  if(!is.element(l, .packages(all.available = TRUE)) ) {
    install.packages(l)
  }
  suppressPackageStartupMessages(library(l, character.only=TRUE))
}

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
  
  return(initial_dd)
  
}

stopQuietly <- function(...) {
  
  # Stops a source file quietly (without printing an error message), used in cases
  # where we have multiple files that need to stop running, but only have one of them
  # throw an error.
  
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
  
} 

# Reformat start date and end date as dates
start_date <- as.Date(start_date, "%m/%d/%Y")
end_date <- as.Date(end_date, "%m/%d/%Y")
