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
          'readxl',
          'rmarkdown',
          'mailR')

for (l in libs) {
  if(!is.element(l, .packages(all.available = TRUE)) ) {
    install.packages(l)
  }
  suppressPackageStartupMessages(library(l, character.only=TRUE))
}

# Reformat start date and end date as dates
if (exists("start_date")) {
  start_date <- as.Date(start_date, "%m/%d/%Y")
  end_date <- as.Date(end_date, "%m/%d/%Y")
  
  if (is.na(start_date)) {
    message = "Your start date is invalid. Please enter a valid start date."
    cat(message)
    stopQuietly()
  }
  
  if (is.na(end_date)) {
    message = "Your end date is invalid. Please enter a valid end date."
    cat(message)
    stopQuietly()
  }
}

stopQuietly <- function(...) {
  
  # Stops a source file quietly (without printing an error message), used in cases
  # where we have multiple files that need to stop running, but only have one of them
  # throw an error.
  
  suppressWarnings(rm(summary_report, pos = ".GlobalEnv"))
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
  
} 

?rm

check_inf <- function(FUN) {
  
  # returns the value of the function if not infinite, otherwise returns NA
  suppressWarnings(return(ifelse(is.infinite(FUN), NA, FUN)))
  
}

get_file_list <- function(folder) {
  
  # Returns list of .txt or .csv files in a folder
  
  files <- list.files(folder, pattern = "csv|txt")
  temp <- paste0(folder, slash, files)
  
  return(temp)
  
}

substrRight <- function(x, n){
  
  # Returns a string of specified length counting backwards
  
  return(substr(x, nchar(x)-n+1, nchar(x)))
  
}

date_reformat <- function(df, ...) {
  
  # Reformats set of columns as dates in a dataframe, first checks to see how year is 
  # formatted
  
  date_cols = list(...)
  
  for (col in unlist(date_cols)) {
    
    # check to see if last three characters in string contain a "/"
    # (thus indicating the year is in 2-digit rather than 4-digit format)
    
    if (grepl("/", substrRight(df[col, 1], 3))) {
      df[, col] = as.Date(df[, col], "%m/%d/%y")
    } else {
      df[, col] = as.Date(df[, col], "%m/%d/%Y")
    }
  }
  
  return(df)
}

date_repair <- function(df, ...) {
  
  # Repairs dates that have been read incorrectly (e.g., if year is listed as '0016' 
  # instead of '2016'). Dates for checking should have already been formatted as dates.
  
  date_cols = list(...)
  
  # Pull all records where the dates do not appear to have been formatted correctly
  for (col in unlist(date_cols)) {
    
    rows = which(!is.na(df[, col]) & df[, col] < as.Date("1970-01-01")) 
    
    for (row in rows) {
      
      if (as.numeric(substr(df[row, col], 3, 4)) > as.numeric(format(Sys.Date(), "%y"))) {
        cent = "19"
      } else {
        cent = "20"
      }
      
      repaired = gsub("00", cent, df[row, col])
      df[row, col] = as.Date(repaired, format = "%Y-%m-%d")
      
    }
    
  }
  
  return(df)
  
}

col_check <- function(folder, type) {
  
  # Checks that a given list of files have the correct columns, returns
  # list of 'bad files' along with column names that are not found in the
  # files indicated.
  
  # Also checks to ensure that start date and end date requested by user
  # are within the dates covered by the data.
  
  # Get file list
  temp = get_file_list(folder)
  
  # Loop through files to ensure they all have the correct columns
  bad_files = c()
  
  if (type == "sample") {
    cols = c("SAMPLEID", "LINKID", "BIRTHDATE", "BIRTHTIME", "COLLECTIONDATE", 
             "COLLECTIONTIME", "RECEIVEDATE", "TRANSIT_TIME", "TRANSFUSED", "SUBMITTERID", 
             "SUBMITTERNAME", "UNSATCODE", "CATEGORY")
  } else if (type == "diagnosis") {
    cols = c("SAMPLEID", "DIAGNOSIS", "DIAGNOSISDATE", "SUBMITTERID", "LINKID")
  }
  
  for (f in temp) {
    # Read in single row from each file to get the column names
    temp_file = suppressWarnings(read.table(f, nrows = 1, header = TRUE, sep=separator, 
                                            fill = TRUE))
    # Strip odd characters from column names
    colnames(temp_file) <- gsub("ï..", "", colnames(temp_file))
    # Find set difference between expected columns and columns in temp_file
    bf_temp = setdiff(cols, colnames(temp_file))
    if (length(bf_temp) != 0) {
      bad_files = c(bad_files, paste0(f, ":\n   missing columns - ", paste(bf_temp, collapse = ", "), "\n"))
    } 
    
  }
  
  if (length(bad_files) != 0) {
    e_begin <- ifelse(length(bad_files) == 1, "One file", "Several files")
    e_verb <- ifelse(length(bad_files) == 1, "does", "do")
    e_art <- ifelse(length(bad_files) == 1, "this", "these")
    e_plur <- ifelse(length(bad_files) == 1, "", "s")
    message = paste0(e_begin, " in the ", type, "_data_path location ", e_verb, 
                     " not have the required column\nheadings. Please check the column headings for ",
                     e_art, " file", e_plur, " before running reports:\n", paste0(bad_files, collapse=""))
    
    cat(message)
    stopQuietly()
  }
  
}

get_dates <- function(df, data_type) {
  
  # Gets minimum and maximum date for the column data will be 
  # filtered by. Used as input for date_compare.
  
  date_col <- ifelse(data_type == "sample", filt_col, "DIAGNOSISDATE")
  
  # Add the minimum date in the date_col to the list of date minimums
  min_date = min(df[, date_col], na.rm = TRUE)
  
  # Add the maximum date in the date_col to the list of date maximums
  max_date = max(df[, date_col], na.rm = TRUE)
  
  return(list(min_date, max_date))
  
}

date_compare <- function(data_type, start_or_end, compare_obj) {
  
  # Checks data to see if start date or end date desired by user
  # is outside of bounds of data source
  
  # compare_obj is a list that contains minimum and maximum date
  # from the data source (created by date_check function)
  
  date_col <- ifelse(data_type == "sample", filt_col, "DIAGNOSISDATE")
  
  comp_s = as.Date(compare_obj[1][[1]]) # minimum date
  comp_e = as.Date(compare_obj[2][[1]]) # maximum date
  
  if (start_or_end == "start") {
    adj <- "earliest"
    adv <- "earlier"
    date <- start_date
    compare_date <- comp_s
  } else {
    adj <- "latest"
    adv <- "later"
    date <- end_date
    compare_date <- comp_e
  }
  
  # Stop report completely if requested start date is after the 
  # end date in the data (regardless of whether user has indicated 
  # a check for start or end)
  if (start_date > comp_e) {
    message <- paste0("\nYour desired start_date, ", start_date, ", is later than any dates for ", date_col, "\nin your ",
                      data_type, " data source (the latest of which is ", comp_e, 
                      ").\nBecause there is no overlap between your requested dates and the dates in\nyour data, the report generation will stop.")
    cat(message)
    stopQuietly()
  }
  
  # Stop report completely if requested end date is before the 
  # start date in the data (regardless of whether user has indicated 
  # a check for start or end)
  if (end_date < comp_s) {
    message <- paste0("\nYour desired end_date, ", end_date, ", is earlier than any dates for ", date_col, "\nin your ",
                      data_type, " data source (the earliest of which is ", comp_s, 
                      ").\nBecause there is no overlap between your requested dates and the dates in\nyour data, the report generation will stop.")
    cat(message)
    stopQuietly()
  }
  
  if ( (start_or_end == "start" & comp_s > start_date) || (start_or_end == "end" & comp_e < end_date) ) {
    message <- paste0("\nYour desired ", start_or_end, "_date, ", date, ", is ", adv, 
                      " than any dates for ", date_col, 
                      "\nin your ", data_type, " data source (the ", adj , " of which is ", as.Date(compare_date),
                      ").\n\nDo you wish to proceed? (enter 'Y' or 'y' for yes)\n")
    cat(message)                
    ans <- readline(prompt = "> ")
    if (tolower(ans) != 'y') {
      cat("\nStopping per your request.")
      stopQuietly()
    } else {
      message <- paste0("\nContinuing with report generation with your selection for ", start_or_end, "_date.\nThis will take a few moments.\n")
      cat(message)
    }
  }
  
}

date_comp_check <- function(df, data_type) {
  
  # Compares start date and end date entered by user with
  # minimum and maximum dates of filter column.
  
  # Get minimum and maximum dates for filter column in df
  date_test <- get_dates(df, data_type)
  
  # Check that earliest date for filt_col in data is earlier than requested start_date
  date_compare(data_type, "start", date_test)
  
  # Check that latest date for filt_col in data is later than requested end_date
  date_compare(data_type, "end", date_test)
  
  # Perform check on year of data if year_check exists (defined in main_report_generator)
  if (exists("year_check")) {
    
    comp_s <- as.Date(date_test[1][[1]]) 
    
    # Get date one year prior to end date
    d_check <- end_date - years(1)
    
    # Check to see if d_check is earlier than dates in data
    if(d_check < comp_s) {
      message <- paste0("\nThe earliest date in your sample data is ", comp_s, ", so the plots\nin your report cards will not show a full year of performance. \n\nDo you wish to proceed? (enter 'Y' or 'y' for yes)\n")
      cat(message)                
      ans <- readline(prompt = "> ")
      if (tolower(ans) != 'y') {
        cat("\nStopping per your request.")
        stopQuietly()
      } else {
        message <- paste0("\nContinuing with report generation.\nThis will take a few moments.\n")
        cat(message)
      }
      
    }
    
  }
  
}

read_data <- function(folder, ...) {
  
  # Returns dataframe of data. Optional arguments are columns to be reformatted as dates
  # (for use with csv and txt files).
  
  # Make list of columns to be reformatted as dates
  date_cols = list(...)
  
  # Get file list
  temp <- get_file_list(folder)
  
  # read in data using different methods depending on what type of data files we have (e.g., .csv vs. .txt)
  lst <- lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=separator, fileEncoding="latin1"))
  initial_dd <- rbindlist(lst, fill=TRUE)
  
  # read in data second time in order to get character vector of SUBMITTERID
  sub_lst <- lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=separator, fileEncoding="latin1",
                                               colClasses="character"))
  
  sub_dd <- rbindlist(sub_lst, fill=TRUE)
  
  # replace initial_dd SAMPLEID with sub_dd version (to keep leading zeros)
  if ("SAMPLEID" %in% names(initial_dd)) {
    initial_dd$SAMPLEID <- as.character(sub_dd$SAMPLEID)
  }
  
  # change initial_dd to a dataframe
  initial_dd <- data.frame(initial_dd)
  
  # Strip odd characters from column names
  colnames(initial_dd) <- gsub("ï..", "", colnames(initial_dd))
  
  # Reformat and repair any specified columns as dates
  if (length(date_cols) > 0) {
    initial_dd <- date_reformat(initial_dd, date_cols)
    initial_dd <- date_repair(initial_dd, date_cols)
  }
  
  # Reformat SUBMITTERID as character
  if("SUBMITTERID" %in% names(initial_dd)) {
    initial_dd$SUBMITTERID <- as.character(initial_dd$SUBMITTERID)
  }
  
  # Replace 9999 values in transit time column with NA
  if("TRANSITTIME" %in% names(initial_dd)) {
    initial_dd$TRANSIT_TIME[initial_dd$TRANSIT_TIME == 9999] <- NA
  }
  
  # If dataframe has CATEGORY column, remove any records that have category listed as "Proficiency", 
  # "Treatment", or "Treatment - PKU"
  remove_cats <- c("Proficiency","Treatment","Treatment - PKU")
  if (!is.null(initial_dd$CATEGORY)) {initial_dd <- initial_dd[!(initial_dd$CATEGORY %in% remove_cats),]}
  
  # Remove any duplicate rows
  initial_dd <- unique(initial_dd)
  
  return(initial_dd)
  
}

create_filt_dfs <- function(df, type=c("sample","diagnosis"), s_date=start_date, e_date=end_date, period=line_chart) {
  
  # Given a dataframe, start date, and end date, returns 2 data frames filtered by
  # start date and end date:
  #   1) period_df - filtered by period of interest (using start_date and end_date)
  #   2) year_df - filtered for one year prior to end_date (using end of period, either the month or 
  #      the quarter (depending on line_chart value)
  
  # Define column that will be used to filter data
  filt_col <- ifelse(type == "sample", filt_col, "DIAGNOSISDATE")
  
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
      year_df$PERIOD  <- as.yearmon(year_df[[filt_col]], format="%Y%m")
    }
  }
  
  return(list(period_df, year_df))
  
}

get_org_metrics <- function(df, group_by="SUBMITTERNAME") {
  
  # Returns dataframe of metrics for submitters for use in report cards (includes
  # rank and unsat counts). Default group_by column is SUBMITTERNAME but user can
  # select a different column
  
  cols = c(group_by, "TRANSIT_TIME", "COLLECTIONDATE", "COLLECTIONTIME", "BIRTHDATE",
           "BIRTHTIME","UNSATCODE","TRANSFUSED")
  
  temp_df = df %>%
    group_by_(group_by) %>%
    select(one_of(cols)) %>%
    dplyr::summarise(
      total_samples=n(),
      avg_transit_time = check_inf(round(mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2)),
      min_transit_time = check_inf(min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE)),
      max_transit_time = check_inf(max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE)),
      rec_in_2_days = check_inf(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)),
      percent_rec_in_2_days = check_inf(round(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
                                                sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100, 2)),
      met_goal = ifelse(percent_rec_in_2_days >= 95, 1, 0),
      col_less_than_24_hours = check_inf(sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                               COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                             na.rm=TRUE)),
      percent_less_than_24_hours = check_inf(round(col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100, 2)),
      trans = check_inf(sum(TRANSFUSED == 'Y', na.rm=TRUE)),
      trans_percent = check_inf(round(trans/total_samples * 100, 2)),
      unsat_count = check_inf(sum(!is.na(UNSATCODE))),
      unsat_percent = check_inf(round(unsat_count/total_samples * 100, 2)))
  
  ##### ADD RANKINGS #####
  
  # Rank submitters by mean transit time (ascending order; e.g., least mean transit time = #1)
  temp_df$rank_transit = rank(temp_df$avg_transit_time, na.last="keep", ties.method="min")
  
  # Rank submitters by percentage of samples recevied within 2 days (descending order; e.g.,
  # greatest percentage of samples received by target time = #1)
  temp_df$rank_percent_within_goal = rank(-temp_df$percent_rec_in_2_days, na.last="keep", ties.method="min")
  
  # Rank submitters  by number of samples collected at less than 24 hours of age (ascending order;
  # e.g., least number of early collections = #1)
  temp_df$rank_early_collection = rank(temp_df$percent_less_than_24_hours, na.last="keep", ties.method="min")
  
  # Rank submitters by number of samples transfused prior to collection (ascending order;
  # e.g., least number of early collections = #1)
  temp_df$rank_transfused = rank(temp_df$trans_percent, na.last="keep", ties.method="min")
  
  # Rank ubmitters by number of unsatisfactory samples (ascending order; e.g., least number of unsats = #1)
  temp_df$rank_unsats = rank(temp_df$unsat_percent, na.last="keep", ties.method="min")
  
  ##### ADD UNSAT COUNTS #####
  
  # get count of unsat codes for each submitter
  unsat_prep = df[!is.na(df$UNSATCODE),] %>% 
    group_by_(group_by, "UNSATCODE") %>%
    dplyr::summarise(count = n())  
  colnames(unsat_prep) = c("ORG", "UNSATCODE", "count")
  
  # get all possibilities for unsat codes
  unsat_seq = seq(1:nrow(unsats))
  
  # create cross join of all possible unsat codes and all submitter names
  cross_join = CJ(ORG=unique(temp_df[[group_by]]), UNSATCODE=unsat_seq)
  
  # create left join of unsat counts and cross_join (so we have NAs
  # for each submitter that has no unsats for that particular code)
  unsat_amts = left_join(cross_join, unsat_prep, by=c("ORG", "UNSATCODE"))
  
  # replace UNSATCODE column with 'col' column
  unsat_amts$col = paste("unsat_", str_pad(unsat_amts$UNSATCODE, 2, pad="0"), sep="")
  unsat_amts$UNSATCODE = NULL
  
  # reshape dataframe to have rows as SUBMITTERNAME and columns as col (e.g., unsat_01, unsat_02, etc.)
  unsats_ready = dcast(unsat_amts, ORG ~ col, value.var="count")
  
  # replace column names (unsat_01, etc.) with unsat descriptions
  names(unsats_ready) = c(group_by, unlist(as.list(as.character(unsats$description), sorted = FALSE)))
  
  # left join unsats_ready and temp_df
  temp_df <- left_join(temp_df, unsats_ready, by=group_by)
  
  return(list(temp_df, unsats_ready))
  
}

get_diagnoses <- function(df, rpt) {
  
  # Identifies diagnoses for a set of organizations (either hospital or birth center)
  
  temp_submitters = dplyr::filter(submitters, TYPE == rpt)
  
  # filter out diagnoses not associated with organizations of interest
  temp_diag = df[df$SUBMITTERID %in% temp_submitters$SUBMITTERID,]
  
  # add submitter name to each record
  temp_diag = left_join(temp_diag, submitters, by="SUBMITTERID")
  colnames(temp_diag)[which(names(temp_diag) == "HOSPITALREPORT")] <- "SUBMITTERNAME"
  
  # Select columns from temp_diag for report, get count of each diagnosis
  # NOTE: if a diagnosis is associated with more than one organization (e.g., 
  # because the infant was born at a particular hospital, transferred
  # to another hospital, and both submitted samples that ended up being
  # associated with a diagnosis), ALL organizations in the dataframe submitting samples 
  # associated with a diagnosis will be 'credited' with this on their reports. For this
  # reason, we will NOT want to use a sum of the diagnoses in this dataframe
  # to count the total number of diagnoses, as this method will count some
  # diagnoses more than once.
  diagnoses_temp <- temp_diag %>%
    select(DISORDER, SUBMITTERNAME, LINKID, Narrative) %>%
    group_by(SUBMITTERNAME, DISORDER, LINKID, Narrative) %>%
    dplyr::summarise()
  
  # Get count of separate diagnoses associated with samples for each organization
  diagnoses <- diagnoses_temp %>%
    group_by(SUBMITTERNAME, DISORDER, Narrative) %>%
    dplyr::summarise(Count=n())
  
  # Rearrange columns
  diagnoses <- diagnoses[,c("SUBMITTERNAME","DISORDER","Count","Narrative")]
  
  return(diagnoses)
  
}

get_state_metrics_over_samples <- function(df, transfused) {
  
  # Get state metrics averaged over samples. Takes dataframe and transfused = TRUE or FALSE to indicate
  # whether transfusion information should be included
  
  temp_df = df %>%
    select(TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, TRANSFUSED, UNSATCODE) %>%
    dplyr::summarise(
      total_samples=n(),
      avg_transit_time = round(mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2),
      min_transit_time = min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
      max_transit_time = max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
      rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE),
      percent_rec_in_2_days = round(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
                                      sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100, 2),
      col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                     COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                   na.rm=TRUE),
      percent_less_than_24_hours = round(col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100, 2),
      trans = sum(TRANSFUSED == 'Y'),
      trans_percent = round(trans/total_samples * 100, 2),
      unsat_count = sum(!is.na(UNSATCODE)),
      unsat_percent = round(unsat_count/total_samples * 100, 2)
    )
  
  if (transfused == FALSE) {
    
    temp_df = select(temp_df, -c(trans, trans_percent))
    
  }
  
  return(temp_df)
  
}

get_state_metrics_over_orgs <- function(df, transfused) {
  
  # Get state metrics averaged over organizations (e.g., hospital or birthcenter). 
  # Takes dataframe of summarized data for each organization and transfused = 
  # TRUE or FALSE to indicate whether transfusion information should be included
  
  temp_df = df %>%
    dplyr::summarise(
      submitters = nrow(df),
      total_samples=sum(total_samples, na.rm=TRUE),
      avg_transit_time = round(mean(avg_transit_time, na.rm=TRUE), 2),
      min_transit_time = min(min_transit_time, na.rm=TRUE),
      max_transit_time = max(max_transit_time, na.rm=TRUE),
      rec_in_2_days = sum(rec_in_2_days, na.rm=TRUE),
      percent_rec_in_2_days = round(mean(percent_rec_in_2_days, na.rm=TRUE), 2),
      met_goal = sum(met_goal, na.rm=TRUE),
      percent_met_goal = round((met_goal / tot_sub_h) * 100, 2),
      col_less_than_24_hours = sum(col_less_than_24_hours, na.rm=TRUE),
      percent_less_than_24_hours = round(mean(percent_less_than_24_hours, na.rm=TRUE), 2),
      trans = sum(trans, na.rm=TRUE),
      trans_percent = round(mean(trans_percent, na.rm=TRUE), 2),
      unsat_count = sum(unsat_count, na.rm=TRUE),
      unsat_percent = round(mean(unsat_percent, na.rm=TRUE), 2)
    )
  
  if (transfused == FALSE) {
    
    temp_df = select(temp_df, -c(trans, trans_percent))
    
  }
  
  return(temp_df)
  
}

requestEmail <- function() {
  
  # Requests an email address and password for sending emails. If
  # email address is NULL, will replace with "donotreply@dgs.virginia.gov"
  
  cat("Enter the email address from which to send the hospital reports (note\nthat this must be a dgs.virginia.gov account). If you wish to use the\ndefault email address of 'donotreply@dgs.virginia.gov', press enter instead:")
  address <- readline("> ")
  
  if(address == "") {
    
    cat("Using 'donotreply@dgs.virginia.gov' to send reports.")
    return("donotreply@dgs.virginia.gov")
    
  } else {
    
    cat("Enter your email password. If no password is required, press enter instead.")
    password <- readline("> ")
    
    if(password == "") {
      
      return(address)
      
    } else {
      
      cat("Your password information will be removed from the R environment\nafter emails are sent.")
      return(list(address, password))
      
    }
  }
  
}

# Read in submitter names as we wish them to appear in the report
temp <- paste(codes_path, slash, "VA NBS Report Card Organization Names.csv", sep="")
submitters <- as.data.frame(read.csv(temp, sep=","))
names(submitters) <- c("SUBMITTERID","HOSPITALREPORT","TYPE")
submitters$SUBMITTERID <- as.character(submitters$SUBMITTERID)

# Stop report if all values in TYPE are not "Hospital" or "BirthCenter"
if (!all(levels(submitters$TYPE) %in% c("Hospital", "BirthCenter"))) {
  
  {stop(sprintf("The 'VA NBS Report Card Organization Names' csv file has '%s' listed in the 'Type' column. Please only use either 'Hospital' or 'BirthCenter' in this column.", 
                levels(submitters$TYPE)[which(!(levels(submitters$TYPE) %in% c("Hospital", "BirthCenter")))])) }
}

# Stop report if user has identified BC as the type of report, has requested
# that report cards or diagnoses be run, and has not identified any birthcenters in the
# VA NBS file
if (!exists("summary_report") & report_type == "BC" & !"BirthCenter" %in% submitters$TYPE) {
  {stop(sprintf("You have requested that reports for birth centers be generated, but there\nare no birth centers identified in the 'VA NBS Report Card Organization Names'\ncsv file. Please enter one or more birth centers in this file if you wish to\ncreate reports for these organizations."))}
}

# Test for IDs assigned to multiple organizations in submitters
ID_test <- submitters[(duplicated(submitters$SUBMITTERID) | duplicated(submitters$SUBMITTERID, 
                                                                       fromLast=TRUE)),]

# Stop report if duplicate IDs are discovered
if (nrow(ID_test) != 0){
  e_begin <- ifelse(length(unique(ID_test$SUBMITTERID)) == 1, "One ID", "Several IDs")
  e_verb <- ifelse(length(unique(ID_test$SUBMITTERID)) == 1, "is", "are")
  e_messages <- ""
  for (id in unique(ID_test$SUBMITTERID)) {
    temp_hosps <- ID_test$HOSPITALREPORT[ID_test$SUBMITTERID == id]
    test_hs <- paste0("\nHOSPITALS:     ", paste(temp_hosps, collapse=", "), "\n")
    test_ids <- paste0("DUPLICATED ID: ", id, "\n")
    e_messages <- paste0(e_messages, paste0(test_hs, test_ids))
  }
  {stop(sprintf("%s in 'VA NBS Report Card Organization Names' %s assigned to more than one hospital:\n%s\nPlease correct in 'VA NBS Report Card Organization Names' before running reports.", 
                e_begin, e_verb, e_messages)) }
}
