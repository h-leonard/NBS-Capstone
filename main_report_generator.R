# DO NOT RUN THIS FILE DIRECTLY, AS THE VARIABLES USED FOR
# FILE LOCATION AND NAMES ARE SET IN "run_file_and_variable_setting.R."
# IF YOU RUN THAT FILE, IT WILL ALLOW YOU TO SET YOUR FILE LOCATIONS
# AND ALSO RUN THIS FILE.

# Set value for filt_col based on whether the summary
# report is being run. Default is 'RECEIVEDATE'.
if (exists("summary_report") & exists("summary_filter")) {
  filt_col <- summary_filter
} else {
  filt_col <- 'RECEIVEDATE'
}

# source load_packages file
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)

# read in unsats csv file
unsats <- read.csv(paste(codes_path, slash, "unsat_codes.csv", sep=""))

# read in individual messages to hospitals to be included in report
messages <- read.csv(paste(codes_path, slash, "organization_messages.csv", sep=""), stringsAsFactors = FALSE)
messages <- messages[!is.na(messages$Message),]

# Check for correct columns
col_check(sample_data_path, "sample")

# read in sample data and reformat COLLECTIONDATE and BIRTHDATE as dates
initial_dd_prep <- read_data(sample_data_path, "COLLECTIONDATE", "RECEIVEDATE", "BIRTHDATE")

# Check that range of filt_col column dates overlaps the requested start and end date
date_comp_check(initial_dd_prep, "sample")

# join initial_dd with submitter file on submitter ID
initial_dd <- left_join(initial_dd_prep, submitters, by="SUBMITTERID")

# create dataframes from initial_dd, filtered on start date and end date
temp_sample_dfs <- create_filt_dfs(initial_dd, type="sample")
dd <- as.data.frame(temp_sample_dfs[1])
year_dd <- as.data.frame(temp_sample_dfs[2])

# Check to ensure that there is a full year of data (so that the plots
# on the report cards will be fully realized)
if (!exists("summary_report")) {
  
  # Set year_check to be TRUE
  year_check <- TRUE
  
  # Perform date test
  date_comp_check(year_dd, "sample")
  
  # Remove year_check
  rm(year_check)
}

# output report of all submitters not in VA NBS Report Card Organization Names, 
# with a count of samples, for determining if any submitters need to be
# added to this file
not_in_VA_NBS_hosp <- year_dd %>%
  filter(is.na(HOSPITALREPORT)) %>%
  group_by(SUBMITTERID, SUBMITTERNAME) %>%
  select(SUBMITTERID, SUBMITTERNAME) %>%
  dplyr::summarise(TOTAL=n()) %>%
  arrange(desc(TOTAL))

# write file to admin reports
write.csv(not_in_VA_NBS_hosp, paste0(admin_path, slash, "submitters_not_in_VA_NBS_organization_file.csv"))

# set cutoff value for transit time (4 hours, or 1/6 of day)
cutoff <- 1/6

# replace submitter name with hospitalreport field for dd and year_dd
dd$SUBMITTERNAME <- dd$HOSPITALREPORT
year_dd$SUBMITTERNAME <- year_dd$HOSPITALREPORT

################

# If report_type is "H" (hospital) OR if the summary_report variable exists (indicating we are running
# a summary report), run the hospital code below

if (exists("summary_report") | report_type == "H") {
  
  dd_h <- filter(dd, TYPE == "Hospital")
  year_dd_h <- filter(year_dd, TYPE == "Hospital")
  
  # Continue if number of rows in dd_h is greater than 0
  if (nrow(dd_h) > 0) {
  
    # create data frame of required metrics for each submitter
    org_metrics_hosp <- get_org_metrics(dd_h)
    org_metrics_h <- as.data.frame(org_metrics_hosp[1], check.names=FALSE)
    
    # get state metrics, AVERAGED OVER HOSPTIALS (rather than samples)
      
    # determine number of hospitals
    tot_sub_h <-  nrow(org_metrics_h)
      
    # create metrics for state
    state_h <- get_state_metrics_over_orgs(org_metrics_h, transfused=TRUE)
    
  # Stop report if there is no data for hospitals and the user is trying to 
  # generate report cards for hospitals
  } else if (!exists("summary_report")) {
    
    cat("\nERROR: There is no sample data for hospitals for the requested\ntime period, so report cards cannot be run.\n")
    stopQuietly()
  
  }
  
}

# If report_type is "BC" (birth center) OR if the summary_report variable exists (indicating we are 
# running a summary report), run the birth center code below

if (exists("summary_report") | report_type == "BC") {
  
  # Subset data
  dd_bc <- filter(dd, TYPE == "BirthCenter")
  year_dd_bc <- filter(year_dd, TYPE == "BirthCenter")
  
  # Continue if number of rows in dd_bc is greater than 0
  if (nrow(dd_bc) > 0) {
  
    # create data frame of required metrics for each submitter
    org_metrics_birthcenter <- get_org_metrics(dd_bc)
    org_metrics_bc <- as.data.frame(org_metrics_birthcenter[1], check.names=FALSE)
  
    # Create state metrics based on ALL SAMPLES
    state_bc <- get_state_metrics_over_samples(dd, transfused=TRUE)
    
    # Get summary statistics for state for each period for use in birthcenter plots across ALL SAMPLES
    state_plot_bc <- year_dd %>%
      group_by(PERIOD) %>%
      dplyr::summarise(
        total_samples = n(),   
        percent_rec_in_2_days = round(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
                                        sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100, 2),
        unsat_count = sum(!is.na(UNSATCODE)),
        unsat_percent = round(unsat_count/total_samples * 100, 2)
      )
    state_plot_bc$SUBMITTERNAME <- 'State'
  
  # Stop report if there is no data for BirthCenters and the user is trying to 
  # generate report cards for BirthCenters
  } else if (!exists("summary_report")) {
    
    cat("\nERROR: There is no sample data for birthcenters for the requested\ntime period, so report cards cannot be run.\n")
    stopQuietly()
    
  }

}

# Stop reporting if user is running summary reports but no
# data exists for either hospitals or birthcenters
if (exists("summary_report")) {
  
  if (nrow(dd_h) == 0 & nrow(dd_bc) == 0) {
  
  cat("\nERROR: There is no sample data for birthcenters or hospitals for\nthe requested time period, so summary reports cannot be created.\n")
  stopQuietly()
  
  }
  
}

#######################################################

# Run reports if not sourcing this file from summary_report_generator (which
# sets the value of summary_report)
if (!exists("summary_report")) {
  
  ##### Set variables depending on whether user is running hospital or birthcenter report #####
  # Set values for output and markdown file depending on what user has chosen
  if (report_type == "H") {
    
    output <- hospital_path
    markdown_file <- ifelse(min_max == 'Y', "main_report_markdown_WITH_MINMAX.Rmd", 
                            "main_report_markdown.Rmd")
    year_data <- year_dd_h
    org_metrics <- org_metrics_h
    state <- state_h
    
  } else if (report_type == "BC") {
    
    output <- center_path
    markdown_file <- ifelse(min_max == 'Y', "main_report_markdown_birthcenter_WITH_MINMAX.Rmd", 
                            "main_report_markdown_birthcenter.Rmd")
    year_data <- year_dd_bc
    org_metrics <- org_metrics_bc
    state <- state_bc
    
  }
  
  # Group by submitter and period
  org_metrics_plot <- year_data %>%
    group_by(SUBMITTERNAME, PERIOD) %>%
    select(SUBMITTERNAME, TRANSIT_TIME, UNSATCODE, PERIOD) %>%
    dplyr::summarise(
      total_samples=n(),   
      percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, 
                                  na.rm=TRUE)/sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
      unsat_count = sum(!is.na(UNSATCODE)),
      unsat_percent = unsat_count/total_samples * 100
    )
  
  # Determine limits for y-axes by finding min for transit percent
  # and max overall unsat. For min_trainst_percent use the 2% percentile,
  # for max overall unsat use the 98% percentile.
  transit_percent_col <- grep("percent_rec_in_2_days", colnames(org_metrics_plot))
  unsat_percent_col <- grep("unsat_percent", colnames(org_metrics_plot))
  min_transit_percent <- quantile(org_metrics_plot$percent_rec_in_2_days, 0.02, na.rm=TRUE)
  max_overall_unsat <- quantile(org_metrics_plot$unsat_percent, .98, na.rm=TRUE)
  
  # Group by period for state totals
  if (report_type == "H") {
    state_plot_h <- org_metrics_plot %>%
      group_by(PERIOD) %>%
      dplyr::summarise(
        total_samples = sum(total_samples, na.rm=TRUE),   
        percent_rec_in_2_days = mean(percent_rec_in_2_days, na.rm=TRUE),
        unsat_count = sum(unsat_count, na.rm=TRUE),
        unsat_percent = mean(unsat_percent, na.rm=TRUE)
      )
    
    state_plot_h$SUBMITTERNAME <- 'State'
  }
  
  # Set variable for state_plot depending on which report is being run
  if (report_type == "H") {
    
    state_plot <- state_plot_h
    
  } else if (report_type == "BC") {
    
    state_plot <- state_plot_bc
    
  }
  
  ## ADDRESS ORGANIZATIONS WITH NO DATA FOR ANY PARTICULAR PERIOD IN THE DATE RANGE OF INTEREST
  
  # Bind state_plot to year_data
  temp_for_cj <- rbind(state_plot[, c("PERIOD", "SUBMITTERNAME")], year_data[, c("PERIOD", "SUBMITTERNAME")])
  
  # Create cross join of all possible periods and all submitter names
  cross_join_periods <- CJ(SUBMITTERNAME=unique(temp_for_cj$SUBMITTERNAME), PERIOD=unique(temp_for_cj$PERIOD))
  
  # Remove state from cross_join_periods
  cross_join_periods <- filter(cross_join_periods, SUBMITTERNAME != "State")
  
  # Left join org_metrics_plot with cross_join_quarts so that we have NAs for organizations
  # with no data for a particular period
  suppressWarnings(org_metrics_plot <- full_join(org_metrics_plot, cross_join_periods, 
                                                 by=c("SUBMITTERNAME", "PERIOD")))
  
  # Change org metrics to include only a single submitter (if we are only testing the functionality 
  # rather than running all reports)
  if (test_report == "Y") org_metrics = org_metrics[1,]

  # Change org metrics to include only the submitters indicated if only_run is not NULL
  if (!is.null(only_run)) org_metrics = filter(org_metrics, SUBMITTERNAME %in% only_run)

  # Generate report for each organization
  render_file <- paste0(wd, slash, markdown_file)

  for (submitter in org_metrics$SUBMITTERNAME){
    suppressWarnings(rmarkdown::render(input = render_file, 
                      output_format = "pdf_document",
                      output_file = paste(submitter, "_", start_date, "_", 
                                          end_date, ".pdf", sep=''),
                      output_dir = output))
  }
  
}
