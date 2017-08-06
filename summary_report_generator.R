# Set a variable to allow us to source main_report_generator.R and diagnosis_report_generator.R
# witout running those reports
summary_report <- TRUE

# If user has entered something other than "RECEIVEDATE" or "BIRTHDATE" for 
# summary_filter, stop the file and report an error
if (summary_filter != "RECEIVEDATE" & summary_filter != "BIRTHDATE") {
  stop(sprintf("\nYou entered '%s' for the summary_filter variable. Please change the value for this variable to either 'RECEIVEDATE' or 'BIRTHDATE' to filter the sample data.",
               summary_filter))
}

# If user has entered 'BIRTHDATE' for summary_filter, ensure that 
# the user wishes to proceed with this entry (since the output will 
# NOT match the report card data).
if (summary_filter == "BIRTHDATE") {
  cat("\nYou entered 'BIRTHDATE' for the summary_filter variable. This will filter\nthe sample data by BIRTHDATE, so the resulting summaries will NOT match\nthe report cards. Do you wish to proceed? (enter 'Y' or 'y' for yes)")
  ans <- readline(prompt = "> ")
  if (tolower(ans) != 'y') {
    cat("\nStopping per your request.")
    stopQuietly()
  } else {
    cat("\nContinuing with the report creation using 'BIRTHDATE' as the filter.\nThis will take a few moments.\n")
  }
}

# Source main_report_generator.R and diagnosis_report_generator.R
report_run <- paste0(wd, slash, "main_report_generator.R")
source(report_run)

diag_run <- paste0(wd, slash, "diagnosis_report_generator.R")
source(diag_run)

##### PREPARE SUMMARY REPORTS - HOSPITAL AND BIRTHCENTER #####

create_summary_report <- function(org_df, diagnosis_df, type) {
  
  # Writes summary for organization of interest to csv (used for hospital and birthcenter summaries)
  
  # org_df = dataframe of sample information for organizations of interest
  # diagnosis_df = dataframe of diagnoses for organizations of interest
  # type = either "hospital" or "birthcenter"
  
  # Stop if org_df does not exist
  if (!exists(deparse(substitute(org_df)))) {
    
    cat(sprintf("\nWARNING: A summary report for %ss cannot be created, as there are\nno samples for %ss for the time period of interest. In addition,\nthe state summary report will be run without metrics for %ss.\n", 
                type, type, type))
    
  } else {
  
    # create vector for IDs for organizations of interest
    IDs = c()
    
    # get all submitter IDs for each organization of interest
    for (i in 1:nrow(org_df)) {
      IDs[i] <- paste(submitters[submitters$HOSPITALREPORT 
                                 %in% org_df$SUBMITTERNAME[i],]$SUBMITTERID, collapse="; ")
    }

    # reorganize columns for report
    org_summary = org_df[,c(1:3,15,4:7,16,8:10,17,11:12,18,13:14,19:32)]
    
    # add submitter IDs
    org_summary = cbind(as.data.frame(IDs), org_summary)
    
    # determine number of diagnoses for each organization
    
    # if the number of diagnoses is greater than 0, join diagnoses on organizations;
    # otherwise, create a vector of 0 values for diag_count
    if (nrow(diagnosis_df) > 0) {
      diag_count = diagnosis_df %>%
        group_by(SUBMITTERNAME) %>%
        dplyr::summarise(
          total=sum(Count))
    
      # add diagnosis count to hosp_summary
      org_summary = left_join(org_summary, diag_count, by="SUBMITTERNAME")
      
    } else {
      org_summary$total <- 0
    }
    
    # Rename columns
    org_summary_cols = c("Submitter IDs", "Submitter Name", "Sample Count", "Avg. Transit Time", "Rank: Transit Time",
                          "Min. Transit Time", "Max. Transit Time", "Received within 2 Days", 
                          "% Received within 2 Days","Rank: Received within 2 Days",
                          "Met 95% of Samples Received within 2 Days Goal?", "< 24 Hours", 
                          "% < 24 Hours", "Rank: < 24 Hours", "Transfused", "% Transfused", 
                          "Rank: Transfused", "Unsat Count", "Unsat %", "Rank: Unsats", 
                          paste("Unsat:", unlist(as.list(as.character(unsats$description), sorted = FALSE))),
                          "Diagnosis Count")
    names(org_summary) = org_summary_cols
    
    # Replace NAs with 0s
    org_summary[is.na(org_summary)] = 0
    
    # Change 0s to 'NO' and 1s to 'YES' for met goal
    org_summary$`Met 95% of Samples Received within 2 Days Goal?` <- 
      ifelse(org_summary$`Met 95% of Samples Received within 2 Days Goal?` == 0, 'no', 'yes')
    
    # Remove columns relating to transfusion if type === "birthcenter"
    if (type == "birthcenter") {
      org_summary <- org_summary[, -c(15:17)]
    }
    
    # Write to csv
    write.csv(org_summary, paste0(admin_path, slash, type, "_summary_by_", filt_col, ".csv"))
    
  }
  
}


# Write hospital summary report to CSV
create_summary_report(org_metrics_h, diagnoses_h, "hospital")

# Write birthcenter summary report to CSV
create_summary_report(org_metrics_bc, diagnoses_bc, "birthcenter")

##### PREPARE SUMMARY REPORT - DIAGNOSES #####

# Test to see if any diagnoses exist for the time period of interest
if (nrow(dd_diag_narr) == 0) {
  
  cat("\nWARNING: A summary report for diagnosis counts cannot be created, as\nthere are no diagnoses for the time period of interest.\n")
  
} else {

  # Get all unique disorder/patient combinations for period of interest
  diag_all <- dd_diag_narr %>%
    group_by(DISORDER, LINKID) %>%
    dplyr::summarise()
  
  # Remove all NAs
  diag_all <- diag_all[!is.na(diag_all$LINKID),]
  
  # Get counts of each disorder
  diag_all_count <- diag_all %>%
    dplyr::summarise(`Total Count`=n())
  
  # Join full list of disorders to count of disorders
  diag <- as.data.frame(diag_narr$Disorder, stringsAsFactors = FALSE)
  names(diag) <- "DISORDER"
  diag <- left_join(diag, diag_all_count, by="DISORDER")
  
  # replace NAs with 0s
  diag[is.na(diag)] <- 0
  
  # Publish diagnosis summary to admin folder
  write.csv(diag, paste0(admin_path, slash, "diagnosis_summary.csv"))
  
}

##########################################
##### PREPARE SUMMARY REPORT - STATE #####
##########################################

### Hospital Stats: columns 1 and 2 ###

# Check to see if there is any data for hospitals; if so, get 
# statistics for hospitals, and if not, create vectors of NAs

if (!exists("state_h")) {
  
  state_summary <- data.frame(matrix(rep(NA, 28), nrow=1, ncol=28))
  state_h_samp <- data.frame(matrix(rep(NA, 28), nrow=1, ncol=28))

} else {

  # Prep Column 1: HOSPITALS ONLY: Averaged over *hospitals*
  
  state_summary <- state_h
  
  # find counts of each unsat category for hospital submitters
  state_h_unsats <- as.data.frame(org_metrics_hosp[2], check.names=FALSE) %>%
    replace(is.na(.), 0) %>%
    summarise_at(vars(-SUBMITTERNAME), funs(sum))
  
  # add unsat counts to state summary
  state_summary <- cbind(state_summary, state_h_unsats)
  
  # Prep Column 2: HOSPITALS ONLY: Averaged over *samples*
  state_h_samp <- get_state_metrics_over_samples(dd_h, transfused=TRUE)
  
  # Add additional columns
  state_h_samp <- cbind(tot_sub_h, state_h_samp[1:6], state_summary[8:9], 
                        state_h_samp[7:12], state_h_unsats)

}

### BirthCenter Stats: columns 3 and 4 ###

# Check to see if there is any data for birthcenters; if so, get 
# statistics for birthcenters, and if not, create 2 rows of 28 NAs

if (!exists("state_bc")) {
  
  state_bc_org <- data.frame(matrix(rep(NA, 28), nrow=1, ncol=28))
  state_bc_samp <- data.frame(matrix(rep(NA, 28), nrow=1, ncol=28))
  
} else {

  # Prep Column 3: BIRTHCENTERS ONLY: Averaged over *birthcenters*
  state_bc_org <- get_state_metrics_over_orgs(org_metrics_bc, transfused=FALSE)
  
  # find counts of each unsat category for birthcenter submitters
  state_bc_unsats <- as.data.frame(org_metrics_birthcenter[2], check.names=FALSE) %>%
    replace(is.na(.), 0) %>%
    summarise_at(vars(-SUBMITTERNAME), funs(sum))
  
  # Add additional columns and unsats to state_bc_org
  state_bc_org <- cbind(state_bc_org[1:11], NA, NA, state_bc_org[12:13], state_bc_unsats)
  
  # Prep Column 4: BIRTHCENTERS ONLY: Averaged over *samples*
  state_bc_samp <- get_state_metrics_over_samples(dd_bc, transfused=FALSE)
  
  # Add additional columns
  state_bc_samp <- cbind(state_bc_org[1], state_bc_samp[1:6], state_bc_org[8:9], state_bc_samp[7:8], NA, NA, 
                         state_bc_samp[9:10], state_bc_unsats)

}

### Stats for All Submitters: columns 5 and 6 ###
  
# Prep Column 5: ALL SUBMITTERS: Averaged over *submitters*

# remove from dd any records missing SUBMITTERID
dd_filt <- dd %>%
  filter(SUBMITTERID != "" | !is.na(SUBMITTERID))

# get count of submitters, including hospital, birthcenter and organizations
# that do not fit into either category (count all the unique IDs in data, subtract 
# the number of IDs that appear in both the submitters CSV and the data - since
# some organizations have multiple IDs - then add back in the number of organizations 
# whose names appear in both submitters and dd_filt)
all_sub <- length(unique(dd_filt$SUBMITTERID)) - 
  length(intersect(dd_filt$SUBMITTERID, submitters$SUBMITTERID)) + 
  length(intersect(submitters$HOSPITALREPORT, dd_filt$SUBMITTERNAME))

# filter data for submitters not in VA NBS file
others <- dd_filt %>%
  filter(!(SUBMITTERID %in% submitters$SUBMITTERID))

# get metrics for others 
other_metrics <- get_org_metrics(others, group_by="SUBMITTERID")
all_sub_metrics <- as.data.frame(other_metrics[1], check.names=FALSE)

# Bind non-hospital submitter summaries with hospital and birthcenter summaries
# (if they exist)
all_sub_metrics$SUBMITTERNAME <- NA
all_sub_metrics <- all_sub_metrics[,c(33,2:32)]
all_sub_metrics <- rbind(all_sub_metrics, if(exists("org_metrics_h")) org_metrics_h, 
                         if(exists("org_metrics_bc")) org_metrics_bc)

# Get state averages across all submitters (hospital, birthcenter, and others)
state_all_sub <- get_state_metrics_over_orgs(all_sub_metrics, transfused=TRUE)

# Get counts of unsats by category for all submitters
unsat_all_counts <- data.frame(t(colSums(all_sub_metrics[, as.character(unsats$description)], na.rm=TRUE)), 
                               check.names=FALSE)

# Join state_all_sub to unsat_all_counts
state_all_sub <- cbind(state_all_sub, unsat_all_counts)

###### COLUMN 6 PREP: ALL SUBMITTERS: Averaged over *samples*

# Get averages for state over samples
state_all_samp <- get_state_metrics_over_samples(dd, transfused=TRUE)

# Add additional columns
state_all_samp <- cbind(state_all_sub[1], state_all_samp[1:6], state_all_sub[8:9], state_all_samp[7:12], 
                        unsat_all_counts)

###### BIND COLUMNS AND RENAME

# Create list of dfs for hospitals and birthcenters
dfs <- list(state_summary, state_h_samp, state_bc_org, state_bc_samp, state_all_sub, state_all_samp)

# Create vector of column names
state_cols <- c("Number of Submitters","Sample Count","Avg. Transit Time","Min. Transit Time",
                "Max. Transit Time","Received within 2 Days","% Received within 2 Days",
                "Number of Submitters Meeting 95% of Samples Received within 2 Days Goal",
                "% Submitters Meeting 95% of Samples within 2 Days Goal",
                "< 24 Hours","% < 24 Hours","Transfused", "% Transfused", 
                "Unsat Count", "Unsat %", 
                paste("Unsat:", unlist(as.list(as.character(unsats$description), sorted = FALSE))))

# Rename column names of dfs to state_cols
dfs <- lapply(dfs, setNames, state_cols)

# Bind all of the dataframes in the list
state_summary <- data.frame(do.call("rbind", dfs))

# Transform table and rename columns
state_summary <- as.data.frame(t(state_summary))
names(state_summary) <- c("HOSPITALS ONLY: Averaged over hospitals",
                          "HOSPITALS ONLY: Averaged over samples",
                          "BIRTH CENTERS ONLY: Averaged over birth centers",
                          "BIRTH CENTERS ONLY: Averaged over samples",
                          "ALL SUBMITTERS: Averaged over submitters",
                          "ALL SUBMITTERS: Averaged over samples")

# Rename rows
rownames(state_summary) <- state_cols

# Publish state summary to admin folder
write.csv(state_summary, paste0(admin_path, slash, "state_summary_by_", filt_col, ".csv"))

##### PREPARE OUTLIER TRANSIT TIME REPORT #####

time_outliers <- initial_dd %>%
  filter(TRANSIT_TIME > 10, eval(parse(text = filt_col)) >= start_date & 
           eval(parse(text = filt_col)) <= end_date) %>%
  arrange(desc(TRANSIT_TIME))

write.csv(time_outliers, paste0(admin_path, slash, "transit_time_outliers_by_", filt_col, ".csv"))

# Remove summary_report
rm(summary_report)

# Indicate that reports are complete
cat("\nAll summary reports that were able to be generated are now saved to your output folder.")
