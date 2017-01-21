# DO NOT RUN THIS FILE DIRECTLY, AS THE VARIABLES USED FOR
# FILE LOCATION AND NAMES ARE SET IN "run_file_and_variable_setting.R."
# IF YOU RUN THAT FILE, IT WILL ALLOW YOU TO SET YOUR FILE LOCATIONS
# AND ALSO RUN THIS FILE.

# read in unsats csv file
unsats <- read.csv(paste(codes_path, "/", "unsat_codes.csv", sep=""))

# read in submitter names as we wish them to appear in the report
temp <- paste(codes_path, "/", "VA NBS Report Card Hospital Names.csv", sep="")
submitters <- as.data.frame(read.csv(temp, sep=","))
names(submitters) <- c("SUBMITTERID","HOSPITALREPORT")
submitters$SUBMITTERID <- as.character(submitters$SUBMITTERID)

# read in sample data and reformat COLLECTIONDATE and BIRTHDATE as dates
initial_dd <- read_data(sample_data_path, "COLLECTIONDATE", "BIRTHDATE")

# remove any records that have category listed as "Proficiency", "Treatment", or "Treatment - PKU"
remove_cats <- c("Proficiency","Treatment","Treatment - PKU")
initial_dd <- filter(initial_dd, !(CATEGORY %in% remove_cats))

# add hospitalreport name to dd
dd <- left_join(initial_dd, submitters, by="SUBMITTERID")

# replace submitter name with hospitalreport field
dd$SUBMITTERNAME <- dd$HOSPITALREPORT

# remove records that have NA for submittername
dd <- dd[!is.na(dd$HOSPITALREPORT),]

# set cutoff value for transit time (4 hours, or 1/6 of day)
cutoff <- 1/6

##############################

# create data frame of required metrics for each submitter
hospital_metrics <- dd %>%
  group_by(SUBMITTERNAME) %>%
  filter(BIRTHDATE >= start_date & BIRTHDATE <= end_date) %>% 
  select(SUBMITTERNAME, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, TRANSFUSED) %>%
  summarise(
    total_samples=n(),
    avg_transit_time = mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    min_transit_time = min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE),
    percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
      sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
    met_goal = ifelse(percent_rec_in_2_days >= 95, 1, 0),
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                 na.rm=TRUE),
    percent_less_than_24_hours = col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100,
    trans = sum(TRANSFUSED == 'Y'),
    trans_percent = trans/total_samples * 100,
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples * 100
  )

##### ADD RANKINGS #####

# Rank hospitals by mean transit time (ascending order; e.g., least mean transit time = #1)
hospital_metrics$rank_transit <- rank(hospital_metrics$avg_transit_time, na.last="keep", ties.method="min")

# Rank hospitals by percentage of samples recevied within 2 days (descending order; e.g.,
#      greatest percentage of samples received by target time = #1)
hospital_metrics$rank_percent_within_goal <- rank(-hospital_metrics$percent_rec_in_2_days, na.last="keep", ties.method="min")

# Rank hospitals by number of samples collected at less than 24 hours of age (ascending order;
#     e.g., least number of early collections = #1)
hospital_metrics$rank_early_collection <- rank(hospital_metrics$percent_less_than_24_hours, na.last="keep", ties.method="min")

# Rank hospitals by number of samples transfused prior to collection (ascending order;
#     e.g., least number of early collections = #1)
hospital_metrics$rank_transfused <- rank(hospital_metrics$trans_percent, na.last="keep", ties.method="min")

# Rank hospitals by number of unsatisfactory samples (ascending order; e.g., least number of unsats = #1)
hospital_metrics$rank_unsats <- rank(hospital_metrics$unsat_percent, na.last="keep", ties.method="min")

##### ADD UNSAT COUNTS #####

# get count of unsat codes for each submitter
unsat_prep <- dd[!is.na(dd$UNSATCODE),] %>% 
  group_by(SUBMITTERNAME, UNSATCODE) %>%
  filter(BIRTHDATE >= start_date & BIRTHDATE <= end_date) %>%
  summarise(count = n())  

# get all possibilities for unsat codes
unsat_seq <- seq(1:nrow(unsats))

# create cross join of all possible unsat codes and all submitter names
cross_join <- CJ(SUBMITTERNAME=unique(dd$SUBMITTERNAME), UNSATCODE=unsat_seq)

# create left join of unsat counts and cross_join (so we have NAs
# for each submitter that has no unsats for that particular code)
unsat_amts <- left_join(cross_join, unsat_prep, by=c("SUBMITTERNAME", "UNSATCODE"))

# replace UNSATCODE column with 'col' column
unsat_amts$col <- paste("unsat_", str_pad(unsat_amts$UNSATCODE, 2, pad="0"), sep="")
unsat_amts$UNSATCODE <- NULL

# Reshape dataframe to have rows as SUBMITTERNAME and columns as col (e.g., unsat_01, unsat_02, etc.)
unsats_ready <- dcast(unsat_amts, SUBMITTERNAME ~ col, value.var="count")

# Replace column names (unsat_01, etc.) with unsat descriptions
names(unsats_ready) <- c('SUBMITTERNAME', unlist(as.list(as.character(unsats$description), sorted = FALSE)))

# left join unsats_ready and hospital_metrics
hospital_metrics <- left_join(hospital_metrics, unsats_ready, by="SUBMITTERNAME")

############################

# Determine number of hospitals
tot_sub <- nrow(hospital_metrics)

# Create metrics for state
state <- hospital_metrics %>%
  summarise(
    submitters = tot_sub,
    total_samples=sum(total_samples, na.rm=TRUE),
    avg_transit_time = mean(avg_transit_time, na.rm=TRUE),
    min_transit_time = min(min_transit_time, na.rm=TRUE),
    max_transit_time = max(max_transit_time, na.rm=TRUE),
    rec_in_2_days = sum(rec_in_2_days, na.rm=TRUE),
    percent_rec_in_2_days = mean(percent_rec_in_2_days, na.rm=TRUE),
    met_goal = sum(met_goal, na.rm=TRUE),
    percent_met_goal = (met_goal / tot_sub) * 100,
    col_less_than_24_hours = sum(col_less_than_24_hours, na.rm=TRUE),
    percent_less_than_24_hours = mean(percent_less_than_24_hours, na.rm=TRUE),
    trans = sum(trans, na.rm=TRUE),
    trans_percent = mean(trans_percent, na.rm=TRUE),
    unsat_count = sum(unsat_count, na.rm=TRUE),
    unsat_percent = mean(unsat_percent, na.rm=TRUE)
  )

##### CREATE DATA FRAMES FOR USE IN QUARTER PLOTS #####
if (line_chart == "quarterly") {
  quarter_end <- as.Date(as.yearqtr(end_date), frac=1)
  
  # filter dd to include one year of data (dated backwards from end date)
  dd_plot <- filter(dd, BIRTHDATE > (quarter_end - years(1)) & 
                      BIRTHDATE <= quarter_end, !is.na(HOSPITALREPORT))  
  
  # add quarter information to dd_plot
  dd_plot$QUARTER <- as.yearqtr(dd_plot$BIRTHDATE, format="%Y%m")

  # group by submitter and quarter
  hospital_metrics_plot <- dd_plot %>%
    group_by(SUBMITTERNAME, QUARTER) %>%
    select(SUBMITTERNAME, TRANSIT_TIME, UNSATCODE, QUARTER) %>%
    summarise(
      total_samples=n(),   
      percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
      unsat_count = sum(!is.na(UNSATCODE)),
      unsat_percent = unsat_count/total_samples * 100
    )
  
  ## ADDRESS HOSPITALS WITH NO DATA FOR ANY PARTICULAR QUARTER IN THE DATE RANGE OF INTEREST
  
  # get a list of all quarters contained in the dataset
  quarts <- unique(dd_plot$QUARTER)
  
  # create cross join of all possible quarters and all submitter names
  cross_join_quarts <- CJ(SUBMITTERNAME=unique(dd_plot$SUBMITTERNAME), QUARTER=unique(dd_plot$QUARTER))
  
  # left join hospital_metrics_plot with cross_join_quarts so that we have NAs for hospitals 
  # with no data for a particular quarter
  hospital_metrics_plot <- full_join(hospital_metrics_plot, cross_join_quarts, by=c("SUBMITTERNAME", "QUARTER"))
  
  #####

  # determine limits for y-axes by finding max for avg_transit_time
  # and percentage of unsats and min for avg_transit_time. For avg_transit_time use
  # only the greatest value that is less than 4; for unsat percentage,
  # use only the greatest value that is less than 10. 
  transit_percent_col <- grep("percent_rec_in_2_days", colnames(hospital_metrics_plot))
  unsat_percent_col <- grep("unsat_percent", colnames(hospital_metrics_plot))
  min_transit_percent <- quantile(hospital_metrics_plot$percent_rec_in_2_days, 0.02, na.rm=TRUE)
  max_overall_unsat <- quantile(hospital_metrics_plot$unsat_percent, .98, na.rm=TRUE)
  
  # Group by quarter for state totals
  state_plot <- hospital_metrics_plot %>%
    group_by(QUARTER) %>%
    summarise(
      total_samples = sum(total_samples, na.rm=TRUE),   
      percent_rec_in_2_days = mean(percent_rec_in_2_days, na.rm=TRUE),
      unsat_count = sum(unsat_count, na.rm=TRUE),
      unsat_percent = mean(unsat_percent, na.rm=TRUE)
    )
  state_plot$SUBMITTERNAME <- 'State'
  
}
  
##### CREATE DATA FRAMES FOR USE IN MONTH PLOTS #####
if (line_chart == "monthly") {
  month_end <- as.Date(as.yearmon(end_date), frac=1)
  
  # Filter dd to include one year of data (dated backwards from end date)
  dd_plot <- filter(dd, BIRTHDATE > (month_end - years(1)) & 
                      BIRTHDATE <= month_end, !is.na(HOSPITALREPORT))
  
  # add month information to dd_plot
  dd_plot$MONTH <- as.yearmon(dd_plot$BIRTHDATE, format="%Y%m")
  
  # group by submitter and month
  hospital_metrics_plot <- dd_plot %>%
    group_by(SUBMITTERNAME, MONTH) %>%
    select(SUBMITTERNAME, TRANSIT_TIME, UNSATCODE, MONTH) %>%
    summarise(
      total_samples=n(),   
      percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
      unsat_count = sum(!is.na(UNSATCODE)),
      unsat_percent = unsat_count/total_samples * 100
    )
  
  ## ADDRESS HOSPITALS WITH NO DATA FOR ANY PARTICULAR MONTH IN THE DATE RANGE OF INTEREST
  
  # get a list of all months contained in the dataset
  months <- unique(dd_plot$MONTH)
  
  # create cross join of all possible months and all submitter names
  cross_join_months <- CJ(SUBMITTERNAME=unique(dd_plot$SUBMITTERNAME), MONTH=unique(dd_plot$MONTH))
  
  # left join hospital_metrics_plot with cross_join_months so that we have NAs for hospitals with
  # no data for a particular month
  hospital_metrics_plot <- full_join(hospital_metrics_plot, cross_join_months, by=c("SUBMITTERNAME", "MONTH"))
  
  #####
  
  # determine limits for y-axes by finding max for avg_transit_time
  # and percentage of unsats and min for avg_transit_time. For avg_transit_time use
  # only the greatest value that is less than 4; for unsat percentage,
  # use only the greatest value that is less than 10. 
  transit_percent_col <- grep("percent_rec_in_2_days", colnames(hospital_metrics_plot))
  unsat_percent_col <- grep("unsat_percent", colnames(hospital_metrics_plot))
  min_transit_percent <- quantile(hospital_metrics_plot$percent_rec_in_2_days, 0.02, na.rm=TRUE)
  max_overall_unsat <- quantile(hospital_metrics_plot$unsat_percent, .98, na.rm=TRUE)
  
  # Group by month for state totals
  state_plot <- hospital_metrics_plot %>%
    group_by(MONTH) %>%
    summarise(
      total_samples = sum(total_samples, na.rm=TRUE),   
      percent_rec_in_2_days = mean(percent_rec_in_2_days, na.rm=TRUE),
      unsat_count = sum(unsat_count, na.rm=TRUE),
      unsat_percent = mean(unsat_percent, na.rm=TRUE)
    )
  state_plot$SUBMITTERNAME <- 'State'
}

##### PREPARE SUMMARY REPORT - HOSPITAL #####

IDs <- c()

# Get all submitter IDs for each hospital
for (i in 1:nrow(hospital_metrics)) {
  IDs[i] <- paste(submitters[submitters$HOSPITALREPORT 
                          %in% hospital_metrics$SUBMITTERNAME[i],]$SUBMITTERID, collapse="; ")
}

# Reorganize columns
hosp_summary <- hospital_metrics[,c(1:3,15,4:7,16,8:10,17,11:12,18,13:14,19:32)]

# Add submitter IDs
hosp_summary <- cbind(as.data.frame(IDs), hosp_summary)

# Rename columns
hosp_summary_cols <- c("Submitter IDs", "Submitter Name", "Sample Count", "Avg. Transit Time", "Rank: Transit Time",
                  "Min. Transit Time", "Max. Transit Time", "Received within 2 Days", 
                  "% Recevied within 2 Days","Rank: Received within 2 Days",
                  "Met 95% of Samples Received within 2 Days Goal?", "< 24 Hours", 
                  "% < 24 Hours", "Rank: < 24 Hours", "Transfused", "% Transfused", 
                  "Rank: Transfused", "Unsat Count", "Unsat %", "Rank: Unsats", 
                  paste("Unsat:", unlist(as.list(as.character(unsats$description), sorted = FALSE))))
names(hosp_summary) <- hosp_summary_cols

# Replace NAs with 0s
hosp_summary[is.na(hosp_summary)] <- 0

# Change 0s to 'NO' and 1s to 'YES' for met goal
hosp_summary$`Met 95% of Samples Received within 2 Days Goal?` <- 
  ifelse(hosp_summary$`Met 95% of Samples Received within 2 Days Goal?` == 0, 'no', 'yes')

# Write to csv for now - may ultimately want to write to Excel
write.csv(hosp_summary, paste0(admin_path, "/hosp_summary.csv"))

##### PREPARE SUMMARY REPORT - STATE #####

# Get state metrics averaged across hospitals
state_summary <- state

# Get state metrics averaged across all hospital samples
state_h_samp <- dd %>%
  select(TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, RECALL_FLAG, TRANSFUSED) %>%
  filter(BIRTHDATE >= start_date & BIRTHDATE <= end_date) %>%
  summarise(
    submitters = tot_sub,
    total_samples=n(),
    avg_transit_time = mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    min_transit_time = min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE),
    percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
      sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
    met_goal = state_summary$met_goal,
    percent_met_goal = (met_goal / tot_sub) * 100,
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                 na.rm=TRUE),
    percent_less_than_24_hours = col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100,
    trans = sum(TRANSFUSED == 'Y'),
    trans_percent = trans/total_samples * 100,
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples * 100
  )

# Get state metrics averaged across ALL samples (both hospital and non-hospital)

# filter initial_dd for time period of interest and remove samples with no SUBMITTERID
initial_dd_filt <- initial_dd %>%
  filter(BIRTHDATE >= start_date & BIRTHDATE <= end_date, SUBMITTERID != "" | !is.na(SUBMITTERID))

# Get count of submitters, both hospital and non-hospital
# (count all the unique IDs in data, subtract the number of hospital
# IDs since some hospitals have multiple IDs, then add back in the number
# of hospitals whose names appear in both submitters and dd (not initial_dd
# in this case)
all_sub <- length(unique(initial_dd_filt$SUBMITTERID)) - 
  length(intersect(initial_dd_filt$SUBMITTERID, submitters$SUBMITTERID)) + 
  length(intersect(submitters$HOSPITALREPORT, dd$SUBMITTERNAME))

# Identify ALL submitters that met the 95% goal for samples received within 2 days
met_goal_test <- initial_dd_filt %>%
  group_by(SUBMITTERID) %>%
  summarise(
    rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE),
    percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
      sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
    met_goal = ifelse(percent_rec_in_2_days >= 95, 1, 0)
  )

# Get state averages for samples submitted by ALL submitters
state_all_samp <- initial_dd_filt %>%
  select(SUBMITTERID, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, RECALL_FLAG, TRANSFUSED) %>%
  summarise(
    submitters = all_sub,
    total_samples=n(),
    avg_transit_time = mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    min_transit_time = min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE),
    percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
      sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
    met_goal = sum(met_goal_test$met_goal, na.rm=TRUE),
    percent_met_goal = (met_goal / all_sub) * 100,
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                 na.rm=TRUE),
    percent_less_than_24_hours = col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100,
    trans = sum(TRANSFUSED == 'Y'),
    trans_percent = trans/total_samples * 100,
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples * 100
  )  

state_summary <- rbind(state_summary, state_h_samp, state_all_samp)

state_cols <- c("Number of Submitters","Sample Count","Avg. Transit Time","Min. Transit Time",
                "Max. Transit Time","Received within 2 Days","% Received within 2 Days",
                "Number of Submitters Meeting 95% of Samples Received within 2 Days Goal",
                "% Submitters Meeting 95% of Samples within 2 Days Goal",
                "< 24 Hours","% < 24 Hours","Transfused", "% Transfused", 
                "Unsat Count", "Unsat %")

names(state_summary) <- state_cols

#######################################################

# Change hospital metrics to include only a single submitter (if we are only testing the functionality rather
# than running all reports)
if (test_report == "Y") hospital_metrics = hospital_metrics[1,]

# Generate report for each hospital
render_file <- paste(wd, "/", "main_report_markdown.Rmd", sep="")

for (submitter in hospital_metrics$SUBMITTERNAME){
  rmarkdown::render(input = render_file, 
                    output_format = "pdf_document",
                    output_file = paste(submitter, "_", Sys.Date(), ".pdf", sep=''),
                    output_dir = hospital_path)
}
