# DO NOT RUN THIS FILE DIRECTLY, AS THE VARIABLES USED FOR
# FILE LOCATION AND NAMES ARE SET IN "run_file_and_variable_setting.R."
# IF YOU RUN THAT FILE, IT WILL ALLOW YOU TO SET YOUR FILE LOCATIONS
# AND ALSO RUN THIS FILE.
 
# read in unsats csv file
unsats <- read.csv(paste(codes_path, slash, "unsat_codes.csv", sep=""))
 
# read in submitter names as we wish them to appear in the report
temp <- paste(codes_path, slash, "VA NBS Report Card Hospital Names.csv", sep="")
submitters <- as.data.frame(read.csv(temp, sep=","))
names(submitters) <- c("SUBMITTERID","HOSPITALREPORT")
submitters$SUBMITTERID <- as.character(submitters$SUBMITTERID)
 
# read in individual messages to hospitals to be included in report
messages <- read.csv(paste(codes_path, slash, "hospital_messages.csv", sep=""), stringsAsFactors = FALSE)
messages <- messages[!is.na(messages$Message),]
 
# test for IDs assigned to multiple hospitals in submitters
ID_test <- submitters[(duplicated(submitters$SUBMITTERID) | duplicated(submitters$SUBMITTERID, 
                                                                       fromLast=TRUE)),]
 
# stop report if duplicate IDs are discovered
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
  {stop(sprintf("%s in 'VA NBS Report Card Hospital Names' %s assigned to more than one hospital:\n%s\nPlease correct in 'VA NBS Report Card Hospital Names' before running reports.", 
                e_begin, e_verb, e_messages)) }
}
 
# read in sample data and reformat COLLECTIONDATE and BIRTHDATE as dates
initial_dd_prep <- read_data(sample_data_path, "COLLECTIONDATE", "BIRTHDATE")
 
# join initial_dd with submitter file on submitter ID
initial_dd <- left_join(initial_dd_prep, submitters, by="SUBMITTERID")
 
# create dataframes from prep_sample, filtered on start date and end date
temp_sample_dfs <- create_filt_dfs(initial_dd, type="sample")
dd <- as.data.frame(temp_sample_dfs[1])
year_dd <- as.data.frame(temp_sample_dfs[2])
 
# output report of all submitters not in VA NBS Report Card Hospital Names, 
# with a count of samples, for determining if any submitters need to be
# added to this file
not_in_VA_NBS_hosp <- year_dd %>%
  filter(is.na(HOSPITALREPORT)) %>%
  group_by(SUBMITTERID, SUBMITTERNAME) %>%
  select(SUBMITTERID, SUBMITTERNAME) %>%
  summarise(TOTAL=n()) %>%
  arrange(desc(TOTAL))
 
# write file to admin reports
write.csv(not_in_VA_NBS_hosp, paste0(admin_path, slash, "submitters_not_in_VA_NBS_hospital_file.csv"))
 
# prep data frame of non-hospital info for use in summary reporting
non_dd <- dd[is.na(dd$HOSPITALREPORT),]
 
# replace submitter name with hospitalreport field for dd and year_dd
dd$SUBMITTERNAME <- dd$HOSPITALREPORT
year_dd$SUBMITTERNAME <- year_dd$HOSPITALREPORT
 
# remove records that have NA for submittername
dd <- dd[!is.na(dd$HOSPITALREPORT),]
year_dd <- year_dd[!is.na(year_dd$HOSPITALREPORT),]
 
# set cutoff value for transit time (4 hours, or 1/6 of day)
cutoff <- 1/6
 
##############################
 
# create data frame of required metrics for each submitter
hospital_metrics <- dd %>%
  group_by(SUBMITTERNAME) %>%
  select(SUBMITTERNAME, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, TRANSFUSED) %>%
  summarise(
    total_samples=n(),
    avg_transit_time = round(mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2),
    min_transit_time = min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
    rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE),
    percent_rec_in_2_days = round(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
      sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100, 2),
    met_goal = ifelse(percent_rec_in_2_days >= 95, 1, 0),
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                 na.rm=TRUE),
    percent_less_than_24_hours = round(col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100, 2),
    trans = sum(TRANSFUSED == 'Y', na.rm=TRUE),
    trans_percent = round(trans/total_samples * 100, 2),
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = round(unsat_count/total_samples * 100, 2)
  )
 
# Store hospital metrics as separate data frame for later use in state summaries
temp_hosp_metrics <- hospital_metrics
 
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
    avg_transit_time = round(mean(avg_transit_time, na.rm=TRUE), 2),
    min_transit_time = min(min_transit_time, na.rm=TRUE),
    max_transit_time = max(max_transit_time, na.rm=TRUE),
    rec_in_2_days = sum(rec_in_2_days, na.rm=TRUE),
    percent_rec_in_2_days = round(mean(percent_rec_in_2_days, na.rm=TRUE), 2),
    met_goal = sum(met_goal, na.rm=TRUE),
    percent_met_goal = round((met_goal / tot_sub) * 100, 2),
    col_less_than_24_hours = sum(col_less_than_24_hours, na.rm=TRUE),
    percent_less_than_24_hours = round(mean(percent_less_than_24_hours, na.rm=TRUE), 2),
    trans = sum(trans, na.rm=TRUE),
    trans_percent = round(mean(trans_percent, na.rm=TRUE), 2),
    unsat_count = sum(unsat_count, na.rm=TRUE),
    unsat_percent = round(mean(unsat_percent, na.rm=TRUE), 2)
  )
 
##### CREATE DATA FRAMES FOR USE IN PLOTS #####
 
# Group by submitter and period
hospital_metrics_plot <- year_dd %>%
  group_by(SUBMITTERNAME, PERIOD) %>%
  select(SUBMITTERNAME, TRANSIT_TIME, UNSATCODE, PERIOD) %>%
  summarise(
    total_samples=n(),   
    percent_rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, 
                                na.rm=TRUE)/sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100,
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples * 100
  )
  
## ADDRESS HOSPITALS WITH NO DATA FOR ANY PARTICULAR PERIOD IN THE DATE RANGE OF INTEREST
  
# Create cross join of all possible periods and all submitter names
cross_join_periods <- CJ(SUBMITTERNAME=unique(year_dd$SUBMITTERNAME), PERIOD=unique(year_dd$PERIOD))
  
# Left join hospital_metrics_plot with cross_join_quarts so that we have NAs for hospitals 
# with no data for a particular period
hospital_metrics_plot <- full_join(hospital_metrics_plot, cross_join_periods, 
                                   by=c("SUBMITTERNAME", "PERIOD"))
 
####
  
# Determine limits for y-axes by finding min for transit percent
# and max overall unsat. For min_trainst_percent use the 2% percentile,
# for max overall unsat use the 98% percentile.
transit_percent_col <- grep("percent_rec_in_2_days", colnames(hospital_metrics_plot))
unsat_percent_col <- grep("unsat_percent", colnames(hospital_metrics_plot))
min_transit_percent <- quantile(hospital_metrics_plot$percent_rec_in_2_days, 0.02, na.rm=TRUE)
max_overall_unsat <- quantile(hospital_metrics_plot$unsat_percent, .98, na.rm=TRUE)
  
# Group by period for state totals
state_plot <- hospital_metrics_plot %>%
  group_by(PERIOD) %>%
  summarise(
    total_samples = sum(total_samples, na.rm=TRUE),   
    percent_rec_in_2_days = mean(percent_rec_in_2_days, na.rm=TRUE),
    unsat_count = sum(unsat_count, na.rm=TRUE),
    unsat_percent = mean(unsat_percent, na.rm=TRUE)
  )
state_plot$SUBMITTERNAME <- 'State'
 
#######################################################
 
# Change hospital metrics to include only a single submitter (if we are only testing the functionality 
# rather than running all reports)
if (test_report == "Y") hospital_metrics = hospital_metrics[1,]
 
# Change hospital metrics to include only the submitters indicated if only_run is not NULL
if (!is.null(only_run)) hospital_metrics = filter(hospital_metrics, SUBMITTERNAME %in% only_run)
 
# Generate report for each hospital
render_file <- ifelse(min_max == 'Y', paste(wd, slash, "main_report_markdown_WITH_MINMAX.Rmd", sep=""), 
                      paste(wd, slash, "main_report_markdown.Rmd", sep=""))
 
for (submitter in hospital_metrics$SUBMITTERNAME){
  rmarkdown::render(input = render_file, 
                    output_format = "pdf_document",
                    output_file = paste(submitter, "_", as.character(start_date, format="%m-%d-%y"), "_", 
                                        as.character(end_date, format="%m-%d-%y"), ".pdf", sep=''),
                    output_dir = hospital_path)
}
