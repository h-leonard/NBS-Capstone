# Stop running code if duplicate submitter IDs have been discovered in 'main_report_generator'
if(nrow(ID_test) != 0) {stopQuietly()}
 
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
 
# Determine number of diagnoses for each hospital
diag_count <- diagnoses %>%
  group_by(SUBMITTERNAME) %>%
  summarise(
    total=sum(total)
  )
 
# Add diagnosis count to hosp_summary
hosp_summary <- left_join(hosp_summary, diag_count, by="SUBMITTERNAME")
 
# Rename columns
hosp_summary_cols <- c("Submitter IDs", "Submitter Name", "Sample Count", "Avg. Transit Time", "Rank: Transit Time",
                       "Min. Transit Time", "Max. Transit Time", "Received within 2 Days", 
                       "% Recevied within 2 Days","Rank: Received within 2 Days",
                       "Met 95% of Samples Received within 2 Days Goal?", "< 24 Hours", 
                       "% < 24 Hours", "Rank: < 24 Hours", "Transfused", "% Transfused", 
                       "Rank: Transfused", "Unsat Count", "Unsat %", "Rank: Unsats", 
                       paste("Unsat:", unlist(as.list(as.character(unsats$description), sorted = FALSE))),
                       "Diagnosis Count")
names(hosp_summary) <- hosp_summary_cols
 
# Replace NAs with 0s
hosp_summary[is.na(hosp_summary)] <- 0
 
# Change 0s to 'NO' and 1s to 'YES' for met goal
hosp_summary$`Met 95% of Samples Received within 2 Days Goal?` <- 
  ifelse(hosp_summary$`Met 95% of Samples Received within 2 Days Goal?` == 0, 'no', 'yes')
 
# Write to csv for now - may ultimately want to write to Excel
write.csv(hosp_summary, paste0(admin_path, "/hosp_summary.csv"))
 
##### PREPARE SUMMARY REPORT - DIAGNOSES #####
 
# PREP FOR COLUMN 1: COUNT OF DIAGNOSES FROM HOSPITALS 
hosp_diag <- diagnoses %>%
  group_by(DIAGNOSIS) %>%
  summarise(
    total_hosp=sum(total)
  )
 
# join full list of diagnoses to count of hospital diagnoses
diag <- as.data.frame(diag_desc$DIAGNOSIS, stringsAsFactors = FALSE)
names(diag) <- "DIAGNOSIS"
diag <- left_join(diag, hosp_diag, by="DIAGNOSIS")
 
# PREP FOR COLUMN 3: COUNT OF DIAGNOSES FROM ALL SUBMITTERS
all_diag <- dd_diag %>%
  group_by(DIAGNOSIS) %>%
  summarise(
    total_all=n()
  )
 
# join counts of diagnoses for all submitters to diag
diag <- left_join(diag, all_diag, by="DIAGNOSIS")
 
# PREP FOR COLUMN 2: GET COUNTS FOR ONLY NON-HOSPITAL SUBMITTERS
diag$total_nonhosp <- diag$total_all - diag$total_hosp
 
# rearrange and rename columns
diag <- diag[,c(1:2,4,3)]
names(diag) <- c("Diagnosis","Count from Hospital Submitters","Count from Other Submitters",
                 "Total Count")
 
# replace NAs with 0s
diag[is.na(diag)] <- 0
 
# Publish diagnosis summary to admin folder
write.csv(diag, paste0(admin_path, "/diagnosis_summary.csv"))
 
##### PREPARE SUMMARY REPORT - STATE #####
 
## COLUMN 1 PREP: HOSPITALS ONLY: Averaged over *hospitals*
state_summary <- state
 
# find counts of each unsat category for hospital submitters
state_h_unsats <- unsats_ready %>%
  replace(is.na(.), 0) %>%
  summarise_each(funs(sum), -SUBMITTERNAME)
 
# add unsat counts to state summary
state_summary <- cbind(state_summary, state_h_unsats)
 
## COLUMN 2 PREP: HOSPITALS ONLY: Averaged over *samples*
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
 
# add unsat counts to state summary
state_h_samp <- cbind(state_h_samp, state_h_unsats)
 
## COLUMN 3 PREP: ALL SUBMITTERS: Averaged over *submitters*
 
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
 
# determine metrics by non-hospital submitter
all_sub_metrics <- initial_dd_filt %>%
  filter(!(SUBMITTERID %in% submitters$SUBMITTERID)) %>%
  group_by(SUBMITTERID) %>%
  select(SUBMITTERID, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
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
 
# Bind non-hospital submitter summaries with hospital summaries
all_sub_metrics$SUBMITTERNAME <- NA
all_sub_metrics <- all_sub_metrics[,c(15,1:14)]
all_sub_metrics$SUBMITTERID <- NULL
all_sub_metrics <- rbind(all_sub_metrics, temp_hosp_metrics)
 
# Get count of unsats by category for all submitters
unsat_all_prep <- initial_dd_filt[!is.na(initial_dd_filt$UNSATCODE),] %>% 
  group_by(UNSATCODE) %>%
  summarise(count = n())  
 
unsat_all_amts <- left_join(unsats, unsat_all_prep, by = c("code" = "UNSATCODE"))
unsat_all_amts[is.na(unsat_all_amts)] <- 0
unsat_all_ready <- as.data.frame(t(unsat_all_amts[,3]))
names(unsat_all_ready) <- unsat_all_amts[,2]
 
# Get state averages across all submitters (hospital and non-hospital)
state_all_sub <- all_sub_metrics %>%
  summarise(
    submitters = all_sub,
    total_samples=sum(total_samples, na.rm=TRUE),
    avg_transit_time = mean(avg_transit_time, na.rm=TRUE),
    min_transit_time = min(min_transit_time, na.rm=TRUE),
    max_transit_time = max(max_transit_time, na.rm=TRUE),
    rec_in_2_days = sum(rec_in_2_days, na.rm=TRUE),
    percent_rec_in_2_days = mean(percent_rec_in_2_days, na.rm=TRUE),
    met_goal = sum(met_goal, na.rm=TRUE),
    percent_met_goal = (met_goal / all_sub) * 100,
    col_less_than_24_hours = sum(col_less_than_24_hours, na.rm=TRUE),
    percent_less_than_24_hours = mean(percent_less_than_24_hours, na.rm=TRUE),
    trans = sum(trans, na.rm=TRUE),
    trans_percent = mean(trans_percent, na.rm=TRUE),
    unsat_count = sum(unsat_count, na.rm=TRUE),
    unsat_percent = mean(unsat_percent, na.rm=TRUE)
  )
 
state_all_sub <- cbind(state_all_sub, unsat_all_ready)
 
## COLUMN 4 PREP: ALL SUBMITTERS: Averaged over *samples*
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
    met_goal = state_all_sub$met_goal,
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
 
state_all_samp <- cbind(state_all_samp, unsat_all_ready)
 
state_summary <- rbind(state_summary, state_h_samp, state_all_sub, state_all_samp)
 
state_cols <- c("Number of Submitters","Sample Count","Avg. Transit Time","Min. Transit Time",
                "Max. Transit Time","Received within 2 Days","% Received within 2 Days",
                "Number of Submitters Meeting 95% of Samples Received within 2 Days Goal",
                "% Submitters Meeting 95% of Samples within 2 Days Goal",
                "< 24 Hours","% < 24 Hours","Transfused", "% Transfused", 
                "Unsat Count", "Unsat %", paste("Unsat:", unlist(as.list(as.character(unsats$description), sorted = FALSE))))
 
names(state_summary) <- state_cols
 
# Transform table and rename columns
state_summary <- as.data.frame(t(state_summary))
names(state_summary) <- c("HOSPITALS ONLY: Averaged over hospitals",
                          "HOSPITALS ONLY: Averaged over samples",
                          "ALL SUBMITTERS: Averaged over submitters",
                          "ALL SUBMITTERS: Averaged over samples")
 
# Add diagnosis summary
diag_hosp <- sum(hosp_summary$`Diagnosis Count`)
diag_all <- nrow(dd_diag)
state_diag <- c(diag_hosp, diag_hosp, diag_all, diag_all)
avg_diag <- c(diag_hosp/tot_sub, diag_hosp/tot_sub, diag_all/all_sub, diag_all/all_sub)
state_summary <- rbind(state_summary, state_diag, avg_diag)
rownames(state_summary)[(nrow(state_summary)-1):(nrow(state_summary))] <- c("Number of Diagnoses","Avg. Number of Diagnoses")
 
# Publish state summary to admin folder
write.csv(state_summary, paste0(admin_path, "/state_summary.csv"))
