# DO NOT RUN THIS FILE DIRECTLY, AS THE VARIABLES USED FOR
# FILE LOCATION AND NAMES ARE SET IN "run_file_and_variable_setting.R."
# IF YOU RUN THAT FILE, IT WILL ALLOW YOU TO SET YOUR FILE LOCATIONS
# AND ALSO RUN THIS FILE.
 
# read in unsats csv file
unsats <- read.csv(paste(codes_path, "/", "unsat_codes.csv", sep=""))
 
# read in submitter names as we wish them to appear in the report
temp2 <- paste(codes_path, "/", "VA NBS Report Card Hospital Names.csv", sep="")
submitters <- as.data.frame(read.csv(temp2, sep=","))
names(submitters) <- c("HOSPITALCODE","HOSPITALREPORT")
submitters$HOSPITALCODE <- as.character(submitters$HOSPITALCODE)
 
# read in data
files <- list.files(data_path, pattern="*.txt")
temp <- paste(data_path, "/", files, sep="")
dd <- do.call(rbind, lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, sep=separator)))
 
# reformat dates as dates
dd$COLLECTIONDATE <- as.Date(dd$COLLECTIONDATE, "%m/%d/%Y", origin = "1904-01-01")
dd$BIRTHDATE <- as.Date(dd$BIRTHDATE, "%m/%d/%Y", origin = "1904-01-01")
start_date <- as.Date(start_date, "%m/%d/%Y")
end_date <- as.Date(end_date, "%m/%d/%Y")
 
# add hospitalreport name to dd
dd <- left_join(dd, submitters, by="HOSPITALCODE")
 
# MAY NEED TO ADD ADDITIONAL CODE TO PULL IN 
# SUBMITTER NAME BY SUBMITTER ID IF HOSPITAL CODE IS MISSING
 
# replace submitter name with hospitalreport field
dd$SUBMITTERNAME <- dd$HOSPITALREPORT
 
# create data frame of required metrics for each submitter
hospital_metrics <- dd[!is.na(dd$HOSPITALREPORT),] %>%
  group_by(SUBMITTERNAME) %>%
  select(SUBMITTERNAME, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, RECALL_FLAG, TRANSFUSED) %>%
  filter(BIRTHDATE >= start_date & BIRTHDATE <= end_date) %>%
  summarise(
    total_samples=n(),
    avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
    min_transit_time = min(TRANSIT_TIME, na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME, na.rm=TRUE),
    rec_in_2_days = ifelse(!is.nan(sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME))), 
                           sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME)), NA),
    met_goal = ifelse(rec_in_2_days >= 0.95, 1, 0),
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME, na.rm=TRUE),
    percent_less_than_24_hours = ifelse(!is.infinite(col_less_than_24_hours/sum(RECALL_FLAG == "N")),
                                        col_less_than_24_hours/sum(RECALL_FLAG == "N"), NA),
    trans = sum(TRANSFUSED == 'Y'),
    trans_percent = trans/total_samples,
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples,
    unsat_1 = ifelse(sum(UNSATCODE == 1, na.rm=TRUE) != 0, sum(UNSATCODE == 1, na.rm=TRUE), NA),
    unsat_2 = ifelse(sum(UNSATCODE == 2, na.rm=TRUE) != 0, sum(UNSATCODE == 2, na.rm=TRUE), NA),
    unsat_3 = ifelse(sum(UNSATCODE == 3, na.rm=TRUE) != 0, sum(UNSATCODE == 3, na.rm=TRUE), NA),
    unsat_4 = ifelse(sum(UNSATCODE == 4, na.rm=TRUE) != 0, sum(UNSATCODE == 4, na.rm=TRUE), NA),
    unsat_5 = ifelse(sum(UNSATCODE == 5, na.rm=TRUE) != 0, sum(UNSATCODE == 5, na.rm=TRUE), NA),
    unsat_6 = ifelse(sum(UNSATCODE == 6, na.rm=TRUE) != 0, sum(UNSATCODE == 6, na.rm=TRUE), NA),
    unsat_7 = ifelse(sum(UNSATCODE == 7, na.rm=TRUE) != 0, sum(UNSATCODE == 7, na.rm=TRUE), NA),
    unsat_8 = ifelse(sum(UNSATCODE == 8, na.rm=TRUE) != 0, sum(UNSATCODE == 8, na.rm=TRUE), NA),
    unsat_9 = ifelse(sum(UNSATCODE == 9, na.rm=TRUE) != 0, sum(UNSATCODE == 9, na.rm=TRUE), NA),
    unsat_10 = ifelse(sum(UNSATCODE == 10, na.rm=TRUE) != 0, sum(UNSATCODE == 10, na.rm=TRUE), NA),
    unsat_11 = ifelse(sum(UNSATCODE == 11, na.rm=TRUE) != 0, sum(UNSATCODE == 11, na.rm=TRUE), NA),
    unsat_12 = ifelse(sum(UNSATCODE == 12, na.rm=TRUE) != 0, sum(UNSATCODE == 12, na.rm=TRUE), NA),
    unsat_13 = ifelse(sum(UNSATCODE == 13, na.rm=TRUE) != 0, sum(UNSATCODE == 13, na.rm=TRUE), NA)
  )
 
 
# Overall unsat percentage:
unsatp <- (sum(!is.na(dd$UNSATCODE)))/nrow(dd)
 
# Calculate percent of specific unsatisfactory samples
unsat1 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 1])))/nrow(dd)
unsat2 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 2])))/nrow(dd)
unsat3 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 3])))/nrow(dd)
unsat4 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 4])))/nrow(dd)
unsat5 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 5])))/nrow(dd)
unsat6 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 6])))/nrow(dd)
unsat7 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 7])))/nrow(dd)
unsat8 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 8])))/nrow(dd)
unsat9 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 9])))/nrow(dd)
unsat10 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 10])))/nrow(dd)
unsat11 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 11])))/nrow(dd)
unsat12 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 12])))/nrow(dd)
unsat13 <- (sum(!is.na(dd$UNSATCODE[dd$UNSATCODE == 13])))/nrow(dd)
 
 
# Unsat samples per each hospital for previous 3 quarters
 
 #Calculate quarter end from user inputted start date
quarter_end1 <- as.Date(as.yearqtr(start_date,"%m/%d/%Y") - 1/4, frac = 1)
quarter_end2 <- as.Date(as.yearqtr(start_date, "%m/%d/%Y") - 1/2, frac = 1)
quarter_end3 <- as.Date(as.yearqtr(start_date, "%m/%d/%Y") - 3/4, frac = 1)
 
# Calculate quarter start date from each quarter end date
quarter_start1 <- as.Date(as.yearqtr(quarter_end1) - 1/4, frac = 1) + 1
quarter_start2 <- as.Date(as.yearqtr(quarter_end2) - 1/4, frac = 1) + 1
quarter_start3 <- as.Date(as.yearqtr(quarter_end3) - 1/4, frac = 1) + 1
 
 
 
# Rank hospitals by mean transit time (ascending order; e.g., least mean transit time = #1)
hospital_metrics$rank_transit <- rank(hospital_metrics$avg_transit_time[hospital_metrics$SUBMITTERNAME != "NA"], 
                                      na.last="keep", ties.method="min")
 
# Rank hospitals by percentage of samples recevied within 2 days (descending order; e.g.,
#      greatest percentage of samples received by target time = #1)
hospital_metrics$rank_percent_within_goal <- rank(-hospital_metrics$rec_in_2_days[hospital_metrics$SUBMITTERNAME != "NA"], 
                                                  na.last="keep", ties.method="min")
 
# Rank hospitals by number of samples collected at less than 24 hours of age (ascending order;
#     e.g., least number of early collections = #1)
hospital_metrics$rank_early_collection <- rank(hospital_metrics$col_less_than_24_hours[hospital_metrics$SUBMITTERNAME != "NA"], 
                                               na.last="keep", ties.method="min")
 
# Rank hospitals by number of unsatisfactory samples (ascending order; e.g., least number of unsats = #1)
hospital_metrics$rank_unsats <- rank(hospital_metrics$unsat_count[hospital_metrics$SUBMITTERNAME != "NA"], 
                                     na.last="keep", ties.method="min")
 
# Change hospital metrics to include only a single submitter (if we are only testing the functionality rather
# than running all reports)
if (test_report == "Y") hospital_metrics = hospital_metrics[1,]
 
# Create metrics for state
state <- dd %>%
  select(TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, RECALL_FLAG, TRANSFUSED) %>%
  filter(BIRTHDATE >= start_date & BIRTHDATE <= end_date) %>%
  summarise(
    total_samples=n(),
    avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
    min_transit_time = min(TRANSIT_TIME, na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME, na.rm=TRUE),
    rec_in_2_days = ifelse(!is.nan(sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME))), 
                           sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME)), NA),
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME, na.rm=T),
    percent_less_than_24_hours = ifelse(!is.infinite(col_less_than_24_hours/sum(RECALL_FLAG == "N")),
                                        col_less_than_24_hours/sum(RECALL_FLAG == "N"), NA),
    trans = sum(TRANSFUSED == 'Y'),
    trans_percent = trans/total_samples,
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples
  )
 
# Add number of hospitals to state df
state$submitters <- nrow(hospital_metrics)
 
##### CREATE DATA FRAMES FOR USE IN QUARTER PLOTS #####
 
# Filter dd to include one year of data (dated backwards from end date)
dd_plot <- filter(dd, BIRTHDATE > (end_date - years(1)) & 
                    BIRTHDATE <= end_date, !is.na(HOSPITALREPORT))
 
# add quarter information to dd_plot
dd_plot$QUARTER <- as.yearqtr(dd_plot$BIRTHDATE, format="%Y%m")
 
# group by submitter and quarter
hospital_metrics_plot <- dd_plot %>%
  group_by(SUBMITTERNAME, QUARTER) %>%
  select(SUBMITTERNAME, TRANSIT_TIME, UNSATCODE, QUARTER) %>%
  summarise(
    total_samples=n(),   
    avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples * 100
  )
 
# determine limits for y-axes by finding max for avg_transit_time
# and percentage of unsats and min for avg_transit_time. For avg_transit_time use
# only the greatest value that is less than 4; for unsat percentage,
# use only the greatest value that is less than 10. 
avg_transit_col <- grep("avg_transit_time", colnames(hospital_metrics_plot))
unsat_percent_col <- grep("unsat_percent", colnames(hospital_metrics_plot))
max_overall_transit <- max(hospital_metrics_plot[hospital_metrics_plot$avg_transit_time < 4,avg_transit_col])
min_overall_transit <- min(hospital_metrics_plot$avg_transit_time)
max_overall_unsat <- max(hospital_metrics_plot[hospital_metrics_plot$unsat_percent < 10,unsat_percent_col])
 
# Group by quarter for state totals (NOTE: this uses unweighted mean)
state_plot <- dd_plot %>%
  group_by(QUARTER) %>%
  select(QUARTER, TRANSIT_TIME, UNSATCODE) %>%
  summarise(
    total_samples=n(),   
    avg_transit_time = mean(TRANSIT_TIME, na.rm=TRUE),
    unsat_count =  sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples * 100
  )
state_plot$SUBMITTERNAME <- 'State'
 
#######################################################
 
# Generate report for each hospital
render_file <- paste(wd, "/", "r_script_pdf.Rmd", sep="")
 
for (submitter in hospital_metrics$SUBMITTERNAME){
  rmarkdown::render(input = render_file, 
                    output_format = "pdf_document",
                    output_file = paste(submitter, "_", Sys.Date(), ".pdf", sep=''),
                    output_dir = output_path)
}
 
# Remove variables from environment
# keep = 'dd'
# keep = ''
# rm(list=setdiff(ls(), keep))
