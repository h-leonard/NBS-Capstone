# load packages
library(knitr)
library(markdown)
library(rmarkdown)
library(dplyr)

# read in data and reformat dates as dates
dd <- read.csv('capstone.csv', sep='|')
dd$COLLECTIONDATE <- as.Date(dd$COLLECTIONDATE, "%m/%d/%Y")
dd$BIRTHDATE <- as.Date(dd$BIRTHDATE, "%m/%d/%Y")

# create data frame of required metrics for each submitter
hospital_metrics <- dd %>%
  group_by(SUBMITTERNAME) %>%
  select(SUBMITTERNAME, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, RECALL_FLAG, TRANSFUSED) %>%
  filter(BIRTHDATE >= as.Date("2016-04-01") & BIRTHDATE <= as.Date("2016-06-30")) %>%
  summarise(
    total_samples=n(),
    avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
    min_transit_time = min(TRANSIT_TIME, na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME, na.rm=TRUE),
    rec_in_2_days = ifelse(!is.nan(sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME))), 
                           sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME)), NA),
    met_goal = ifelse(rec_in_2_days >= 0.95, 1, 0),
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME),
    percent_less_than_24_hours = ifelse(!is.nan(col_less_than_24_hours/sum(RECALL_FLAG == "N")),
                                        col_less_than_24_hours/sum(RECALL_FLAG == "N"), NA),
    trans = sum(TRANSFUSED == 'Y'), # need to validate how we identify these
    unsat_count = sum(!is.na(UNSATCODE)),
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

# Calculate percent of total unsatisfactory samples


# Overall unsat percentage:
unsatp <- (sum(!is.na(dd$UNSATCODE)))/nrow(dd)


# Unsat percentage per specific hospital:
for (i in hospital_metrics$SUBMITTERNAME) {
  hospital_metrics["unsat%"] <- hospital_metrics$unsat_count/hospital_metrics$total_samples
}


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

###### 
# IF YOU WANT TO RUN **ALL** OF THE HOSPITAL REPORTS, 'COMMENT OUT' THE NEXT LINE
# OF CODE (YOU CAN DO THIS BY ADDING A POUND SIGN CHARACTER TO THE START OF THE LINE 
# BELOW)
hospital_metrics = hospital_metrics[1,]
######

# Generate report for each hospital

###### 
# CHANGE PATH VARIABLES BELOW TO CORRECT LOCATIONS FOR YOUR COMPUTER. THE FIRST PATH (r_file_path)
# SHOULD BE THE LOCATION WHERE YOU HAVE "r_script_pdf.Rmd" STORED. THE SECOND PATH (output_path)
# SHOULD BE WHERE YOU WANT THE REPORTS FOR THE HOSPITALS TO BE SAVED.
r_file_path <- "/Users/chrispatrick/Documents/Classes/DS 6001/Newborn Screening/Data/"
output_path <- "/Users/chrispatrick/Documents/Classes/DS 6001/Newborn Screening/Data/reports"
######

render_file <- paste(r_file_path, "r_script_pdf.Rmd", sep="")

for (submitter in hospital_metrics$SUBMITTERNAME){
  rmarkdown::render(input = render_file, 
                    output_format = "pdf_document",
                    output_file = paste("test_report_", submitter, "_", Sys.Date(), ".pdf", sep=''),
                    output_dir = output_path)
}
