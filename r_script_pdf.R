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

# read in data
pattern <- paste("*.", data_type, sep="")
files <- list.files(data_path, pattern=pattern)
temp2 <- paste(data_path, "/", files, sep="")
if (data_type == "xlsx") {
  initial_dd <- do.call(rbind, lapply(temp2, function(x) readWorksheet(loadWorkbook(x), sheet = 1, header=TRUE)))
} else {
  initial_dd <- do.call(rbind, lapply(temp2, function(x) read.csv(x, stringsAsFactors = FALSE, sep=separator)))
}

# remove any records that have category listed as "Proficiency", "Treatment", or "Treatment - PKU"
remove_cats <- c("Proficiency","Treatment","Treatment - PKU")
dd <- filter(initial_dd, !(CATEGORY %in% remove_cats))

# reformat dates as dates
dd$COLLECTIONDATE <- as.Date(dd$COLLECTIONDATE, "%m/%d/%Y", origin = "1904-01-01")
dd$BIRTHDATE <- as.Date(dd$BIRTHDATE, "%m/%d/%Y", origin = "1904-01-01")
start_date <- as.Date(start_date, "%m/%d/%Y")
end_date <- as.Date(end_date, "%m/%d/%Y")

# add hospitalreport name to dd
dd <- left_join(dd, submitters, by="SUBMITTERID")

# replace submitter name with hospitalreport field
dd$SUBMITTERNAME <- dd$HOSPITALREPORT

# remove records that have NA for submittername and that are not in the target period
dd <- dd[!is.na(dd$HOSPITALREPORT),]

##############################

# create data frame of required metrics for each submitter
hospital_metrics <- dd %>%
  group_by(SUBMITTERNAME) %>%
  filter(BIRTHDATE >= start_date & BIRTHDATE <= end_date) %>% 
  select(SUBMITTERNAME, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, 
         UNSATCODE, RECALL_FLAG, TRANSFUSED) %>%
  summarise(
    total_samples=n(),
    avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
    min_transit_time = min(TRANSIT_TIME, na.rm=TRUE),
    max_transit_time = max(TRANSIT_TIME, na.rm=TRUE),
    rec_in_2_days = sum(TRANSIT_TIME <= 2, na.rm=TRUE),
    percent_rec_in_2_days = ifelse(!is.nan(sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME))), 
                           sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME)), NA) * 100,
    met_goal = ifelse(rec_in_2_days >= 0.95, 1, 0),
    col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE | 
                                   COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME, na.rm=TRUE),
    percent_less_than_24_hours = ifelse(!is.infinite(col_less_than_24_hours/sum(RECALL_FLAG == "N")),
                                        col_less_than_24_hours/sum(RECALL_FLAG == "N"), NA),
    trans = sum(TRANSFUSED == 'Y'),
    trans_percent = trans/total_samples,
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples
  )

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

# create left join of unsat counts and cross_join (so we have 0 counts
# for each submitter that has no unsats for that particular code)
unsat_amts <- left_join(cross_join, unsat_prep, by=c("SUBMITTERNAME", "UNSATCODE"))

# replace UNSATCODE column with 'col' column
unsat_amts$col <- paste("unsat_", str_pad(unsat_amts$UNSATCODE, 2, pad="0"), sep="")
unsat_amts$UNSATCODE <- NULL

# Reshape dataframe to have rows as SUBMITTERNAME and columns as col (e.g., unsat_01, unsat_02, etc.)
unsats_ready <- dcast(unsat_amts, SUBMITTERNAME ~ col, value.var="count")

# left join unsats_ready and hospital_metrics
hospital_metrics <- left_join(hospital_metrics, unsats_ready, by="SUBMITTERNAME")

############################

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

# Determine number of hospitals
tot_sub <- nrow(hospital_metrics)

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
    rec_in_2_days = sum(TRANSIT_TIME <= 2, na.rm=TRUE),
    percent_rec_in_2_days = ifelse(!is.nan(sum(TRANSIT_TIME <= 2, na.rm=TRUE)/sum(!is.na(TRANSIT_TIME))), 
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
state$submitters <- tot_sub

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
      avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
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
  avg_transit_col <- grep("avg_transit_time", colnames(hospital_metrics_plot))
  unsat_percent_col <- grep("unsat_percent", colnames(hospital_metrics_plot))
  max_overall_transit <- quantile(hospital_metrics_plot$avg_transit_time, .98, na.rm=TRUE)
  min_overall_transit <- min(hospital_metrics_plot$avg_transit_time, na.rm=TRUE)
  max_overall_unsat <- quantile(hospital_metrics_plot$unsat_percent, .98, na.rm=TRUE)
  
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
      avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
      unsat_count = sum(!is.na(UNSATCODE)),
      unsat_percent = unsat_count/total_samples * 100
    )
  
  ## ADDRESS HOSPITALS WITH NO DATA FOR ANY PARTICULAR MONTH IN THE DATE RANGE OF INTEREST
  
  # get a list of all quarters contained in the dataset
  months <- unique(dd_plot$MONTH)
  
  # create cross join of all possible quarters and all submitter names
  cross_join_months <- CJ(SUBMITTERNAME=unique(dd_plot$SUBMITTERNAME), MONTH=unique(dd_plot$MONTH))
  
  # left join hospital_metrics_plot with cross_join_months so that we have NAs for hospitals with
  # no data for a particular month
  hospital_metrics_plot <- full_join(hospital_metrics_plot, cross_join_months, by=c("SUBMITTERNAME", "MONTH"))
  
  #####
  
  # determine limits for y-axes by finding max for avg_transit_time
  # and percentage of unsats and min for avg_transit_time. For avg_transit_time use
  # only the greatest value that is less than 4; for unsat percentage,
  # use only the greatest value that is less than 10. 
  avg_transit_col <- grep("avg_transit_time", colnames(hospital_metrics_plot))
  unsat_percent_col <- grep("unsat_percent", colnames(hospital_metrics_plot))
  max_overall_transit <- quantile(hospital_metrics_plot$avg_transit_time, .98, na.rm=TRUE)
  min_overall_transit <- min(hospital_metrics_plot$avg_transit_time, na.rm=TRUE)
  max_overall_unsat <- quantile(hospital_metrics_plot$unsat_percent, .98, na.rm=TRUE)
  
  # Group by month for state totals (NOTE: this uses unweighted mean)
  state_plot <- dd_plot %>%
    group_by(MONTH) %>%
    select(MONTH, TRANSIT_TIME, UNSATCODE) %>%
    summarise(
      total_samples=n(),   
      avg_transit_time = mean(TRANSIT_TIME, na.rm=TRUE),
      unsat_count =  sum(!is.na(UNSATCODE)),
      unsat_percent = unsat_count/total_samples * 100
    )
  state_plot$SUBMITTERNAME <- 'State'
}

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
