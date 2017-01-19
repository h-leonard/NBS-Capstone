# read in narratives for diagnoses
diag_narr <- read.csv(paste(codes_path, "/", "diagnosis_descriptions.csv", sep=""), stringsAsFactors = FALSE)
diag_narr <- diag_narr[,2:3]
 
# read in diagnosis data
initial_dd_diag <- read_data(diag_data_path, "DIAGNOSISDATE")
 
# join diagnoses and narratives
dd_diag_narr <- full_join(initial_dd_diag, diag_narr, by="DIAGNOSIS")
 
# filter diagnoses based on start date and end date
dd_diag <- dd_diag_narr %>%
  filter(DIAGNOSISDATE >= start_date & DIAGNOSISDATE <= end_date)
  
# filter diagnoses based on start date and end date
need_narratives_list <- dd_diag_narr %>%
  filter(DIAGNOSISDATE >= start_date & DIAGNOSISDATE <= end_date & (DESCRIPTION == "" | is.na(DESCRIPTION))) %>%  
  select(DIAGNOSIS, DESCRIPTION) %>%
  group_by(DIAGNOSIS) %>%
  summarise(
    total=sum(is.na(DESCRIPTION))
  )
 
# determine if diagnosis is in current version of file with narratives or not
need_narratives_list$`In Diagnosis List File?` <- ifelse(need_narratives_list$total > 0, "no", "yes")
need_narratives_list$total <- NULL
 
# write data frame of diagnoses needing narratives to 
write.csv(need_narratives_list, paste0(admin_path, "/need_narratives.csv"))
