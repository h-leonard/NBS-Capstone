# Source load_packages file
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)
 
# Source diagnosis_prep.R for initial preparation of dataset
source(paste0(wd, slash, "diagnosis_prep.R"))
  
# filter diagnoses based on start date and end date
need_narratives_list <- dd_diag_narr %>%
  filter(DIAGNOSISDATE >= start_date & DIAGNOSISDATE <= end_date & (Narrative == "" | is.na(Narrative))) %>%  
  select(DIAGNOSIS, Narrative) %>%
  group_by(DIAGNOSIS) %>%
  summarise(
    total=sum(is.na(Narrative))
  )
 
# determine if diagnosis is in current version of file with narratives or not
need_narratives_list$`In Diagnosis List File?` <- ifelse(need_narratives_list$total > 0, "no", "yes")
need_narratives_list$total <- NULL
 
# write data frame of diagnoses needing narratives to admin folder
write.csv(need_narratives_list, paste0(admin_path, slash, "need_narratives.csv"))
