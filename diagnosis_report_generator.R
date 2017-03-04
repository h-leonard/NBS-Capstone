# Source loadpackages file
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)
 
# Source diagnosis_prep.R for initial preparation of dataset
source(paste0(wd, slash, "diagnosis_prep.R"))
 
# filter out diagnoses not associated with hospitals
dd_diag_hosp <- dd_diag_narr[dd_diag_narr$SUBMITTERID %in% submitters$SUBMITTERID,]
 
# add submitter name to each record
dd_diag_hosp <- left_join(dd_diag_hosp, submitters, by="SUBMITTERID")
colnames(dd_diag_hosp)[which(names(dd_diag_hosp) == "HOSPITALREPORT")] <- "SUBMITTERNAME"
 
# Select columns from dd_diag_hosp for report, get count of each diagnosis
# NOTE: if a diagnosis is associated with more than one hospital (e.g., 
# because the infant was born at a particular hospital, transferred
# to another hospital, and both submitted samples that ended up being
# associated with a diagnosis), ALL hospitals submitting samples associated
# with a diagnosis will be 'credited' with this on their reports. For this
# reason, we will NOT want to use a sum of the diagnoses in this dataframe
# to count the total number of diagnoses, as this method will count some
# diagnoses more than once.
diagnoses_temp <- dd_diag_hosp %>%
  select(DISORDER, SUBMITTERNAME, PATIENTID, Narrative) %>%
  group_by(SUBMITTERNAME, DISORDER, PATIENTID, Narrative) %>%
  summarise()
 
# Get count of separate diagnoses associated with samples for each hospital
diagnoses <- diagnoses_temp %>%
  group_by(SUBMITTERNAME, DISORDER, Narrative) %>%
  summarise(Count=n())
 
# Rearrange columns
diagnoses <- diagnoses[,c(1:2,4,3)]
 
#######################################################
 
# Run reports if not sourcing this file from summary_report_generator (which
# sets the value of run_pdfs)
if (!exists("run_pdfs")) {
 
  # Change diagnosis report to include only a single submitter (if we are only testing the 
  # functionality rather than running all reports)
  if (test_report == "Y") diagnoses = diagnoses[diagnoses$SUBMITTERNAME == diagnoses$SUBMITTERNAME[1],]
  
  # Change diagnosis report to include only the submitters indicated if only_run is not ""
  if (!is.null(only_run)) diagnoses = filter(diagnoses, SUBMITTERNAME %in% only_run)
  
  # Get date range for report
  dates <- paste(gsub(" 0", " ", format(start_date, format="%B %d, %Y")), " - ", 
                    gsub(" 0", " ", format(end_date, format="%B %d, %Y")), sep="")
  
  # Generate report for each hospital
  render_file_diag <- paste(wd, slash, "diagnosis_markdown.Rmd", sep="")
  
  for (submitter in diagnoses$SUBMITTERNAME){
    rmarkdown::render(input = render_file_diag, 
                      output_format = "pdf_document",
                      output_file = paste(submitter, "_Diagnoses_", 
                                          start_date, "_", end_date, ".pdf", sep=''),
                      output_dir = hospital_path)
  }
  
}
