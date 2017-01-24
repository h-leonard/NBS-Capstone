# Stop running code if duplicate submitter IDs have been discovered in 'main_report_generator'
if(nrow(ID_test) != 0) {stopQuietly()}

# read in descriptions of diagnoses for use in report
diag_desc <- read.csv(paste(codes_path, "/", "diagnosis_descriptions.csv", sep=""), stringsAsFactors = FALSE)
diag_desc <- diag_desc[,2:3]

# read in individual messages to hospitals to be included in report
messages <- read.csv(paste(codes_path, "/", "hospital_messages.csv", sep=""), stringsAsFactors = FALSE)
messages <- messages[messages$Message != "",]

# read in sample data and reformat DIAGNOSISDATE as dates
initial_dd_diag <- read_data(diag_data_path, "DIAGNOSISDATE")

# filter diagnoses based on start date and end date
dd_diag <- initial_dd_diag %>%
  filter(DIAGNOSISDATE >= start_date & DIAGNOSISDATE <= end_date)

# join diagnoses to dd dataframe based on sampleID
dd_diag_hosp <- inner_join(dd_diag, dd, by="SAMPLEID")

# select columns from dd_diag_hosp for report, get count of each diagnosis
diagnoses <- dd_diag_hosp %>%
  select(DIAGNOSIS, SUBMITTERNAME) %>%
  group_by(SUBMITTERNAME, DIAGNOSIS) %>%
  summarise(
    total = n()
  )

#######################################################

# Change diagnosis report to include only a single submitter (if we are only testing the functionality rather
# than running all reports)
if (test_report == "Y") diagnoses = diagnoses[diagnoses$SUBMITTERNAME == diagnoses$SUBMITTERNAME[1],]

# Get date range for report
dates <- paste(gsub(" 0", " ", format(start_date, format="%B %d, %Y")), " - ", 
                  gsub(" 0", " ", format(end_date, format="%B %d, %Y")), sep="")

# Add diagnoses descriptions to data frame
diag_info <- inner_join(diagnoses, diag_desc, by="DIAGNOSIS")

# Generate report for each hospital
render_file_diag <- paste(wd, "/", "diagnosis_markdown.Rmd", sep="")

for (submitter in diagnoses$SUBMITTERNAME){
  rmarkdown::render(input = render_file_diag, 
                    output_format = "pdf_document",
                    output_file = paste(submitter, "_DiagnosisReport_", Sys.Date(), ".pdf", sep=''),
                    output_dir = hospital_path)
}

# Remove variables from environment
# keep = 'diagnoses'
# keep = ''
# rm(list=setdiff(ls(), keep))
