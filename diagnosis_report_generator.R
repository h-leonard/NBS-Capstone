# read in descriptions of diagnoses for use in report
diag_desc <- read.csv(paste(codes_path, "/", "diagnosis_descriptions.csv", sep=""), stringsAsFactors = FALSE)
diag_desc <- diag_desc[,2:3]
 
# prepare to read in data
files_diag <- list.files(diag_data_path)
temp_diag <- paste(diag_data_path, "/", files_diag, sep="")
 
# find out what type of data files we have by getting the file (assumes all data files are same file type)
data_type_diag <- substr(files_diag[1], as.numeric(regexpr("\\.([[:alnum:]]+)$", files_diag[1])[1]), nchar(files_diag[1]))
 
# read in data using different methods depending on what type of data files we have (e.g., .xls vs. .txt)
if (data_type_diag == ".xlsx" | data_type_diag == ".xls") {
  initial_dd_diag <- do.call(rbind, lapply(temp_diag, function(x) readWorksheet(loadWorkbook(x), sheet = 1, header=TRUE)))
} else {
  initial_dd_diag <- do.call(rbind, lapply(temp_diag, function(x) read.csv(x, stringsAsFactors = FALSE, sep=separator)))
}
 
# reformat dates as dates
initial_dd_diag$DIAGNOSISDATE <- as.Date(initial_dd_diag$DIAGNOSISDATE, "%m/%d/%Y", origin = "1904-01-01")
 
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
                    output_dir = output_path)
}
 
# Remove variables from environment
# keep = 'diagnoses'
# keep = ''
# rm(list=setdiff(ls(), keep))
