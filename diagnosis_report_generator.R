# If user has entered something other than "H" or "BC" for 
# report_type and is not running the summary report, stop the
# file and report an error
if (report_type != "H" & report_type != "BC" & !exists("summary_report")) {
  stop(sprintf("You entered '%s' for the report_type variable. Please change the value for this variable to either 'H' to run reports for hospitals or 'BC' to run reports for birthcenters.",
               report_type))
}

# Source load_packages file
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)

# Source diagnosis_prep.R for initial preparation of dataset
source(paste0(wd, slash, "diagnosis_prep.R"))

# If report_type is "H" (hospital) OR if the summary_report variable exists (indicating we are running
# a summary report), run the hospital code below

if (exists("summary_report") | report_type == "H") {
  
  diagnoses_h <- get_diagnoses(dd_diag_narr, "Hospital")
  
} 

if (exists("summary_report") | report_type == "BC") {
  
  diagnoses_bc <- get_diagnoses(dd_diag_narr, "BirthCenter")
  
}

#######################################################

# Run reports if not sourcing this file from summary_report_generator (which
# sets the value of summary_report)
if (!exists("summary_report")) {
  
  if (report_type == "H") {
    
    diagnoses <- diagnoses_h
    output <- hospital_path
    
  } else if (report_type == "BC") {
    
    diagnoses <- diagnoses_bc
    output <- center_path
    
  }
  
  # If there is at least one diagnosis, run the following code
  if (nrow(diagnoses) > 0) {
    
    # Add a hard line break to each narrative so we have space between rows
    diagnoses$Narrative <- paste0(diagnoses$Narrative, "\\\n")
    
    # Change diagnosis report to include only a single submitter (if we are only testing the 
    # functionality rather than running all reports)
    if (test_report == "Y") diagnoses = diagnoses[diagnoses$SUBMITTERNAME == diagnoses$SUBMITTERNAME[1],]
    
    # Change diagnosis report to include only the submitters indicated if only_run is not ""
    if (!is.null(only_run)) diagnoses = filter(diagnoses, SUBMITTERNAME %in% only_run)
    
    # Get date range for report
    dates <- paste(gsub(" 0", " ", format(start_date, format="%B %d, %Y")), " - ", 
                   gsub(" 0", " ", format(end_date, format="%B %d, %Y")), sep="")
    
    # Change column names
    colnames(diagnoses)[2:4] <- c("Diagnosis", "Count", "Description")
    
    # Generate report for each hospital
    render_file_diag <- paste(wd, slash, "diagnosis_markdown.Rmd", sep="")
    
    for (submitter in diagnoses$SUBMITTERNAME) {
      
      # Subset dataframe
      diag_sub = as.data.frame(diagnoses[diagnoses$SUBMITTERNAME == submitter, 2:4])
      
      # Change spacing for last row
      diag_sub$Description[nrow(diag_sub)] <- gsub("\\\\\\n", "", diag_sub$Description[nrow(diag_sub)])
      
      # Create report
      rmarkdown::render(input = render_file_diag, 
                        output_format = "pdf_document",
                        output_file = paste(submitter, "_Diagnoses_", 
                                            start_date, "_", end_date, ".pdf", sep=''),
                        output_dir = output)
    } 
    
  } else {
    cat("There are no diagnoses for the given time period and the organizations of interest.")
  }
  
}
