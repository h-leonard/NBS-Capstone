# Source load_packages file
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)
 
# Read in narratives for diagnoses, if diag_narr doesn't yet exist
if(!exists("diag_narr")) {
  diag_narr <- read.csv(paste(codes_path, slash, "diagnosis_narratives.csv", sep=""), 
                      stringsAsFactors = FALSE)
}
 
# Make copy of diag_narr for use in listing narratives (leaving out variant names)
diag_narr_list <- diag_narr[,c(1,3)]
 
#######################################################
  
# Remove any line breaks, then add hard line breaks to all narratives
diag_narr_list$Narrative <- gsub("\\n", "", diag_narr_list$Narrative)
diag_narr_list$Narrative <- paste0(diag_narr_list$Narrative, "\\\n")
 
# Generate report of disorder narratives
render_file_narr <- paste(wd, slash, "narrative_list_markdown.Rmd", sep="")
 
rmarkdown::render(input = render_file_narr, 
                  output_format = "pdf_document",
                  output_file = paste("Disorder_Narrative_List.pdf"),
                  output_dir = admin_path)
