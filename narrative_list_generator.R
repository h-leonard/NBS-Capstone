# Source load_packages file
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)

# Read in narratives for diagnoses
diag_narr <- read.csv(paste(codes_path, slash, "diagnosis_narratives.csv", sep=""), 
                      stringsAsFactors = FALSE)

# Change column name for VariantNames
colnames(diag_narr)[2] <- "Variant Names"

# Add space after semicolons
diag_narr$`Variant Names` <- gsub(";", "; ", diag_narr$`Variant Names`)

# Drop last 2 columns
diag_narr <- diag_narr[, 1:3]

#######################################################
  
# Remove any line breaks, then add hard line breaks to all narratives
diag_narr$Narrative <- gsub("\\n", "", diag_narr$Narrative)
diag_narr$Narrative <- paste0(diag_narr$Narrative, "\\\n")

# Add hard line breaks to all variant names
diag_narr$`Variant Names` <- paste0(diag_narr$`Variant Names`, "\\\n")

# Generate report of disorder narratives
render_file_narr <- paste(wd, slash, "narrative_list_markdown.Rmd", sep="")

rmarkdown::render(input = render_file_narr, 
                  output_format = "pdf_document",
                  output_file = paste("Disorder_Narrative_List.pdf"),
                  output_dir = admin_path)
