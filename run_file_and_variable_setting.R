##### Change the settings below as needed #####
 
# SET WORKING DIRECTORY
# This should be where you have all of your R code files stored
 
wd <- "r_files"
 
#########
 
# SAMPLE DATA FILE PATH
# This should be your directory (e.g., the location on your 
# computer) where you have the SAMPLE data stored. 
# **VERY IMPORTANT**: make sure that you ONLY have the sample data
# files you want to read in at this location, and that all of them are in the 
# same exact format (e.g., xlsx, xls, txt, or csv).
 
sample_data_path <- "data/Report_data"
 
#########
 
# DIAGNOSIS DATA FILE PATH
# This should be your directory (e.g., the location on your 
# computer) where you have the DIAGNOSIS data stored. 
# **VERY IMPORTANT**: make sure that you ONLY have the diagnosis data
# files you want to read in at this location, and that all of them are in the 
# same exact format (e.g., xlsx, xls, txt, or csv).
 
diag_data_path <- "data/Diagnosis_data"
 
#########
 
# SEPARATOR
# Enter the separator (e.g., '|' or ',' that is used to separate between fields in your data files)
 
separator <- "|"
 
#########
 
# CODES FILE PATH
# This should be your directory (e.g., the location on your
# computer) where you have all supporting csv files. Required
# at this location are two files:
# -- VA NBS Report Card Hospital Names.csv - has hospital codes & names
# -- unsat_codes.csv - has descriptions for each unsatisfactory code
 
codes_path <- "submitter_and_unsat_codes"
 
#########
 
# REPORT OUTPUT DIRECTORY (FOR HOSPITALS)
# Enter the location on your computer where you want the hospital
# reports to be saved
 
output_path <- "/mnt/reports"
 
########
 
# SUMMARY REPORT OUTPUT DIRECTORY (FOR DCLS)
# Enter the location on your computer where you want the summary
# reports to be saved
 
summary_path <- "/mnt/summary"
 
########
 
# DATES FOR REPORT
# Enter the dates you want to use for generating the hospital reports
# Use "mm/dd/yyyy" format (e.g., "04/01/2016")
 
start_date <- "07/01/2016"
end_date <- "09/30/2016"
 
########
 
# QUARTERLY OR MONTHLY
# Enter "quarterly" or "monthly" to define the parameters 
# for the line chart for transit time and unsat count 
 
line_chart <- "quarterly"
 
#########
 
# FUNCTIONALITY TEST
# Do you want to run a report for a single submitter to test functionality,
# or do you want reports for all submitters? Enter "Y" if you wish to run
# only a single report, "N" otherwise
 
test_report <- "N"
 
#########
 
# RUN REPORT - do not change this code
load_packages <- paste(wd, "/", "load_packages.R", sep="")
report_run <- paste(wd, "/", "main_report_generator.R", sep="")
diag_run <- paste(wd, "/", "diagnosis_report_generator.R", sep="")
source(load_packages)
source(report_run)
source(diag_run)
