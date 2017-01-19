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
 
hospital_path <- "/mnt/hospital_reports"
 
########
 
# ADMINISTRATIVE REPORT OUTPUT DIRECTORY (FOR DCLS)
# Enter the location on your computer where you want administrative
# reports to be saved
 
admin_path <- "/mnt/admin_reports"
 
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
 
# *** DO NOT CHANGE THIS CODE ***
 
# LOAD PACKAGES AND FUNCTIONS
load_packages <- paste0(wd, "/load_packages_and_functions.R")
source(load_packages)
 
# GENERATE LIST OF DIAGNOSES IN DATA SET THAT NEED NARRATIVES
# BEFORE RUNNING DIAGNOSIS REPORTS - THIS WILL OUTPUT A CSV
# TO THE FOLDER YOU DESIGNATE IN THE admin_path VARIABLE
# ABOVE (make sure to run the load_packages above before running
# this code)
diag_narr_test <- paste0(wd, "/diagnosis_narrative_test.R")
source(diag_narr_test)
 
# RUN REPORTS
report_run <- paste0(wd, "/main_report_generator.R")
diag_run <- paste0(wd, "/diagnosis_report_generator.R")
source(report_run)
source(diag_run)
