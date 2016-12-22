##### Change the settings below as needed #####
 
# SET WORKING DIRECTORY
# This should be where you have all of your R code files stored
 
wd <- "r_files"
 
#########
 
# DATA FILE PATH
# This should be your directory (e.g., the location on your 
# computer) where you have the data stored
 
data_path <- "data"
 
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
 
# REPORT OUTPUT DIRECTORY
# Enter the location on your computer where you want the hospital
# reports to be saved
 
output_path <- "/mnt/reports"
 
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
load_packages <- paste(wd, "/load_packages.R", sep="")
run_file <- paste(wd, "/r_script_pdf.R", sep="")
source(load_packages)
source(run_file)
