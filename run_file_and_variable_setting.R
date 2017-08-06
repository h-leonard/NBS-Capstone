##### Change the settings below as needed #####

# SET COMPUTER TYPE
# Enter 'PC' or 'MAC'

comp_type <- 'MAC'

#########

# SET WORKING DIRECTORY
# This should be where you have all of your R code files stored

wd <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/FINAL DELIVERABLES (and other code)/r_files/report_card"

#########

# SAMPLE DATA FILE PATH
# This should be your directory (e.g., the location on your 
# computer) where you have the SAMPLE data stored. 
# **VERY IMPORTANT**: make sure that you ONLY have the sample data
# files you want to read in at this location, and that all of them are in the 
# same exact format (e.g., xlsx, xls, txt, or csv).

sample_data_path <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/FINAL DELIVERABLES (and other code)/Sample"

#########

# DIAGNOSIS DATA FILE PATH
# This should be your directory (e.g., the location on your 
# computer) where you have the DIAGNOSIS data stored. 
# **VERY IMPORTANT**: make sure that you ONLY have the diagnosis data
# files you want to read in at this location, and that all of them are in the 
# same exact format (e.g., xlsx, xls, txt, or csv).

diag_data_path <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/FINAL DELIVERABLES (and other code)/Diagnosis"

#########

# SEPARATOR
# Enter the separator (e.g., '|' or ',' that is used to separate between fields in your data files).

separator <- "|"

#########

# CODES FILE PATH
# This should be your directory (e.g., the location on your
# computer) where you have all supporting csv files. Required
# at this location are five files:
#   -- diagnosis_descriptions.csv - narratives for each diagnosis for use in diagnosis reporting
#   -- hospital_emails.csv - email addresses for each hospital for sending reports
#   -- hospital_messages.csv - individual messages to hospitals to be included in their reports
#   -- unsat_codes.csv - descriptions for each unsatisfactory code
#   -- VA NBS Report Card Organization Names.csv - hospital codes and names as we want them to appear
#             in their reports

codes_path <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/FINAL DELIVERABLES (and other code)/submitter_and_unsat_codes"

#########

# REPORT OUTPUT DIRECTORY (FOR HOSPITALS)
# Enter the location on your computer where you want the hospital
# reports to be saved

hospital_path <- "/Users/chrispatrick/Desktop/Output"

#########

# REPORT OUTPUT DIRECTORY (FOR BIRTH CENTERS)
# Enter the location on your computer where you want the birth center
# reports to be saved

center_path <- "/Users/chrispatrick/Desktop/Output"

########

# ADMINISTRATIVE REPORT OUTPUT DIRECTORY (FOR DCLS)
# Enter the location on your computer where you want administrative
# reports to be saved

admin_path <- "/Users/chrispatrick/Desktop/Output"

########

# INCLUDE MIN AND MAX TRANSIT TIMES
# Enter 'Y' if you wish to include these in the hospital reports;
# otherwise enter 'N'.

min_max <- "N"

########

# INDICATE WHETHER YOU WISH TO RUN REPORTS FOR HOSPITALS OR BIRTHCENTERS
# Enter 'H' to indicate that you wish to run hospital reports, or 'BC' for
# birthcenter reports

report_type <- "H"

########

# INDICATE WHETHER YOU WISH TO USE RECEIVEDATE OR BIRTHDATE FOR RUNNING SUMMARY REPORTS
# Enter 'RECEIVEDATE' or 'BIRTHDATE' to indicate the filter you wish to use for running
# summary reports. Note that report cards always use RECEIVEDATE and this cannot be changed,
# so summary reports will not align with the report cards if 'BIRTHDATE' is chosen.

summary_filter <- "RECEIVEDATE"

########

# DATES FOR REPORT
# Enter the dates you want to use for generating the hospital reports
# Use "mm/dd/yyyy" format (e.g., "04/01/2016")

start_date <- "11/01/2016"
end_date <- "11/30/2016"

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

# RUN ONLY REPORTS FOR SPECIFIC HOSPITALS OR BIRTHCENTERS
# If you want to run all reports or only a test report, keep the value
# of this variable as NULL. If you want to run a report for a specific hospital
# or set of hospitals, enter their names. IMPORTANT: the names as entered
# should match the VA NBS Report Card Organization Names.csv exactly.
#   Format for keeping this variable blank (so all reports run): NULL
#   Format for entering a single hospital: "Augusta Health"
#   Format for entering multiple hospitals: c("Augusta Health","Naval Medical Center - Portsmouth")

# only_run <- c("Augusta Health","Naval Medical Center - Portsmouth")
# only_run <- "Naval Medical Center - Portsmouth"
only_run <- NULL

#########

# *** DO NOT CHANGE THIS CODE ***

# SET FILE SEPARATOR
slash <- ifelse(comp_type == 'PC', '\\', '/')

# GENERATE LIST OF DIAGNOSES IN DATA SET THAT NEED NARRATIVES
# BEFORE RUNNING DIAGNOSIS REPORTS - THIS WILL OUTPUT A CSV
# TO THE FOLDER YOU DESIGNATE IN THE admin_path VARIABLE
# ABOVE
diag_narr_test <- paste0(wd, slash, "diagnosis_narrative_test.R")
source(diag_narr_test)

# RUN REPORTS FOR HOSPITALS or BIRTHCENTERS
report_run <- paste0(wd, slash, "main_report_generator.R")
source(report_run)

diag_run <- paste0(wd, slash, "diagnosis_report_generator.R")
source(diag_run)

# RUN SUMMARY AND NARRATIVE REPORTS
summary_run <- paste0(wd, slash, "summary_report_generator.R")
source(summary_run)

narrative_list <- paste0(wd, slash, "narrative_list_generator.R")
source(narrative_list)
