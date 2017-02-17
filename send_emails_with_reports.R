##### Change the settings below before sending each email #####
 
# FUNCTIONALITY TEST
# Do you want to run a single email to yourself for testing your message 
# before sending to all hospitals? Enter 'Y' if you want to test the
# email first, and also enter the email address to use for sending the test
# email.
 
email_test <- "Y"
test_email_recipient <- "Rhonda.West@dgs.virginia.gov"
 
#########
 
# EMAIL SUBJECT
# This will be what Outlook uses as the subject for the email.
 
subject <- "Quarterly Report from DCLS: 10/1/2016 - 12/31/2016"
 
#########
 
# EMAIL MESSAGE
# This will be what appears in the message body.
# Use '\n' for each carriage return, as in this example:
#   message <- "Hi,\n\nPlease find attached the new hospital report card from the Division of Consolidated Laboratory Services. If your hospital had any diagnoses for the given period, you will also find a diagnosis report attached.\n\nSincerely,\nDCLS"
 
message <- ""
 
#########
 
# SET COMPUTER TYPE
# Enter 'PC' or 'MAC'
 
comp_type <- 'MAC'
 
#########
 
# SET WORKING DIRECTORY
# This should be where you have all of your R code files stored
 
wd <- "/mnt/cpp4f/NBS_Report_Card/report_card_r_files"
 
#########
 
# CODES FILE PATH
# This should be your directory (e.g., the location on your
# computer) where you have all supporting csv files. Required
# at this location are five files:
#   -- diagnosis_descriptions.csv - narratives for each diagnosis for use in diagnosis reporting
#   -- hospital_emails.csv - email addresses for each hospital for sending reports
#   -- hospital_messages.csv - individual messages to hospitals to be included in their reports
#   -- unsat_codes.csv - descriptions for each unsatisfactory code
#   -- VA NBS Report Card Hospital Names.csv - hospital codes and names as we want them to appear
#             in their reports
 
codes_path <- "/mnt/cpp4f/NBS_Report_Card/submitter_and_unsat_codes"
 
#########
 
# REPORT FOLDER LOCATION
# Enter the location on your computer where the reports for sending to
# hospitals are stored
 
hospital_path <- "/mnt/cpp4f/NBS_Report_Card/hospital_reports"
 
#########
 
# IMPORTANT THINGS TO CHECK BEFORE RUNNING THE EMAIL CODE BELOW:
#   1. Change the email_test variable to 'Y' if you wish to test the email.
#   2. Update the subject and message above.
#   3. The folder where you have your hospital reports (set in the hospital_variable)
#      should ONLY have the reports you want to send. If there are older reports with 
#      the hospital's name at this location, the email function will send them with 
#      the email.
#   4. Run all lines of code before these instructions so all variables
#      are correctly set.
 
# *** DO NOT CHANGE THIS CODE ***
 
# SET FILE SEPARATOR
slash <- ifelse(comp_type == 'PC', '\\', '/')
 
# LOAD PACKAGES AND FUNCTIONS
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)
 
# SEND EMAILS
send_emails <- paste0(wd, slash, "email_generator.R")
source(send_emails)
