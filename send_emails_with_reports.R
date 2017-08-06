##### Change the settings below before sending each email #####

# FUNCTIONALITY TEST
# Do you want to run a single email to yourself for testing your message 
# before sending to all hospitals? Enter 'Y' if you want to test the
# email first, and also enter the email address to use for sending the test
# email.

email_test <- "N"
test_email_recipient <- ""

#########

# CHOOSE WHETHER TO SEND THE EMAIL TO HOSPITALS OR TO BIRTHCENTERS
# Enter 'H' for hospitals or 'BC' for birthcenters.

send_to <- "BC"

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

wd <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/Caliper/Added birthcenter/r_files/report_card"

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

codes_path <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/Caliper/Added birthcenter/submitter_and_unsat_codes"

#########

# HOSPITAL REPORT FOLDER LOCATION
# Enter the location on your computer where the reports for sending to
# hospitals are stored

hospital_path <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/Caliper/Added birthcenter/Hospital output"

#########

# BIRTHCENTER REPORT FOLDER LOCATION
# Enter the location on your computer where the reports for sending to
# birthcenters are stored

center_path <- "/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/Caliper/Added birthcenter/Birthcenter output"

#########

# IMPORTANT THINGS TO CHECK BEFORE RUNNING THE EMAIL CODE BELOW:
#   1. Change the email_test variable to 'Y' if you wish to test the email.
#   2. Set the send_to variable to 'H' or 'BC' depending on whether you wish to
#      send emails to hospitals or birthcenters.
#   3. Update the subject and message above.
#   4. The folder where you have your hospital reports (set in the hospital_path variable)
#      or the folder where you have your center reports (set in the center_path variable)
#      should ONLY have the reports you want to send. If there are older reports with 
#      the hospital's name at this location, the email function will send them with 
#      the email.
#   5. Run all lines of code before these instructions so all variables
#      are correctly set.

# *** DO NOT CHANGE THIS CODE ***

# SET FILE SEPARATOR
slash <- ifelse(comp_type == 'PC', '\\', '/')

# SEND EMAILS
send_emails <- paste0(wd, slash, "email_generator.R")
source(send_emails)
