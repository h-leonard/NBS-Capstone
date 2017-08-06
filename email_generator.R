# LOAD PACKAGES AND FUNCTIONS
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)

# If user has entered something other than "H" or "BC" for 
# send_to, stop running the file and report an error
if (send_to != "H" & send_to != "BC") {
  stop(sprintf("You entered '%s' for the send_to variable. Please change the value for this variable to either 'H' to send emails to hospitals or 'BC' to send emails to birthcenters.",
               send_to))
}

# load in email addresses
emails <- read.csv(paste(codes_path, slash, "organization_emails.csv", sep=""), 
                   stringsAsFactors=FALSE, header=TRUE)

# get unique submitters from submitters dataframe
sub_unique <- unique(submitters[, 2:3])

# get type for each organization in hospital_emails
emails <- suppressWarnings(left_join(emails, sub_unique, c("Name" = "HOSPITALREPORT")))

# select addresses of interest depending on whether user has entered
# 'H' or 'BC' for the send_to variable, also set path for location reports
# for sending
if (send_to == "H") {
  emails <- emails[emails$TYPE == "Hospital", ]
  path <- hospital_path
} else {
  emails <- emails[emails$TYPE == "BirthCenter", ]
  path <- center_path
}


# remove rows with no email addresses
emails <- emails[emails$Email != "",]

# set 'email_end' to 1 if user is testing email functionality
# so only a single email is sent, which will have the report
# for the first hospital in the list (organized alphabetically)
email_end <- ifelse(email_test == 'Y', 1, nrow(emails))

# request email information from user
emailInfo <- requestEmail()

# loop through organizations, adding attachments that match their names,
# and send email to each set of receipients for the organization
for (i in 1:email_end) {
  
  # Get list of attachments for each organization, and send email if any attachments exist
  attachments <- paste(path, slash, list.files(path, pattern=paste0(emails$Name[i], "*")), 
                       sep="")
  if(!is.list(attachments) &&!grepl("pdf", attachments)) next
  
  # Get list of recipients for each organization
  recipients <- unlist(ifelse(email_test == 'Y', 
                              test_email_recipient, strsplit(emails$Email[i], ";")))
  
  # If password is provided, send email with authentication
  if (length(emailInfo) == 2) {
    send.mail(from = emailInfo[[1]],
              to = recipients,
              subject = subject,
              body = message,
              smtp = list(host.name = "csmtp.cov.virginia.gov", 
                          port = 25,
                          user.name = emailInfo[[1]],           
                          passwd = emailInfo[[2]]),            
              authenticate = TRUE,
              attach.files = attachments,
              send = TRUE)
    
    # Otherwise, do not require authentication  
  } else {
    send.mail(from = emailInfo[[1]],
              to = recipients,
              subject = subject,
              body = message,
              smtp = list(host.name = "csmtp.cov.virginia.gov", 
                          port = 25,
                          user.name = emailInfo[[1]]),
              authenticate = FALSE,
              attach.files = attachments,
              send = TRUE)
  }
}

# Remove user's email address and password from environment
rm(emailInfo)
