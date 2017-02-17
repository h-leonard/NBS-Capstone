# load in email addresses for hospitals
emails <- read.csv(paste(codes_path, slash, "hospital_emails.csv", sep=""), stringsAsFactors=FALSE,
                   header=TRUE)
 
# remove rows with no email addresses
emails <- emails[emails$Email != "",]
 
# set 'email_end' to 1 if user is testing email functionality
# so only a single email is sent, which will have the report
# for the first hospital in the list (organized alphabetically)
email_end <- ifelse(email_test == 'Y', 1, nrow(emails))
 
# loop through hospitals, adding attachments that match their names,
# and send email to each set of receipients for the hospital
for (i in 1:email_end) {
  attachments <- paste(hospital_path, slash, list.files(hospital_path, 
                                  pattern=paste0(emails$Hospital[i], "*")), sep="")
  if(!is.list(attachments) && attachments == hospital_path) next
  recipients <- ifelse(email_test == 'Y', test_email_recipient, emails$Email[i])
  sendEmail(recipients, subject, message, attachments)
}
