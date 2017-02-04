# load in email addresses for hospitals
emails <- read.csv(paste(codes_path, "/", "hospital_emails.csv", sep=""), stringsAsFactors=FALSE,
                   header=TRUE)
 
# remove rows with no email addresses
emails <- emails[emails$Email != "",]
 
# set email parameters
subject <- "Quarterly Report from DCLS: 7/1/2016 - 9/30/2016"
message <- "Hi,\n\nPlease find attached the new hospital report card from the Division of Consolidated Laboratory Services. If your hospital had any diagnoses for the given period, you will also find a diagnosis report attached.\n\nSincerely,\nDCLS"
 
testing <- "N"
test_email_recipient <- "ellemsee@gmail.com"
 
email_end <- ifelse(testing == 'Y', 1, nrow(emails))
 
for (i in 1:email_end) {
  attachments <- paste(hospital_path, "/",
                       list.files(hospital_path, 
                                  pattern=paste0(emails$Hospital[i], "*")), sep="")
  if(!is.list(attachments) && attachments == hospital_path) next
  recipients <- ifelse(testing == 'Y', test_email_recipient, emails$Email[i])
  sendEmail(recipients, subject, message, attachments)
}
