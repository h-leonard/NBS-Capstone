library(mailR)

# load email addresses for hospitals
emails <- read.csv("/Users/chrispatrick/Documents/Classes/Fall 2016/DS 6001/Newborn Screening/Email/hospital_emails.csv", 
                   stringsAsFactors=FALSE, header=TRUE)

# remove rows with no email addresses
emails <- emails[emails$Email != "",]

sender <- "newbornscreeninguva@gmail.com"
password <- "O8@H$0P3T4N9gGwRt7Qo"
subject <- "Quarterly Report from DCLS: 7/1/2016 - 9/30/2016"
message <- "Hi,\n\nPlease find attached the new hospital report card from the Division of Consolidated Laboratory Services. If your hospital had any diagnoses for the given period, you will also find a diagnosis report attached.\n\nSincerely,\nDCLS"

testing <- "N"
test_email_recipient <- "ellemsee@gmail.com"

email_end <- ifelse(testing == 'Y', 1, nrow(emails))

for (i in 1:email_end) {
  attachments <- paste("/Users/chrispatrick/Desktop/", 
                       list.files("/Users/chrispatrick/Desktop/", 
                                  pattern=paste(emails$Hospital[i], "*", sep="")), sep="")
  recipients <- ifelse(testing == 'Y', test_email_recipient, unlist(strsplit(emails$Email[i], ",")))
  send.mail(from=sender, 
            to=recipients,
            subject=subject,
            body=message,
            smtp=list(host.name="smtp.gmail.com", 
                      port=465,                            
                      user.name=sender,
                      passwd=password,
                      ssl=TRUE),
            authenticate=TRUE,
            send=TRUE,
            attach.files=attachments)
}
