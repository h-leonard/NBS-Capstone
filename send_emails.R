# load in email addresses for hospitals
emails <- read.csv(paste(codes_path, "/", "hospital_emails.csv", sep=""), stringsAsFactors=FALSE,
                   header=TRUE)
 
# remove rows with no email addresses
emails <- emails[emails$Email != "",]
 
# set email parameters
sender <- **ADD EMAIL HERE**
password <- **ADD PASSWORD HERE**
subject <- "Quarterly Report from DCLS: 7/1/2016 - 9/30/2016"
message <- "Hi,\n\nPlease find attached the new hospital report card from the Division of Consolidated Laboratory Services. If your hospital had any diagnoses for the given period, you will also find a diagnosis report attached.\n\nSincerely,\nDCLS"
 
for (i in 1:nrow(emails)) {
  attachments <- paste(output_path, "/",
                       list.files(output_path, 
                                  pattern=paste(emails$Hospital[i], "*", sep="")), sep="")
  recipients <- unlist(strsplit(emails$Email[i], ","))
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
