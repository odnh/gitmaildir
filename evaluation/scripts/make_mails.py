import mailbox
from email.mime.text import MIMEText

m = mailbox.Maildir("~/testMail")
for i in range(1,100000):
    msg = MIMEText("Email Contents here")
    msg['Subject'] = "Subj"
    msg["From"] = "me"
    msg["To"] = "you"
    m.add(msg)
