import mailbox
import random
import string
from email.mime.text import MIMEText

def random_string(string_length):
    letters = string.ascii_letters
    return ''.join(random.choice(letters) for i in range(string_length))

data_dir = "data"

for i in range(1000):
    msg = MIMEText(random_string(75000))
    msg['Subject'] = "subject"
    msg["From"] = "me"
    msg["To"] = "you"
    with open(data_dir+"/file."+str(i).zfill(4), "w+") as f:
        f.write(msg.as_string())
        f.write("\n")
