import random
import string
from email.mime.text import MIMEText

def gen_rand_data(n):
    return ''.join(random.choices(string.ascii_uppercase + string.digits, k=n))

data_dir = "data_75kB"
for i in range(10000):
    msg = MIMEText(gen_rand_data(75000))
    msg['Subject'] = "Subject"
    msg["From"] = "me"
    msg["To"] = "you"
    with open(data_dir + "/file." + str(i).zfill(5), "w+") as f:
        f.write(msg.as_string())
        f.write("\n")
