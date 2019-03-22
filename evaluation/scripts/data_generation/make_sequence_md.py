import os

sequence = [line.rstrip('\n') for line in open("move_sequence_order.txt")]
sequence = [int(elem) for elem in sequence]

mail_names = os.listdir("md_data/new")

mail_names = [mail_names[i] for i in sequence]

with open("move_sequence_md","w+") as f:
    for number in mail_names:
        f.write(str(number))
        f.write("\n")
