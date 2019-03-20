sequence = [line.rstrip('\n') for line in open("move_sequence_order.txt")]
sequence = [int(elem) for elem in sequence]

mail_names = []
with open("mbox") as f:
    for line in f:
        if line.startswith("FROM:"):
            mail_names.append(line[6:])

mail_names = [mail_names[i] for i in sequence]

with open("move_sequence_mb","w+") as f:
    for number in mail_names:
        f.write(str(number))
        f.write("\n")
