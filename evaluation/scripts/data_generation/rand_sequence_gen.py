import numpy as np

sequence = np.random.permutation(1000)
with open("move_sequence_order.txt","w+") as f:
    for number in sequence:
        f.write(str(number))
        f.write("\n")
