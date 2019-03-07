from random import choice
from string import ascii_uppercase

with open('test_strings.txt', 'w') as f:
    for i in range(1000):
        data = ''.join(choice(ascii_uppercase) for i in range(100))
        f.write(data + "\n")
