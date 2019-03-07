import time
import socket
import random
from subprocess import run, PIPE

test_dir = '"/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/gtspeed"'

def test_git():
    with open('test_strings.txt') as f:
        for line in f:
            p = run(['gitmaildir_cli', 'deliver', '--dir='+test_dir], stdout=PIPE, input=line, encoding='ascii')

def test_non_git():
    with open('test_strings.txt') as f:
        for line in f:
            filename = str(int(time.time() * 1000000)) + "." + str(random.random() * 1000000000) + "." + socket.gethostname()
            mail_file = open('gtspeed/'+filename, 'w')
            mail_file.write(line)
            mail_file.close

test_git()
