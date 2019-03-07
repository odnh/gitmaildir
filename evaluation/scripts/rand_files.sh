seq -w 1 1000 | xargs -n1 -I% sh -c 'dd if=/dev/urandom of=file.% count=1024'
