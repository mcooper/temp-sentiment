import os
import json

os.chdir('/home/ubuntu/tweets')

fs = os.listdir()

total = {}
for f in fs:
    print(f)
    with open(f, 'r') as con:
        l = json.loads(con.read())
    total[f] = len(l)

sum = sum([total[f] for f in total])

os.system('~/telegram.sh "done with ' + str(sum) + ' tweets"')

#144096303 tweets!
