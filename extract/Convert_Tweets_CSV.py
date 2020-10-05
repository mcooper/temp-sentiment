import os
import json
import pandas as pd
import multiprocessing as mp
from multiprocessing import Pool

os.chdir('/home/ubuntu/tweets/tweets')

#All files
fs = os.listdir()

jsons = [f for f in fs if 'csv' not in f]
csvs = [f for f in fs if 'json' not in f]

fs = [f for f in jsons if (f[:10] + '.csv') not in csvs]

fs.sort()

def convert(f):
    print(f)
    with open(f, 'r') as file:
        d = json.loads(file.read())
    new = []
    for i in d:
        if 'geo' in i.keys():
            #Flatten geo vars
            i.update({'lat': i['geo']['coordinates'][1]})
            i.update({'lon': i['geo']['coordinates'][0]})
            del i['geo']
        if 'user' in i.keys():
            del i['user']
        new.append(i)
    newdf = pd.DataFrame(new)
    newdf.to_csv(f.replace('json', 'csv'), index=False)

p = Pool()
p.map(convert, fs)

