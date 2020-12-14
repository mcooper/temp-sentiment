# Run on separate server!
# Everything must be SSHed over!

import json
import itertools
import ee
import os
from datetime import datetime, timedelta, time
import pandas as pd
import numpy as np
import time as sleeptime #otherwise interferes with datetime.time
from functools import reduce
import math

ee.Initialize()

os.chdir('/home/ubuntu/')

def getFiles():
    #Get list of all files that have not yet been processed
    os.system('ssh ubuntu@sesync-tweets ls ~/tweets/tweets/ | grep json > ~/existing')
    with open('/home/ubuntu/existing') as f:
        fs = f.read().splitlines()
    
    os.system('ssh ubuntu@sesync-tweets ls ~/tweets/hourly_nldas2/  > ~/done')
    with open('/home/ubuntu/done') as f:
        done = f.read().splitlines()
    
    done = [d[:10] for d in done]
    fs = [f for f in fs if f[:10] not in done]
    
    return(fs)

def chunker(iter, size):
    chunks = [];
    if size < 1:
        raise ValueError('Chunk size must be greater than 0.')
    for i in range(0, len(iter), size):
        chunks.append(iter[i:(i+size)])
    return chunks

def getData(feats, img, var):
    #First, break feats into lists of lists, each with len < 4500
    feats = chunker(feats, 4500)
    
    allres = []
    for feat in feats:
        res = img.reduceRegions(ee.FeatureCollection(feat), reducer=ee.Reducer.first()).getInfo()
        allres = allres + res['features']
    
    df = pd.DataFrame([x['properties'] for x in allres])
    df = df.rename(columns={'first': var})
    return(df)


for f in getFiles():  
    print(datetime.now(), f)
    outf = f[:10] + '.csv'
    
    #Read in data
    os.system('scp sesync-tweets:~/tweets/tweets/' + f + ' ~/')
    with open('/home/ubuntu/' + f, 'r') as c:
        dat = json.loads(c.read())
    os.system('rm ' + f)
     
    dat = [{'lat': d['geo']['coordinates'][1],
            'lon': d['geo']['coordinates'][0],
            'id': d['id'],
            'tweet_created_at': d['tweet_created_at'],
            'hour': d['tweet_created_at'][:13]} for d in dat]
    dat = pd.DataFrame(dat).drop_duplicates()
    
    if dat.shape[0] == 0:
        #Create empty file to avoid attempting that day again
        os.system('ssh ubuntu@sesync-tweets touch ~/tweets/hourly_nldas2/' + outf)
        print("Skipping " + f)
        continue
    
    daydat = pd.DataFrame()
    for hour in dat.hour.unique().tolist():
        sel = dat[dat.hour == hour] 
        img = ee.Image('NASA/NLDAS/FORA0125_H002/A' + f[:4] + f[5:7] + f[8:10] + '_' + hour[11:13] + '00')
        temp = img.select('temperature')
        speh = img.select('specific_humidity')
        pres = img.select('pressure')
        prcp = img.select('total_precipitation')
        srad = img.select('shortwave_radiation')
        lrad = img.select('longwave_radiation')
        wndu = img.select('wind_u')
        wndv = img.select('wind_v')
         
        feats = []
        for i,v in sel.iterrows():
            lon = v['lon']
            lat = v['lat']
            pt = ee.Geometry.Point(lon, lat)
            feat = ee.Feature(pt, {'tweet_created_at': v['tweet_created_at'], 'id': str(v['id'])})
            feats.append(feat)
         
        temp_res = getData(feats, temp, 'temp')
        speh_res = getData(feats, speh, 'speh')
        pres_res = getData(feats, pres, 'pres')
        prcp_res = getData(feats, prcp, 'prcp')
        srad_res = getData(feats, srad, 'srad')
        lrad_res = getData(feats, lrad, 'lrad')
        wndu_res = getData(feats, wndu, 'wndu')
        wndv_res = getData(feats, wndv, 'wndv')
        
        res = reduce(lambda x, y: pd.merge(x, y, on=['tweet_created_at', 'id']), 
                [temp_res, speh_res, pres_res, prcp_res, srad_res, lrad_res, wndu_res, wndv_res])
        daydat = pd.concat([daydat, res])
        daydat = pd.concat([daydat, res])
        
    daydat.to_csv(outf, index=False)
    os.system('scp /home/ubuntu/' + outf + ' sesync-tweets:~/tweets/hourly_nldas2/' + outf)
    os.system('rm /home/ubuntu/' + outf) 
    sleeptime.sleep(10)

os.system('/home/ubuntu/telegram.sh "NLDAS Failed"')




