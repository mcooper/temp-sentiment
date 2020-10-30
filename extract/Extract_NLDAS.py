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
    fs = os.listdir('/home/ubuntu/tweets/tweets/')
    fs = [f for f in fs if 'json' in f]
    
    done = os.listdir('/home/ubuntu/tweets/hourly_nldas/')
    
    done = [d[:10] for d in done]
    fs = [f for f in fs if f[:10] not in done]
    
    fs.sort(reverse=True)
   
    return(fs)

def sh2rh(sh, temp, press):
    #Adapted from https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity
    press = press * 0.01 #convert milibars to pascals
    es = 6.112 * math.exp((17.67*temp)/(temp + 243.5))
    e = sh*press / (0.378 * sh + 0.622)
    rh = e/es
    if rh > 1:
        return(1)
    if rh < 0:
        return(0)
    return(rh)

def heatindex(t, rh):
    #Translated from R weathermetrics::heat.index.algorithm
    #For more info: https://doi.org/10.1016/j.envres.2018.09.032
    
    #Convert to fahrenheit
    t = t*(9/5) + 32
    
    #Convert rh to percent
    rh = rh*100
    
    if t <= 40:
        hi = t
    else:
        alpha = 61 + ((t - 68) * 1.2) + (rh * 0.094)
        hi = 0.5 * (alpha + t)
        if hi > 79:
            hi = -42.379 + 2.04901523 * t + 10.14333127 * rh - 0.22475541 * t * rh - 6.83783 * 10**-3 * t**2 - 5.481717 * 10**-2 * rh**2 + 1.22874 * 10**-3 * t**2 * rh + 8.5282 * 10**-4 * t * rh**2 - 1.99 * 10**-6 * t**2 * rh**2
            if (rh <= 13) & (t >= 80) & (t <= 112):
                adjustment1 = (13 - rh)/4
                adjustment2 = math.sqrt((17 - abs(t - 95))/17)
                totaladjustment = adjustment1 * adjustment2
                hi = hi - totaladjustment
            elif (rh > 85) & (t >= 80) & (t <= 87):
                adjustment1 = (rh - 85)/10
                adjustment2 = (87 - t)/5
                totaladjustment = adjustment1 * adjustment2
                hi = hi + totaladjustment
    
    #Convert back to Celsius
    hi = (hi - 32)*(5/9)
    return(hi)

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
    with open('/home/ubuntu/tweets/tweets/' + f, 'r') as c:
        dat = json.loads(c.read())
     
    dat = [{'lat': d['geo']['coordinates'][1],
            'lon': d['geo']['coordinates'][0],
            'id': d['id'],
            'tweet_created_at': d['tweet_created_at'],
            'hour': d['tweet_created_at'][:13]} for d in dat]
    dat = pd.DataFrame(dat).drop_duplicates()
    
    if dat.shape[0] == 0:
        #Create empty file to avoid attempting that day again
        os.system('touch ~/tweets/hourly_nldas/' + outf)
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
        
        res = reduce(lambda x, y: pd.merge(x, y, on=['tweet_created_at', 'id']), 
                [temp_res, speh_res, pres_res, prcp_res, srad_res])
        daydat = pd.concat([daydat, res])
        
    daydat['relh'] = daydat.apply(lambda x: sh2rh(x['speh'], x['temp'], x['pres']), axis=1)
    daydat['hi'] = daydat.apply(lambda x: heatindex(x['temp'], x['relh']), axis=1)
    
    #Clean up extrememly long floating point numbers
    daydat['temp'] = round(daydat['temp'], 2)
    daydat['hi'] = round(daydat['hi'], 2)
    
    #Drop unnecessary meteo columns
    daydat = daydat.drop(columns=['speh', 'pres', 'relh'])
   
    daydat.to_csv('/home/ubuntu/tweets/hourly_nldas/' + outf, index=False)
    sleeptime.sleep(10)

os.system('/home/ubuntu/telegram.sh "NLDAS Failed"')




