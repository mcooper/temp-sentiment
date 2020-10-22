#source $HOME/miniconda3/bin/activate
#conda activate ee

import json
import ee
import os
from datetime import datetime, timedelta, time
import pandas as pd
import numpy as np
import time as sleeptime #otherwise interferes with datetime.time
import progressbar

ee.Initialize()

os.chdir('/home/ubuntu/tweets')

fs = sorted(os.listdir())
fs = [f for f in fs if f[-4:] == 'json']

def parse_res(res, val, to_round):
    if to_round:
        return [round(x['properties'][val], 2) if x['properties']['ppt'] is not None else np.nan for x in res['features'] ]
    else:
        return [x['properties'][val] if x['properties']['ppt'] is not None else np.nan for x in res['features'] ]

for f in fs:
    print(datetime.now(), f)
    #From the docs: The dataset uses a day-ending naming convention, e.g., a day ending at 1200 UTC on 1 January is labeled 1 January. 
    today = datetime.strptime(f[:10], '%Y-%m-%d')
    name = today + timedelta(days=1)
    
    #Separate climate data for whether tweet was before or after 12 UTC
    after12 = ee.Image("OREGONSTATE/PRISM/AN81d/" + name.strftime("%Y%m%d"))
    before12 = ee.Image("OREGONSTATE/PRISM/AN81d/" + today.strftime("%Y%m%d"))
    with open(f, 'r') as c:
        dat = json.loads(c.read())
    
    if len(dat) == 0:
        print('passing')
        continue
    
    #Get collections of points, separated by morning/afternoon
    morning_points = []
    afternoon_points = []
    for d in dat:
        lon = d['geo']['coordinates'][0]
        lat = d['geo']['coordinates'][1]
        created_at = datetime.strptime(d['tweet_created_at'], '%Y-%m-%d %H:%M:%S')
        is_morning = created_at.time() < time(12, 00)
        pt = ee.Geometry.Point(lon, lat)
        feat = ee.Feature(pt, {'tweet_created_at': d['tweet_created_at'], 'id': str(d['id'])})
        if is_morning: 
            morning_points.append(feat)
        else:
            afternoon_points.append(feat)
     
    #Make points in batches of 800 points so no query is too large
    morning_feats = []
    i = 0
    while i < len(morning_points):
        j = i + 1000
        fc = ee.FeatureCollection(morning_points[i:j])
        morning_feats.append(fc)
        i = j
    
    afternoon_feats = []
    i = 0
    while i < len(afternoon_points):
        j = i + 1000
        fc = ee.FeatureCollection(afternoon_points[i:j])
        afternoon_feats.append(fc)
        i = j
    
    #Collect data results for morning
    resdf = pd.DataFrame({})
    for feat in progressbar.progressbar(morning_feats):
        res = before12.reduceRegions(feat, reducer=ee.Reducer.first()).getInfo()
        id = parse_res(res, 'id', False)
        tweet_created_at = parse_res(res, 'tweet_created_at', False)
        ppt = parse_res(res, 'ppt', True)
        tdmean = parse_res(res, 'tdmean', True)
        tmax = parse_res(res, 'tmax', True)
        tmin = parse_res(res, 'tmin', True)
        tmean = parse_res(res, 'tmean', True)
        vpdmin = parse_res(res, 'vpdmin', True)
        vpdmax = parse_res(res, 'vpdmax', True)
        df = pd.DataFrame({'id': id, 'ppt': ppt, 'tdmean': tdmean, 'tmax': tmax, 'tmin': tmin,
            'tmean': tmean, 'vpdmin': vpdmin, 'vpdmax': vpdmax, 'tweet_created_at': tweet_created_at})
        resdf = pd.concat([resdf, df])
    
    #Do the same for afternoon
    for feat in progressbar.progressbar(afternoon_feats):
        res = after12.reduceRegions(feat, reducer=ee.Reducer.first()).getInfo()
        id = parse_res(res, 'id', False)
        tweet_created_at = parse_res(res, 'tweet_created_at', False)
        ppt = parse_res(res, 'ppt', True)
        tdmean = parse_res(res, 'tdmean', True)
        tmax = parse_res(res, 'tmax', True)
        tmin = parse_res(res, 'tmin', True)
        tmean = parse_res(res, 'tmean', True)
        vpdmin = parse_res(res, 'vpdmin', True)
        vpdmax = parse_res(res, 'vpdmax', True)
        df = pd.DataFrame({'id': id, 'ppt': ppt, 'tdmean': tdmean, 'tmax': tmax, 'tmin': tmin,
            'tmean': tmean, 'vpdmin': vpdmin, 'vpdmax': vpdmax, 'tweet_created_at': tweet_created_at})
        resdf = pd.concat([resdf, df])
    
    resdf.to_csv('/home/ubuntu/tweets/temps/' + f[:10] + '.csv', index=False)
    sleeptime.sleep(10)


