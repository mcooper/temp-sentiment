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

os.chdir('/home/ubuntu/tweets/tweets')

fs = sorted(os.listdir())
fs = [f for f in fs if 'json' in f]

already_done = os.listdir('/home/ubuntu/tweets/landcover')
already_done = [a[:10] for a in already_done]

fs = [f for f in fs if f[:10] not in already_done]

def parse_res(res, val, to_round):
    if to_round:
        return [round(x['properties'][val], 2) if x['properties']['landcover'] is not None else np.nan for x in res['features']]
    else:
        return [x['properties'][val] if x['properties']['landcover'] is not None else np.nan for x in res['features'] ]

for f in fs:
    print(datetime.now(), f)
    #From the docs: The dataset uses a day-ending naming convention, e.g., a day ending at 1200 UTC on 1 January is labeled 1 January. 
    today = datetime.strptime(f[:10], '%Y-%m-%d')
    name = today + timedelta(days=1)
    
    #Separate climate data for whether tweet was before or after 12 UTC
    nlcd2011 = ee.Image("USGS/NLCD/NLCD2011")
    nlcd2016 = ee.Image("USGS/NLCD/NLCD2016")
    #after12 = ee.Image("OREGONSTATE/PRISM/AN81d/" + name.strftime("%Y%m%d"))
    #before12 = ee.Image("OREGONSTATE/PRISM/AN81d/" + today.strftime("%Y%m%d"))
    with open(f, 'r') as c:
        dat = json.loads(c.read())
    
    if len(dat) == 0:
        print('passing')
        continue
    
    #Get collections of points, separated by year before 2014 and year after 2014 
    before_points = []
    after_points = []
    for d in dat:
        lon = d['geo']['coordinates'][0]
        lat = d['geo']['coordinates'][1]
        created_at = datetime.strptime(d['tweet_created_at'], '%Y-%m-%d %H:%M:%S')
        is_early = (created_at.year < 2014)
        pt = ee.Geometry.Point(lon, lat)
        feat = ee.Feature(pt, {'tweet_created_at': d['tweet_created_at'], 'id': str(d['id'])})
        if is_early:
            before_points.append(feat)
        else:
            after_points.append(feat)
     
    #Make points in batches of [batch=1000] points so no query is too large
    before_feats = []
    batch = 1000
    i = 0
    while i < len(before_points):
        j = i + batch
        fc = ee.FeatureCollection(before_points[i:j])
        before_feats.append(fc)
        i = j
    
    after_feats = []
    i = 0
    while i < len(after_points):
        j = i + batch
        fc = ee.FeatureCollection(after_points[i:j])
        after_feats.append(fc)
        i = j
    
    #Collect data results for before 2014
    resdf = pd.DataFrame({})
    for feat in before_feats:
        res = nlcd2011.reduceRegions(feat, reducer=ee.Reducer.first()).getInfo()
        id = parse_res(res, 'id', False)
        tweet_created_at = parse_res(res, 'tweet_created_at', False)
        landcover = parse_res(res,'landcover',True)
        impervious = parse_res(res, 'impervious',True)
        tree_cover = parse_res(res, 'percent_tree_cover',True)
        df = pd.DataFrame({'id': id, 'landcover': landcover, 'impervious': impervious, 'tree': tree_cover,
             'tweet_created_at': tweet_created_at})
        resdf = pd.concat([resdf, df])
    
    #Do the same for after 2016
    for feat in after_feats:
        res = nlcd2011.reduceRegions(feat, reducer=ee.Reducer.first()).getInfo()
        id = parse_res(res, 'id', False)
        tweet_created_at = parse_res(res, 'tweet_created_at', False)
        landcover = parse_res(res,'landcover',True)
        impervious = parse_res(res, 'impervious',True)
        tree_cover = parse_res(res, 'percent_tree_cover',True)
        df = pd.DataFrame({'id': id, 'landcover': landcover, 'impervious': impervious, 'tree': tree_cover,
             'tweet_created_at': tweet_created_at})
        resdf = pd.concat([resdf, df])
    
    resdf.to_csv('/home/ubuntu/tweets/landcover/' + f[:10] + '.csv', index=False)
    sleeptime.sleep(10)

os.system('~/telegram.sh "Check EE Scripts"')



