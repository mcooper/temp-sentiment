from osgeo import gdal,ogr
import struct
import json
from pyproj import Transformer
from multiprocessing import Process, Pool
import os
from datetime import datetime
import pandas as pd

READPATH = '/home/ubuntu/tweets/tweets/'
WRITEPATH = '/home/ubuntu/tweets/landcover/'

read = os.listdir(READPATH)
done = os.listdir(WRITEPATH)

read = [r for r in read if 'json' in r]
done = [d[:10] for d in done]

files = [r for r in read if r[:10] not in done]

proj = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'

transformer = Transformer.from_crs('epsg:4326', proj)

tree_11_file = '/home/ubuntu/tweets/landcover_data/nlcd_2011_treecanopy_2019_08_31.img'
tree_16_file = '/home/ubuntu/tweets/landcover_data/nlcd_2016_treecanopy_2019_08_31.img'
urbn_11_file = '/home/ubuntu/tweets/landcover_data/NLCD_2011_Impervious_L48_20190405.img'
urbn_16_file = '/home/ubuntu/tweets/landcover_data/NLCD_2016_Impervious_L48_20190405.img'
lcov_11_file = '/home/ubuntu/tweets/landcover_data/NLCD_2011_Land_Cover_L48_20190424.img'
lcov_16_file = '/home/ubuntu/tweets/landcover_data/NLCD_2016_Land_Cover_L48_20190424.img'

tree11 = gdal.Open(tree_11_file)
tree16 = gdal.Open(tree_16_file)
urbn11 = gdal.Open(urbn_11_file)
urbn16 = gdal.Open(urbn_16_file)
lcov11 = gdal.Open(lcov_11_file)
lcov16 = gdal.Open(lcov_16_file)

def getRasterData(d, year):
    if 'geo' not in d.keys():
        return({})
    if 'coordinates' not in d['geo'].keys():
        return({})
    if d['geo']['coordinates'] is None:
        return({})
    
    dx,dy=d['geo']['coordinates'][1], d['geo']['coordinates'][0]
    mx,my=transformer.transform(dx,dy) #coord in map units
    
    #Convert from map to pixel coordinates.
    #Only works for geotransforms with no rotation.
    px = int((mx - gt[0]) / gt[1]) #x pixel
    py = int((my - gt[3]) / gt[5]) #y pixel
    
    structval = tree.ReadRaster(px,py,1,1,buf_type=gdal.GDT_UInt16) #Assumes 16 bit int aka 'short'
    treeval = struct.unpack('h' , structval) #use the 'short' format code (2 bytes) not int (4 bytes)
    
    structval = urbn.ReadRaster(px,py,1,1,buf_type=gdal.GDT_UInt16) #Assumes 16 bit int aka 'short'
    urbnval = struct.unpack('h' , structval) #use the 'short' format code (2 bytes) not int (4 bytes)
    
    structval = lcov.ReadRaster(px,py,1,1,buf_type=gdal.GDT_UInt16) #Assumes 16 bit int aka 'short'
    lcovval = struct.unpack('h' , structval) #use the 'short' format code (2 bytes) not int (4 bytes)
   
    rdict = {'id': d['id'] , 'id_str': d['id_str'], 'tweet_created_at': d['tweet_created_at'],
            'tree': treeval[0], 'impervious': urbnval[0], 'landcover': lcovval[0]}
    return(rdict)

def processTweetFiles(filename):
    print(datetime.now(), filename)
    with open(READPATH + filename, 'r') as f:
        dat = json.loads(f.read())
    
    year = int(filename[:4])
    
    if year > 2013:
        tree = tree16
        urbn = urbn16
        lcov = lcov16
    else:
        tree = tree11
        urbn = urbn11
        lcov = lcov11

    gt = tree.GetGeoTransform() #gt is the same for all rasters
    tree = tree.GetRasterBand(1)
    urbn = urbn.GetRasterBand(1)
    lcov = lcov.GetRasterBand(1)
        
    # for d in dat:
    #     getRasterData(d, urbn, tree, lcov, gt)
    #     print(dat.index(d))
  
    dat = [(d, urbn, tree, lcov, gt) for d in dat]

    pool = Pool(processes=20)
    res = pool.map(getRasterData, dat)

    processes = []
    for d in dat:
        proc = Process(target=getRasterData, args=(d, urbn, tree, lcov, gt))
        processes.append(proc)
        proc.start()

    for process in processes:
        process.join()

    
    pd.DataFrame(allres).to_csv(WRITEPATH + filename.replace('json', 'csv'), index=False)




