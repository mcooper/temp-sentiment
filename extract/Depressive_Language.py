import pandas as pd
from tqdm import tqdm
import nltk
from nltk import word_tokenize
from multiprocessing import Pool
import glob
import os

###############################
# Metadata
###############################

N_CORES = 64

SAVE_PATH = '/home/ubuntu/tweets/depressive/'
READ_PATH = '/home/ubuntu/tweets/tweets/'

str_depress_terms = '''addictive alone anxiety appetite attacks bleak depress depressed depression drowsiness episodes fatigue frightened lonely nausea nervousness severe sleep suicidal suicide trapped'''
LST_DEPRESS_TERMS = str_depress_terms.split(' ')
DICT_DEPRESS_TERMS = {LST_DEPRESS_TERMS[i]: 1 for i in range(len(LST_DEPRESS_TERMS))}

###################################
# Define functions
###################################

def CheckDepressTerm(text):
    '''
    Return 1 or 0 for whether input contains any depress term
    '''
    words = nltk.word_tokenize(text)
    for w in words:
        if w in DICT_DEPRESS_TERMS:
            return 1
    return 0

def process_all_by_file(f):
    fn = f.split('/')[-1]
    filename = SAVE_PATH + fn
    try:
        df = pd.read_csv(f, lineterminator='\n')
    #Some have no text data, so drop
    except pd.errors.EmptyDataError:
        pd.DataFrame({}).to_csv(filename, index=False)
        print('empty: {}'.format(f))
        return -1
    tqdm.pandas(desc=filename)
    df = df[df['pure_text'].notna()]
    df = df[['id','pure_text','tweet_created_at']]
    df['depress_term'] = df['pure_text'].progress_map(lambda x:CheckDepressTerm(x))
    df = df.drop(columns=['pure_text'])
    print(filename)
    df.to_csv(filename,index=False)
    return 0

###################################
# Run data
###################################

#Get all not-yet-processed tweet data
tweetfiles = glob.glob(READ_PATH + '*.csv')
donefiles = glob.glob(SAVE_PATH + '*.csv')
donefiles = [tweet.split('/')[-1] for tweet in donefiles]
tweetfiles = [tweet for tweet in tweetfiles if tweet.split('/')[-1] not in donefiles]

pool = Pool(processes=N_CORES)
for f in tweetfiles:
    pool.apply_async(process_all_by_file, args=(f, ))

os.system('/home/ubuntu/telegram "Done with Depressive Language"')
os.system('sudo poweroff')


