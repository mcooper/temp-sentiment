#Run in python3.6

import pandas as pd
import numpy as np
from scipy import sparse
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import OneHotEncoder
import getpass

#Model run name
MOD_RUN = 'weather_income_test3'

INTERACT_VAR = 'income_percap'

### Local ###
if getpass.getuser() == 'mattcoop':
    OUT_DIR = '/home/mattcoop/tweets/mod-res/'
    data = pd.read_csv('~/tweets/all_samp_1pct.csv')
    savedat = False

##### Cloud #####
if getpass.getuser() == 'ubuntu':
    OUT_DIR = '/home/ubuntu/tweets/mod-res/'
    data = pd.read_csv('~/tweets/all.csv')
    data.to_hdf('/home/ubuntu/tweets/data.h5', 'data')
    savedat = True

#Remove columns we are not using right now
data = data.drop(columns= ['temp', 'weather_term',
                            'majority', 'daynum', 'tree', 'impervious'])

#Get outcome var and remove from predictors
y = data[['vader']]
data = data.drop(columns=['vader'])

#Get categories of interact var
cat = np.sort(data[INTERACT_VAR].unique())

#Set knots
knots = {'temp.hi': [-20, -10, 0, 5, 10, 15, 20, 25, 30, 40],
         'precip': [0.001, 1, 10, 30],
         'srad': [0.1, 200, 500, 800]}

#Make predictor matrix with ranges one beyond the knots and with segments of length 10 in the knots
def make_range(l):
    l = [l[0] - 1] + l + [l[-1] + 1]
    r = []
    for i in range(1, len(l)):
        r = r + np.linspace(l[i-1], l[i], 10, endpoint=False).tolist()
    return(r)

pred = pd.concat([pd.DataFrame({'temp.hi': make_range(knots['temp.hi'])}),
           pd.DataFrame({'precip': make_range(knots['precip'])}),
           pd.DataFrame({'srad': make_range(knots['srad'])})]).fillna(0)

pred = pd.concat([pred, pred, pred])

pred[INTERACT_VAR] = np.repeat(data[INTERACT_VAR].unique(), pred.shape[0]/3)


#Make continuous segments for predictor vars
for grp in cat[1:]:
    for c in knots['precip']:
        data['precip_' + grp + '_' + str(c)] = (np.maximum(0, data['precip'] - c))*(data[INTERACT_VAR] == grp)
        pred['precip_' + grp + '_' + str(c)] = (np.maximum(0, pred['precip'] - c))*(pred[INTERACT_VAR] == grp)
    for c in knots['temp.hi']:
        data['temp.hi_' + grp + '_' + str(c)] = (np.maximum(0, data['temp.hi'] - c))*(data[INTERACT_VAR] == grp)
        pred['temp.hi_' + grp + '_' + str(c)] = (np.maximum(0, pred['temp.hi'] - c))*(pred[INTERACT_VAR] == grp)
    for c in knots['srad']:
        data['srad_' + grp + '_' + str(c)] = (np.maximum(0, data['srad'] - c))*(data[INTERACT_VAR] == grp)
        pred['srad_' + grp + '_' + str(c)] = (np.maximum(0, pred['srad'] - c))*(pred[INTERACT_VAR] == grp)


#Make Cateogorical Vars for Control Variables
data['statemonth'] = data.fips.apply(lambda x: str(100000 + x)[1:3]) + data.doy.apply(lambda x: x[:2]) 

cat_var_names = ['dow', 'doy', 'tod', 'fips', 'year', 'statemonth']

cat_vars = data[cat_var_names]
data = data.drop(columns=cat_var_names)

enc = OneHotEncoder(drop='first', sparse=True)
enccat = enc.fit(cat_vars)
cat_names = enccat.get_feature_names(cat_var_names)
cat_vars = enccat.transform(cat_vars)

#Make Categorical Vars for interactive effect
enc = OneHotEncoder(drop='first', sparse=False)
enccnt = enc.fit(pred.loc[: ,[INTERACT_VAR]])

pred = pd.concat([pred, 
        pd.DataFrame(enccnt.transform(pred.loc[: ,[INTERACT_VAR]]), 
            columns = cat[1:])], axis=0)

data = pd.concat([data, 
        pd.DataFrame(enccnt.transform(data.loc[: ,[INTERACT_VAR]]), 
            columns = cat[1:])], axis=0)

#Make sure pred vars and in the same order as fit vars
pred = pred[data.columns]
cnt_names = data.columns

#Add control vars to prediction and combined continuous and dummy vars into 
predX = np.hstack([pred.values, np.full([pred.shape[0], cat_vars.shape[1]], 0)])
data = sparse.hstack([sparse.csr_matrix(data.values), cat_vars])

#Save sparse matrix as scipy.sparse.npz object
# if savedat:
#    sparse.save_npz(OUT_DIR + MOD_RUN + '_data.npz', data)

#Now run regression
mod = LinearRegression().fit(data, y)
coef_res = pd.DataFrame({'names': ['intercept'] + cnt_names.tolist() + cat_names.tolist(),
                        'coefs': mod.intercept_.tolist() + mod.coef_.tolist()[0]})
pred['predicted'] = mod.predict(predX)

#Write results
coef_res.to_csv(OUT_DIR + MOD_RUN + '_coefs.csv', index=False)
pred.to_csv(OUT_DIR + MOD_RUN + '_preds.csv', index=False)

#Get standard errors
#https://stackoverflow.com/questions/22381497/python-scikit-learn-linear-model-parameter-standard-error

# Getting Singularity error
# Might work with full data - try in the cloud
# y_hat = mod.predict(data)
# residuals = y - y_hat
# N = data.shape[0]
# p = data.shape[1] + 1
# residual_sum_of_squares = residuals.T @ residuals
# sigma_squared_hat = residual_sum_of_squares.iloc[0][0] / (N - p)
# X_with_intercept = sparse.hstack([sparse.csr_matrix(np.ones([data.shape[0], 1])), data])
# var_beta_hat = sparse.linalg.inv(sparse.csc_matrix(X_with_intercept.T @ X_with_intercept)) * sigma_squared_hat
# 
# diag = var_beta_hat.diagonal()
# 
# coef_res['standarderror'] = diag ** 0.5
# 
# coef_res.to_csv(OUT_DIR + MOD_RUN + '_coefs.csv', index=False)
# 
# import statsmodels.api as sm
# ols = sm.OLS(y, X_with_intercept.toarray())
# ols_result = ols.fit()
# ols_result.summary()















