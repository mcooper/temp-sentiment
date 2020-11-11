import pandas as pd
import numpy as np
from scipy.sparse import csr_matrix
from scipy.sparse import csc_matrix
from scipy.sparse import hstack
from scipy.sparse import linalg
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import OneHotEncoder

#Model run name
MOD_RUN = 'weather'

#Output directory
OUT_DIR = '/home/mattcoop/tweets/mod-results/'

data = pd.read_csv('~/tweets/all_samp_1pct.csv')

#Remove columns we are not using right now
data = data.drop(columns= ['temp', 'weather_term', 'income_percap', 
                            'majority', 'daynum', 'tree', 'impervious'])

#Get outcome var and remove from predictors
y = data[['vader']]
data = data.drop(columns=['vader'])

#Make predictor matrix of outcome vars
pred = pd.DataFrame(np.array([(t, p, s) for t in np.linspace(-10, 40, 40) for p in np.linspace(0, 10, 20) for s in np.linspace(0, 1400, 20)]))
pred.columns = ['temp.hi', 'precip', 'srad']

#Discretize Continuous (predictor) Vars
data['temp.hi_q'] = pd.cut(data['temp.hi'], bins=[-33, 0, 20, 35, 55])
data['precip_q'] = pd.cut(data['precip'], bins=[-0.1, 0.0001, 1, 75])
data['srad_q'] = pd.cut(data['srad'], bins=[-0.1, 0.0001, 400, 1400])

pred['temp.hi_q'] = pd.cut(pred['temp.hi'], bins=[-33, 0, 20, 35, 55])
pred['precip_q'] = pd.cut(pred['precip'], bins=[-0.1, 0.0001, 1, 75])
pred['srad_q'] = pd.cut(pred['srad'], bins=[-0.1, 0.0001, 400, 1400])

#Make segments for predictor vars
for c in data['precip_q'].unique()[1:]:
    data['precip_q' + str(c)] = data['precip']*(data['precip_q'] == c)
    pred['precip_q' + str(c)] = pred['precip']*(pred['precip_q'] == c)

for c in data['temp.hi_q'].unique()[1:]:
    data['temp.hi_q' + str(c)] = data['temp.hi']*(data['temp.hi_q'] == c)
    pred['temp.hi_q' + str(c)] = pred['temp.hi']*(pred['temp.hi_q'] == c)

for c in data['srad_q'].unique()[1:]:
    data['srad_q' + str(c)] = data['srad']*(data['srad_q'] == c)
    pred['srad_q' + str(c)] = pred['srad']*(pred['srad_q'] == c)

#Make Cateogorical Vars, including segments
data['statemonth'] = data.fips.apply(lambda x: str(100000 + x)[1:3]) + data.doy.apply(lambda x: x[:2]) 

cat_var_names = ['dow', 'doy', 'tod', 'fips', 'year', 'statemonth', 'precip_q', 'temp.hi_q', 'srad_q']

cat_vars = data[cat_var_names]
data = data.drop(columns=cat_var_names)
enc = OneHotEncoder(drop='first', sparse=True)
encfit = enc.fit(cat_vars)
cat_names = encfit.get_feature_names(cat_var_names)
cat_vars = encfit.transform(cat_vars)

#Make sure pred vars and in the same order as fit vars
pred = pred[data.columns]
cnt_names = data.columns

#Add control vars to prediction and combined continuous and dummy vars into 
predX = np.hstack([pred.values, np.empty([pred.shape[0], cat_vars.shape[1]])])
data = hstack([csr_matrix(data.values), cat_vars])

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
# X_with_intercept = hstack([csr_matrix(np.ones([data.shape[0], 1])), data])
# var_beta_hat = linalg.inv(csc_matrix(X_with_intercept.T @ X_with_intercept)) * sigma_squared_hat
# for p_ in range(p):
#     standard_error = var_beta_hat[p_, p_] ** 0.5
#     coef_res.at[p_, 'standarderror'] = standard_error

# coef_res.to_csv(OUT_DIR + MOD_RUN + '_coefs.csv', index=False)

















