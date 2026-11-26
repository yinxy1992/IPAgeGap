import numpy as np
from sklearn.linear_model import Ridge, Lasso
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.svm import SVR
from xgboost import XGBRegressor
from sklearn.neural_network import MLPRegressor
import pandas as pd
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import r2_score, mean_absolute_error
import pickle
import matplotlib
import warnings
import os


train_data = pd.read_csv('./xiehe_all_train_data_20260312.csv',encoding='gb18030')
test_data = pd.read_csv('./xiehe_all_test_data_20260312.csv',encoding='gb18030')


X_train = train_data.iloc[:,1:]
Y_train = train_data.iloc[:,0]
X_test = test_data.iloc[:,1:]
Y_test = test_data.iloc[:,0]

GB_model = GradientBoostingRegressor(random_state=42)
# 设置参数网格
param_grid_0 = {
    'n_estimators': [100, 150, 200],  
    'learning_rate': [0.01, 0.1, 0.2],  
    'max_depth': [3, 5, 7, 10, 15],  
    'min_samples_split': [10, 15, 20, 25],  
    'min_samples_leaf': [8, 10, 12, 15],  
    'subsample': [0.8, 0.9, 1.0]  
}
grid_search = GridSearchCV(estimator=GB_model, param_grid=param_grid_0, cv=5, verbose=2, n_jobs=max(1, int(os.cpu_count() * 0.75)))

grid_search.fit(X_train, Y_train)
print("Best parameters found: ", grid_search.best_params_)
model = grid_search.best_estimator_
with open('GradientBoosting_xiehe_data_new_all_model.pkl', 'wb') as f:
    pickle.dump(model, f)

pred_train = model.predict(X_train)
mae = mean_absolute_error(Y_train, pred_train)
print("grid_search CV GradientBoosting MAE of xiehe_data_new_all train: ",mae)
r2 = r2_score(Y_train, pred_train)
print("grid_search CV GradientBoosting R^2 of xiehe_data_new_all train: ",r2)

pred_test = model.predict(X_test)
mae = mean_absolute_error(Y_test, pred_test)
print("grid_search CV GradientBoosting MAE of xiehe_data_new_all test: ",mae)
r2 = r2_score(Y_test, pred_test)
print("grid_search CV GradientBoosting R^2 of xiehe_data_new_all test: ",r2)


Lasso_model = Lasso()
# 设置参数网格
param_grid_0 = {
    'alpha': [0.0001, 0.001, 0.01, 0.1, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0, 100.0],  
    'fit_intercept': [True, False],  
    'max_iter': [1000, 3000, 5000, 10000],  
    'tol': [1e-4, 1e-5, 1e-6], 
    'selection': ['cyclic', 'random'],  
    'random_state': [42],  
    'warm_start': [False, True]  
}
grid_search = GridSearchCV(estimator=Lasso_model, param_grid=param_grid_0, cv=5, verbose=2, n_jobs=max(1, int(os.cpu_count() * 0.75)))

grid_search.fit(X_train, Y_train)
print("Best parameters found: ", grid_search.best_params_)
model = grid_search.best_estimator_
with open('Lasso_xiehe_data_new_all_model.pkl', 'wb') as f:
    pickle.dump(model, f)

pred_train = model.predict(X_train)
mae = mean_absolute_error(Y_train, pred_train)
print("grid_search CV Lasso MAE of xiehe_data_new_all train: ",mae)
r2 = r2_score(Y_train, pred_train)
print("grid_search CV Lasso R^2 of xiehe_data_new_all train: ",r2)

pred_test = model.predict(X_test)
mae = mean_absolute_error(Y_test, pred_test)
print("grid_search CV Lasso MAE of xiehe_data_new_all test: ",mae)
r2 = r2_score(Y_test, pred_test)
print("grid_search CV Lasso R^2 of xiehe_data_new_all test: ",r2)


MLP_model = MLPRegressor(random_state=42)
# 设置参数网格
param_grid_0 = {
    'hidden_layer_sizes': [(50,), (100,), (50, 50), (100, 50)],
    'activation': ['relu', 'tanh'],
    'solver': ['adam'],
    'alpha': [0.0001, 0.001, 0.01],
    'batch_size': [16, 32],
    'learning_rate_init': [0.001, 0.01],
    'max_iter': [50, 100, 150],
    'early_stopping': [True]
}
grid_search = GridSearchCV(estimator=MLP_model, param_grid=param_grid_0, cv=5, verbose=2, n_jobs=max(1, int(os.cpu_count() * 0.75)))


grid_search.fit(X_train, Y_train)
print("Best parameters found: ", grid_search.best_params_)
model = grid_search.best_estimator_
with open('MLP_xiehe_data_new_all_model.pkl', 'wb') as f:
    pickle.dump(model, f)

pred_train = model.predict(X_train)
mae = mean_absolute_error(Y_train, pred_train)
print("grid_search CV MLP MAE of xiehe_data_new_all train: ",mae)
r2 = r2_score(Y_train, pred_train)
print("grid_search CV MLP R^2 of xiehe_data_new_all train: ",r2)

pred_test = model.predict(X_test)
mae = mean_absolute_error(Y_test, pred_test)
print("grid_search CV MLP MAE of xiehe_data_new_all test: ",mae)
r2 = r2_score(Y_test, pred_test)
print("grid_search CV MLP R^2 of xiehe_data_new_all test: ",r2)


RF_model = RandomForestRegressor(random_state=42)
# 设置参数网格
param_grid_0 = {
    'n_estimators': [100, 150, 200],
    'max_depth': [3, 5, 7, 10, 15],
    'min_samples_split': [10, 15, 20, 25],
    'min_samples_leaf': [8, 10, 12, 15],
    'max_features': [0.7, 0.8, 0.9, 1],
    'bootstrap': [True],
    'max_samples': [ 0.7, 0.8, 0.9, 1],
}
grid_search = GridSearchCV(estimator=RF_model, param_grid=param_grid_0, cv=5, verbose=2, n_jobs=max(1, int(os.cpu_count() * 0.75)))

grid_search.fit(X_train, Y_train)
print("Best parameters found: ", grid_search.best_params_)
model = grid_search.best_estimator_
with open('RandomForest_xiehe_data_new_all_model.pkl', 'wb') as f:
    pickle.dump(model, f)

pred_train = model.predict(X_train)
mae = mean_absolute_error(Y_train, pred_train)
print("grid_search CV RandomForest MAE of xiehe_data_new_all train: ",mae)
r2 = r2_score(Y_train, pred_train)
print("grid_search CV RandomForest R^2 of xiehe_data_new_all train: ",r2)

pred_test = model.predict(X_test)
mae = mean_absolute_error(Y_test, pred_test)
print("grid_search CV RandomForest MAE of xiehe_data_new_all test: ",mae)
r2 = r2_score(Y_test, pred_test)
print("grid_search CV RandomForest R^2 of xiehe_data_new_all test: ",r2)


Ridge_model = Ridge()
# 设置参数网格
param_grid_0 = {
    'alpha': [0.001, 0.01, 0.1, 1.0, 10.0, 100.0, 1000.0],  
    'solver': ['auto', 'svd', 'cholesky', 'lsqr', 'sparse_cg', 'sag', 'saga'],  
    'fit_intercept': [True, False],  
    'max_iter': [1000, 2000, 5000],  
    'tol': [1e-3, 1e-4, 1e-5],  
    'random_state': [42]  
}
grid_search = GridSearchCV(estimator=Ridge_model, param_grid=param_grid_0, cv=5, verbose=2, n_jobs=max(1, int(os.cpu_count() * 0.75)))

grid_search.fit(X_train, Y_train)
print("Best parameters found: ", grid_search.best_params_)
model = grid_search.best_estimator_
with open('Ridge_xiehe_data_new_all_model.pkl', 'wb') as f:
    pickle.dump(model, f)

pred_train = model.predict(X_train)
mae = mean_absolute_error(Y_train, pred_train)
print("grid_search CV Ridge MAE of xiehe_data_new_all train: ",mae)
r2 = r2_score(Y_train, pred_train)
print("grid_search CV Ridge R^2 of xiehe_data_new_all train: ",r2)

pred_test = model.predict(X_test)
mae = mean_absolute_error(Y_test, pred_test)
print("grid_search CV Ridge MAE of xiehe_data_new_all test: ",mae)
r2 = r2_score(Y_test, pred_test)
print("grid_search CV Ridge R^2 of xiehe_data_new_all test: ",r2)




SVR_model = SVR()
# 设置参数网格
param_grid_0 = {
    'kernel': ['linear', 'rbf', 'poly', 'sigmoid'],  
    'C': [0.01, 0.05, 0.1, 1.0],  
    'epsilon': [0.01, 0.05, 0.1, 0.2],  
    'gamma': ['scale', 'auto', 0.001, 0.01, 0.1],  
    'tol': [1e-3, 1e-4, 1e-5],  
    'max_iter': [ 1000, 2000, 3000],  
}
grid_search = GridSearchCV(estimator=SVR_model, param_grid=param_grid_0, cv=5, verbose=2, n_jobs=max(1, int(os.cpu_count() * 0.75)))


grid_search.fit(X_train, Y_train)
print("Best parameters found: ", grid_search.best_params_)
model = grid_search.best_estimator_
with open('SVR_xiehe_data_new_all_model.pkl', 'wb') as f:
    pickle.dump(model, f)

pred_train = model.predict(X_train)
mae = mean_absolute_error(Y_train, pred_train)
print("grid_search CV SVR MAE of xiehe_data_new_all train: ",mae)
r2 = r2_score(Y_train, pred_train)
print("grid_search CV SVR R^2 of xiehe_data_new_all train: ",r2)

pred_test = model.predict(X_test)
mae = mean_absolute_error(Y_test, pred_test)
print("grid_search CV SVR MAE of xiehe_data_new_all test: ",mae)
r2 = r2_score(Y_test, pred_test)
print("grid_search CV SVR R^2 of xiehe_data_new_all test: ",r2)



XGB_model = XGBRegressor(random_state=42, verbosity=0, n_jobs=1)
# 设置参数网格
param_grid_0 = {
    'n_estimators': [50, 100, 150, 200],
    'learning_rate': [0.01, 0.05, 0.1],
    'max_depth': [6, 8,10],
    'min_child_weight': [1, 3, 5],
    'subsample': [0.8, 0.9, 1.0],
    'colsample_bytree': [0.8, 0.9, 1.0],
    'reg_alpha': [0, 0.1],
    'reg_lambda': [1, 10]
}
grid_search = GridSearchCV(estimator=XGB_model, param_grid=param_grid_0, cv=5, verbose=2, n_jobs=max(1, int(os.cpu_count() * 0.75)))

grid_search.fit(X_train, Y_train)
print("Best parameters found: ", grid_search.best_params_)
model = grid_search.best_estimator_
with open('XGB_xiehe_data_new_all_model.pkl', 'wb') as f:
    pickle.dump(model, f)

pred_train = model.predict(X_train)
mae = mean_absolute_error(Y_train, pred_train)
print("grid_search CV XGB MAE of xiehe_data_new_all train: ",mae)
r2 = r2_score(Y_train, pred_train)
print("grid_search CV XGB R^2 of xiehe_data_new_all train: ",r2)

pred_test = model.predict(X_test)
mae = mean_absolute_error(Y_test, pred_test)
print("grid_search CV XGB MAE of xiehe_data_new_all test: ",mae)
r2 = r2_score(Y_test, pred_test)
print("grid_search CV XGB R^2 of xiehe_data_new_all test: ",r2)
