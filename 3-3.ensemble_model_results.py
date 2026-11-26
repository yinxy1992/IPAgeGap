import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import json
import pickle
import warnings
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.model_selection import learning_curve
import shap

warnings.filterwarnings('ignore')

# ===================== 全局配置 =====================
plt.rcParams.update({
    "font.size": 11,
    "axes.labelsize": 12,
    "axes.titlesize": 13,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 10,
    "figure.dpi": 300,
    "savefig.dpi": 300,
    "axes.grid": False
})

# ===================== 路径与全局变量 =====================
TRAIN_PATH = './xiehe_all_train_data_20260312.csv'
TEST_PATH = './xiehe_all_test_data_20260312.csv'
SAVE_DIR = './ensemble_results/'
os.makedirs(SAVE_DIR, exist_ok=True)

n_ensemble = 20  # 固定20个集成模型
top_n_feat = 20  # 前20个核心特征

# ===================== 加载数据 =====================
train_data = pd.read_csv(TRAIN_PATH, encoding='gb18030')
test_data = pd.read_csv(TEST_PATH, encoding='gb18030')
X_train = train_data.iloc[:, 1:]
Y_train = train_data.iloc[:, 0]
X_test = test_data.iloc[:, 1:]
Y_test = test_data.iloc[:, 0]
FEATURE_NAMES = X_train.columns.tolist()

# ===================== 加载模型、预测值、SHAP =====================
model_path = os.path.join(SAVE_DIR, '20_ensemble_models_healthy.pkl')
with open(model_path, 'rb') as f:
    models = pickle.load(f)

y_pred_train = np.load(os.path.join(SAVE_DIR, 'y_pred_train.npy'))
y_pred_test = np.load(os.path.join(SAVE_DIR, 'y_pred_test.npy'))
shap_values_train = np.load(os.path.join(SAVE_DIR, 'shap_values_train.npy'))
shap_values_test = np.load(os.path.join(SAVE_DIR, 'shap_values_test.npy'))
feat_imp_df = pd.read_csv(os.path.join(SAVE_DIR, 'ensemble_feature_importance.csv'))
top_feats = feat_imp_df['feature'].head(top_n_feat).tolist()

# ===================== 计算模型指标 =====================
def calc_regression_metrics(y_true, y_pred):
    r2 = r2_score(y_true, y_pred)
    rmse = np.sqrt(mean_squared_error(y_true, y_pred))
    mae = mean_absolute_error(y_true, y_pred)
    mask = y_true != 0
    mape = np.mean(np.abs((y_true[mask] - y_pred[mask]) / y_true[mask])) * 100 if np.any(mask) else np.nan
    return {
        "R2": round(r2, 4),
        "RMSE": round(rmse, 4),
        "MAE": round(mae, 4),
        "MAPE(%)": round(mape, 2) if not np.isnan(mape) else "NA"
    }

train_metrics = calc_regression_metrics(Y_train, y_pred_train)
test_metrics = calc_regression_metrics(Y_test, y_pred_test)
all_metrics = {"Train": train_metrics, "Test": test_metrics}

with open(os.path.join(SAVE_DIR, 'model_metrics.json'), 'w', encoding='utf-8') as f:
    json.dump(all_metrics, f, indent=4, ensure_ascii=False)

print("="*50)
print("训练集指标:", train_metrics)
print("测试集指标:", test_metrics)
print("="*50)

# ===================== 绘图：真实值 vs 预测值 =====================
plt.figure(figsize=(7,5))
plt.scatter(Y_test, y_pred_test, alpha=0.6, s=12, color="#1f77b4")
min_val = min(Y_test.min(), y_pred_test.min())
max_val = max(Y_test.max(), y_pred_test.max())
plt.plot([min_val, max_val], [min_val, max_val], 'r--', label="Perfect Fit")
plt.xlabel("True Value")
plt.ylabel("Predicted Value")
plt.legend()
plt.savefig(os.path.join(SAVE_DIR, 'true_vs_pred.png'), bbox_inches='tight')
plt.close()

# ===================== 绘图：残差分布 =====================
residual_test = Y_test - y_pred_test
plt.figure(figsize=(7,5))
plt.hist(residual_test, bins=30, alpha=0.7, color="#2ca02c", edgecolor="black")
plt.axvline(0, color='red', linestyle='--')
plt.xlabel("Residual")
plt.ylabel("Count")
plt.savefig(os.path.join(SAVE_DIR, 'residual_dist.png'), bbox_inches='tight')
plt.close()

# ===================== 绘图：残差 vs 预测值 =====================
plt.figure(figsize=(7,5))
plt.scatter(y_pred_test, residual_test, alpha=0.6, s=12, color="#d62728")
plt.axhline(0, color='black', linestyle='--')
plt.xlabel("Predicted Value")
plt.ylabel("Residual")
plt.savefig(os.path.join(SAVE_DIR, 'residual_vs_pred.png'), bbox_inches='tight')
plt.close()

# ===================== 绘图：学习曲线 =====================
base_model = models[0]
train_sizes, train_scores, test_scores = learning_curve(
    base_model, X_train, Y_train, cv=5, scoring="r2", train_sizes=np.linspace(0.1,1.0,10)
)
train_mean = train_scores.mean(1)
test_mean = test_scores.mean(1)

plt.figure(figsize=(7,5))
plt.plot(train_sizes, train_mean, "o-", label="Train R2")
plt.plot(train_sizes, test_mean, "s-", label="Val R2")
plt.fill_between(train_sizes, train_mean - train_scores.std(1), train_mean + train_scores.std(1), alpha=0.2)
plt.fill_between(train_sizes, test_mean - test_scores.std(1), test_mean + test_scores.std(1), alpha=0.2)
plt.xlabel("Train Size")
plt.ylabel("R2")
plt.legend()
plt.savefig(os.path.join(SAVE_DIR, 'learning_curve.png'), bbox_inches='tight')
plt.close()

# ===================== 绘图：SHAP 汇总图 =====================
plt.figure(figsize=(8,10))
shap.summary_plot(shap_values_train, X_train, feature_names=FEATURE_NAMES, show=False)
plt.savefig(os.path.join(SAVE_DIR, 'shap_summary.png'), bbox_inches='tight')
plt.close()

# ===================== 绘图：SHAP 依赖图 =====================
top10_feats = feat_imp_df['feature'].head(20).tolist()
for feat in top10_feats:
    safe = feat.replace('/', '_').replace('%', '_').replace('+', '_').replace(' ', '_')
    plt.figure(figsize=(6,4.5))
    shap.dependence_plot(feat, shap_values_train, X_train, show=False)
    plt.savefig(os.path.join(SAVE_DIR, f'shap_dependence_{safe}.png'), bbox_inches='tight')
    plt.close()

# ===================== 绘图：SHAP 交互图 =====================
if len(top_feats) >= 2:
    f1, f2 = top_feats[0], top_feats[1]
    safe1 = f1.replace('/', '_').replace('%', '_').replace('+', '_')
    plt.figure(figsize=(7,5))
    shap.dependence_plot(f1, shap_values_train, X_train, interaction_index=f2, show=False)
    plt.savefig(os.path.join(SAVE_DIR, f'shap_interaction_{safe1}.png'), bbox_inches='tight')
    plt.close()

print("\n✅ 全部运行完成！所有图表与指标已保存。")
