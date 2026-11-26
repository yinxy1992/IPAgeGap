import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import json
import pickle
import warnings
warnings.filterwarnings('ignore')

plt.rcParams['font.size'] = 11

from sklearn.model_selection import GridSearchCV
from xgboost import XGBRegressor
import shap

# ===================== 路径与保存设置 =====================
TRAIN_PATH = './xiehe_all_train_data_20260312.csv'
TEST_PATH = './xiehe_all_test_data_20260312.csv'
SAVE_DIR = './ensemble_results/'
os.makedirs(SAVE_DIR, exist_ok=True)

# ===================== 读取数据 =====================
train_data = pd.read_csv(TRAIN_PATH, encoding='gb18030')
test_data = pd.read_csv(TEST_PATH, encoding='gb18030')

X_train = train_data.iloc[:, 1:]
Y_train = train_data.iloc[:, 0]
X_test = test_data.iloc[:, 1:]
Y_test = test_data.iloc[:, 0]

print(f"训练集: {X_train.shape} | 测试集: {X_test.shape}")
FEATURE_NAMES = X_train.columns.tolist()

# ===================== 超参数设置 =====================
BEST_PARAMS_PATH = os.path.join(SAVE_DIR, 'best_params.json')

with open(BEST_PARAMS_PATH, 'r') as f:
    best_params = json.load(f)
print("已加载最优超参数：", best_params)

# ===================== 固定使用 20 个模型集成 =====================
n_ensemble = 20
model_path = os.path.join(SAVE_DIR, '20_ensemble_models_healthy.pkl')

# 训练/加载 20个XGBoost
if not os.path.exists(model_path):
    print("开始训练 20 个集成模型...")
    models = []
    for seed in range(n_ensemble):
        m = XGBRegressor(**best_params, random_state=seed, verbosity=0)
        m.fit(X_train, Y_train)
        models.append(m)
        print(f"模型 {seed+1}/{n_ensemble} 训练完成")
    
    with open(model_path, 'wb') as f:
        pickle.dump(models, f)
else:
    print("加载已保存的 20 个集成模型...")
    with open(model_path, 'rb') as f:
        models = pickle.load(f)

# ===================== 集成预测 =====================
def ensemble_predict(models, X):
    preds = [m.predict(X) for m in models]
    return np.mean(preds, axis=0)

y_pred_train = ensemble_predict(models, X_train)
y_pred_test = ensemble_predict(models, X_test)

# 保存预测结果
np.save(os.path.join(SAVE_DIR, 'y_pred_train.npy'), y_pred_train)
np.save(os.path.join(SAVE_DIR, 'y_pred_test.npy'), y_pred_test)

# ===================== 集成特征重要性 =====================
feat_imp = np.mean([m.feature_importances_ for m in models], axis=0)
feat_imp_df = pd.DataFrame({
    'feature': FEATURE_NAMES,
    'importance': feat_imp
}).sort_values('importance', ascending=False)

feat_imp_df.to_csv(os.path.join(SAVE_DIR, 'ensemble_feature_importance.csv'), index=False,encoding='utf-8-sig')
print("\n集成特征重要性（Top10）：")
print(feat_imp_df.head(10))

# ========== 核心：集成 SHAP 解释（文献方法） ==========
print("\n开始计算集成 SHAP 值...")

shap_train_path = os.path.join(SAVE_DIR, 'shap_values_train.npy')
shap_test_path = os.path.join(SAVE_DIR, 'shap_values_test.npy')

if not os.path.exists(shap_train_path):
    # 逐个模型计算 SHAP 并平均
    shap_values_train_list = []
    shap_values_test_list = []

    for i, m in enumerate(models):
        explainer = shap.TreeExplainer(m)
        sv_train = explainer.shap_values(X_train)
        sv_test = explainer.shap_values(X_test)
        shap_values_train_list.append(sv_train)
        shap_values_test_list.append(sv_test)
        print(f"模型 {i+1}/{n_ensemble} SHAP 计算完成")

    # 集成 = 取平均
    shap_values_train = np.mean(shap_values_train_list, axis=0)
    shap_values_test = np.mean(shap_values_test_list, axis=0)

    np.save(shap_train_path, shap_values_train)
    np.save(shap_test_path, shap_values_test)
    print("集成 SHAP 值已保存")
else:
    shap_values_train = np.load(shap_train_path)
    shap_values_test = np.load(shap_test_path)
    print("已加载保存的集成 SHAP 值")

# ===================== SHAP 绘图（可直接用于论文） =====================
plt.rcParams['font.size'] = 11

# 1. SHAP 总结图（重要性排序）
plt.figure(figsize=(8, 10))
shap.summary_plot(shap_values_train, X_train, max_display = 20, feature_names=FEATURE_NAMES, show=False)
plt.tight_layout()
plt.savefig(os.path.join(SAVE_DIR, 'shap_summary_plot.png'), dpi=300, bbox_inches='tight')
plt.close()

# 2. SHAP 热力图（可选）
exp = shap.Explanation(values=shap_values_train, data=X_train, feature_names=FEATURE_NAMES)
plt.figure(figsize=(10, 6))
shap.plots.heatmap(exp, show=False)
plt.tight_layout()
plt.savefig(os.path.join(SAVE_DIR, 'shap_heatmap.png'), dpi=300)
plt.close()

# 3. 保存 Top 20 重要特征的 SHAP 依赖图数据
top10_feats = feat_imp_df['feature'].head(20).tolist()
for feat in top10_feats:
    safe_feat_name = feat.replace('/', '_').replace('%', '_').replace('+', '_').replace(' ', '_')
    plt.figure(figsize=(6, 4))
    shap.dependence_plot(feat, shap_values_train, X_train, show=False)
    plt.tight_layout()
    plt.savefig(os.path.join(SAVE_DIR, f'shap_dependence_{safe_feat_name}.png'), dpi=300)
    plt.close()

print("\n✅ 全部计算完成！所有结果已保存至：", SAVE_DIR)
