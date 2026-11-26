import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import os
import json
import pickle
import warnings
warnings.filterwarnings('ignore')

from sklearn.model_selection import GridSearchCV
from xgboost import XGBRegressor

_zh_candidates = [
    'WenQuanYi Zen Hei', 'WenQuanYi Micro Hei',
    'Noto Sans CJK SC', 'Noto Sans SC', 'Source Han Sans SC',
    'SimHei', 'Microsoft YaHei',
]
for _f in [
    os.path.expanduser('~/.local/share/fonts/wqy-zenhei.ttc'),
    '/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc',
    '/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc',
]:
    if os.path.exists(_f):
        try:
            fm.fontManager.addfont(_f)
        except Exception:
            pass

_installed = {f.name for f in fm.fontManager.ttflist}
_zh_available = [n for n in _zh_candidates if n in _installed]
print("[字体] 可用中文字体:", _zh_available if _zh_available else "无（请检查 fc-list :lang=zh）")

plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = _zh_available + ['DejaVu Sans', 'Arial']
plt.rcParams['axes.unicode_minus'] = False

# ===================== 路径设置 =====================
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

print(f"训练集样本数: {X_train.shape[0]}, 特征数: {X_train.shape[1]}")

# ===================== 参数搜索空间 =====================
# 91维特征 + 5000样本 → 树浅、正则强、采样适中
param_grid = {
    'n_estimators': [100, 200, 300],
    'learning_rate': [0.01, 0.05, 0.1],
    'max_depth': [3, 4, 5],                # 90维特征不能深
    'min_child_weight': [2, 5, 8],         # 减少过拟合
    'subsample': [0.6, 0.7, 0.8],          # 行采样
    'colsample_bytree': [0.6, 0.7, 0.8],   # 列采样（90维不用太高）
    'reg_alpha': [0, 0.1, 1],              # L1
    'reg_lambda': [5, 10, 15],             # L2 强正则
    'gamma': [0, 0.1, 0.5]                 # 防止无谓分裂
}

# ===================== 网格搜索 =====================
do_grid_search = True

if do_grid_search:
    print("开始网格搜索...")
    base_model = XGBRegressor(random_state=42, verbosity=0, n_jobs=1)
    grid_search = GridSearchCV(
        estimator=base_model,
        param_grid=param_grid,
        cv=5,
        verbose=2,
        n_jobs=max(1, int(os.cpu_count() * 0.75))
    )
    grid_search.fit(X_train, Y_train)
    best_params = grid_search.best_params_

    # 保存最优参数
    with open(os.path.join(SAVE_DIR, 'best_params.json'), 'w', encoding='utf-8') as f:
        json.dump(best_params, f, indent=4, ensure_ascii=False)
    print("已保存最优参数 →", best_params)
else:
    with open(os.path.join(SAVE_DIR, 'best_params.json'), 'r') as f:
        best_params = json.load(f)
    print("已读取最优参数 →", best_params)

# ===================== 训练100个不同随机种子的XGBoost模型 =====================
max_models = 100
top_n = 20
model_save_path = os.path.join(SAVE_DIR, 'trained_models.pkl')

do_train_models = True

if do_train_models:
    all_models = []
    for seed in range(max_models):
        model = XGBRegressor(**best_params, random_state=seed, verbosity=0)
        model.fit(X_train, Y_train)
        all_models.append(model)
        print(f"训练完成 {seed+1}/{max_models}")

    # 保存所有模型
    with open(model_save_path, 'wb') as f:
        pickle.dump(all_models, f)
else:
    with open(model_save_path, 'rb') as f:
        all_models = pickle.load(f)
    print("已读取100个训练好的模型")

# ===================== 计算集成稳定性（重叠率）=====================
# 最终全部模型的特征重要性
final_imp = np.mean([m.feature_importances_ for m in all_models], axis=0)
final_top = set(pd.Series(final_imp, index=X_train.columns).nlargest(top_n).index)

overlap_ratios = []
model_counts = list(range(1, max_models + 1))

for n in model_counts:
    current_models = all_models[:n]
    cur_imp = np.mean([m.feature_importances_ for m in current_models], axis=0)
    cur_top = set(pd.Series(cur_imp, index=X_train.columns).nlargest(top_n).index)
    overlap = len(cur_top & final_top) / top_n
    overlap_ratios.append(overlap)

# 保存重叠率数据，方便后期改图
stability_data = {
    'model_counts': model_counts,
    'overlap_ratios': overlap_ratios,
    'top_n': top_n
}
with open(os.path.join(SAVE_DIR, 'stability_data.json'), 'w', encoding='utf-8') as f:
    json.dump(stability_data, f, indent=4, ensure_ascii=False)

# ===================== 画图：Extended Data Fig 5a 复刻版 =====================
plt.rcParams['font.size'] = 11
plt.figure(figsize=(7, 4))
plt.plot(model_counts, overlap_ratios, linewidth=2.5, color='#2E86AB', marker='o', markersize=4)
plt.axhline(y=0.95, color='crimson', linestyle='--', linewidth=1.5, label='95% overlap threshold')
plt.xlabel('Number of ensemble XGBoost models', fontsize=12)
plt.ylabel(f'Overlap ratio of top {top_n} features', fontsize=12)
plt.title('Feature importance stability with increasing ensemble size', fontsize=13)
plt.ylim(0.5, 1.05)
plt.grid(alpha=0.3)
plt.legend()
plt.tight_layout()
plt.savefig(os.path.join(SAVE_DIR, 'ensemble_stability.png'), dpi=300)
plt.show()
