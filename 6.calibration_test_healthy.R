############################################################
## Residual iAgeGap calibration  (healthy version)
## CSV encoding: GB18030
############################################################

library(dplyr)
library(readxl)

## =========================
## 1. 配置：每组模型只改这里
## =========================
suffix     <- "healthy"
train_file <- "train_data_results_healthy.csv"
test_file  <- "test_data_results_healthy.csv"
hiv_file   <- "HIV_prediction_results_with_sex_healthy.xlsx"
sle_file   <- "final_results_3_levels_20_70_with_original_ids_and_sex_healthy.csv"
tag        <- paste0("_M3_testcalib_", suffix)   # 所有输出文件名统一加这个后缀
AGE_MIN <- 18; AGE_MAX <- 88                      # 年龄截断范围


## =========================
## 2. Helper functions
## =========================

## 2.1 More stable GB18030 CSV reader
read_csv_gb18030 <- function(file) {
  read.csv(
    file,
    fileEncoding = "GB18030",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


## 2.2 Robust GB18030 reader
## 如果普通 GB18030 读取仍然报 Incomplete multibyte sequence，
## 则尝试先 readLines，再用 iconv 去掉无法解码的异常字符。
read_csv_gb18030_robust <- function(file) {
  
  df <- tryCatch(
    {
      read.csv(
        file,
        fileEncoding = "GB18030",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },
    error = function(e) {
      
      message("标准 GB18030 读取失败：", file)
      message("错误信息：", conditionMessage(e))
      message("尝试使用 iconv 清理异常字符后重新读取...")
      
      lines_raw <- readLines(
        file,
        warn = FALSE,
        encoding = "GB18030"
      )
      
      lines_clean <- iconv(
        lines_raw,
        from = "GB18030",
        to = "UTF-8",
        sub = ""
      )
      
      read.csv(
        text = paste(lines_clean, collapse = "\n"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  )
  
  return(df)
}


## 2.3 Write CSV using GB18030
write_csv_gb18030 <- function(df, file) {
  write.csv(
    df,
    file = file,
    row.names = FALSE,
    fileEncoding = "GB18030",
    na = ""
  )
}


## 2.4 Check required columns
check_required_columns <- function(df, required_cols, dataset_name) {
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "数据集 ", dataset_name, " 缺少以下列：",
        paste(missing_cols, collapse = ", "),
        "\n当前实际列名为：",
        paste(names(df), collapse = ", ")
      )
    )
  }
}


## 2.5 Prepare calibration variables
prepare_calib_data <- function(df, age_col, pred_col, sex_col) {
  df %>%
    mutate(
      age_original       = suppressWarnings(as.numeric(.data[[age_col]])),
      age_for_calib      = pmin(pmax(age_original, AGE_MIN), AGE_MAX),  # 仅用于代入校准公式
      pred_age_for_calib = suppressWarnings(as.numeric(.data[[pred_col]])),
      sex_for_calib      = .data[[sex_col]],
      sex_for_calib_factor = factor(sex_for_calib, levels = c(0, 1),
                                    labels = c("Female", "Male"))
    )
}


## 2.6 Apply age-only calibration
apply_age_calibration <- function(df, age_col, pred_col, sex_col, calib_model_age) {
  
  df_tmp <- prepare_calib_data(
    df = df,
    age_col = age_col,
    pred_col = pred_col,
    sex_col = sex_col
  )
  
  df_out <- df_tmp %>%
    mutate(
      iAgeGap_raw = pred_age_for_calib - age_original,
      pred_age_expected_calib_age = predict(
        calib_model_age,
        newdata = df_tmp
      ),
      iAgeGap_resid_calib_age =
        pred_age_for_calib - pred_age_expected_calib_age
    ) %>%
    select(-age_original, -age_for_calib, -pred_age_for_calib, -sex_for_calib, -sex_for_calib_factor) 
  
  return(df_out)
}


## 2.7 Apply age + sex calibration
apply_age_sex_calibration <- function(df, age_col, pred_col, sex_col, calib_model_age_sex) {
  
  df_tmp <- prepare_calib_data(
    df = df,
    age_col = age_col,
    pred_col = pred_col,
    sex_col = sex_col
  )
  
  df_out <- df_tmp %>%
    mutate(
      iAgeGap_raw = pred_age_for_calib - age_original,
      pred_age_expected_calib_age_and_sex = predict(
        calib_model_age_sex,
        newdata = df_tmp
      ),
      iAgeGap_resid_calib_age_and_sex =
        pred_age_for_calib - pred_age_expected_calib_age_and_sex
    ) %>%
    select(-age_original, -age_for_calib, -pred_age_for_calib, -sex_for_calib, -sex_for_calib_factor) 
  
  return(df_out)
}


## =========================
## 3. Read data
## =========================

## train/test 用普通 GB18030 读取
train_df <- read_csv_gb18030(train_file)
test_df  <- read_csv_gb18030(test_file)

## xlsx 不需要指定编码
hiv_df <- read_excel(hiv_file)

## SLE 文件用更稳健的读取方式
sle_df <- read_csv_gb18030_robust(sle_file)


## =========================
## 4. Check column names
## =========================

cat("\n==============================\n")
cat("Column names check\n")
cat("==============================\n")

cat("\nTrain columns:\n")
print(names(train_df))

cat("\nTest columns:\n")
print(names(test_df))

cat("\nHIV columns:\n")
print(names(hiv_df))

cat("\nSLE columns:\n")
print(names(sle_df))


## =========================
## 5. Define column names
## =========================

train_age_col <- "真实年龄"
train_pred_col <- "预测年龄"
train_sex_col <- "性别"

test_age_col <- "真实年龄"
test_pred_col <- "预测年龄"
test_sex_col <- "性别"

hiv_age_col <- "实际年龄"
hiv_pred_col <- "预测年龄"
hiv_sex_col <- "性别"

sle_age_col <- "实际年龄"
sle_pred_col <- "预测年龄"
sle_sex_col <- "性别"


## =========================
## 6. Check required columns
## =========================

check_required_columns(
  train_df,
  c(train_age_col, train_pred_col, train_sex_col),
  "train_data_results_healthy.csv"
)

check_required_columns(
  test_df,
  c(test_age_col, test_pred_col, test_sex_col),
  "test_data_results_healthy.csv"
)

check_required_columns(
  hiv_df,
  c(hiv_age_col, hiv_pred_col, hiv_sex_col),
  "HIV_prediction_results_with_sex_healthy.xlsx"
)

check_required_columns(
  sle_df,
  c(sle_age_col, sle_pred_col, sle_sex_col),
  "final_results_3_levels_20_70_with_original_ids_and_sex_healthy.csv"
)


## =========================
## 7. Prepare training data
## =========================

train_calib_df <- prepare_calib_data(
  df = train_df,
  age_col = train_age_col,
  pred_col = train_pred_col,
  sex_col = train_sex_col
)


cat("\n==============================\n")
cat("Training calibration variables summary\n")
cat("==============================\n")

cat("\nMissing age:\n")
print(sum(is.na(train_calib_df$age_for_calib)))

cat("\nMissing predicted age:\n")
print(sum(is.na(train_calib_df$pred_age_for_calib)))

cat("\nSex distribution:\n")
print(table(train_calib_df$sex_for_calib, useNA = "ifany"))

cat("\nSex factor distribution:\n")
print(table(train_calib_df$sex_for_calib_factor, useNA = "ifany"))


## 准备测试集校准变量
test_calib_df <- prepare_calib_data(
  df = test_df,
  age_col = test_age_col,
  pred_col = test_pred_col,
  sex_col = test_sex_col
)
cat("\nTest set sex distribution:\n")
print(table(test_calib_df$sex_for_calib_factor, useNA = "ifany"))


test_calib_df <- prepare_calib_data(test_df, test_age_col, test_pred_col, test_sex_col)


## =========================
## 8. Step 1: Age-only calibration model
## =========================

# calib_model_age <- lm(
#   pred_age_for_calib ~ age_for_calib,
#   data = train_calib_df
# )

calib_model_age <- lm(pred_age_for_calib ~ age_for_calib,
                      data = test_calib_df)

cat("\n==============================\n")
cat("Age-only calibration model\n")
cat("==============================\n")
print(summary(calib_model_age))


## =========================
## 9. Apply age-only calibration
## =========================

train_df_age_calib <- apply_age_calibration(
  df = train_df,
  age_col = train_age_col,
  pred_col = train_pred_col,
  sex_col = train_sex_col,
  calib_model_age = calib_model_age
)

test_df_age_calib <- apply_age_calibration(
  df = test_df,
  age_col = test_age_col,
  pred_col = test_pred_col,
  sex_col = test_sex_col,
  calib_model_age = calib_model_age
)

hiv_df_age_calib <- apply_age_calibration(
  df = hiv_df,
  age_col = hiv_age_col,
  pred_col = hiv_pred_col,
  sex_col = hiv_sex_col,
  calib_model_age = calib_model_age
)

sle_df_age_calib <- apply_age_calibration(
  df = sle_df,
  age_col = sle_age_col,
  pred_col = sle_pred_col,
  sex_col = sle_sex_col,
  calib_model_age = calib_model_age
)


## =========================
## 10. Save age-only calibration results
## =========================

write_csv_gb18030(
  train_df_age_calib,
  paste0("train_data_results_with_iAgeGap_resid_calib_age_healthy", tag, ".csv")
)

write_csv_gb18030(
  test_df_age_calib,
  paste0("test_data_results_with_iAgeGap_resid_calib_age_healthy", tag, ".csv")
)

write_csv_gb18030(
  hiv_df_age_calib,
  paste0("HIV_prediction_results_with_iAgeGap_resid_calib_age_healthy", tag, ".csv")
)

write_csv_gb18030(
  sle_df_age_calib,
  paste0("SLE_prediction_results_with_iAgeGap_resid_calib_age_healthy", tag, ".csv")
)


## 11. 主定义：M3（二次 × 性别），在测试集上拟合
calib_model_age_sex <- lm(
  pred_age_for_calib ~ (age_for_calib + I(age_for_calib^2)) * sex_for_calib_factor,
  data = test_calib_df
)
print(summary(calib_model_age_sex))
options(digits = 10); print(coef(calib_model_age_sex)); options(digits = 7)

## 11b. 敏感性分析：M2（线性 × 性别）
calib_model_M2 <- lm(pred_age_for_calib ~ age_for_calib * sex_for_calib_factor,
                     data = test_calib_df)




library(splines)
m_lin <- lm(pred_age_for_calib ~ age_for_calib + sex_for_calib_factor, data = test_calib_df)
m_spl <- lm(pred_age_for_calib ~ ns(age_for_calib, df = 3) + sex_for_calib_factor, data = test_calib_df)
print(anova(m_lin, m_spl))
print(c(AIC_linear = AIC(m_lin), AIC_spline = AIC(m_spl)))


test_agesex <- apply_age_sex_calibration(test_df, test_age_col, test_pred_col,
                                         test_sex_col, calib_model_age_sex)
sub <- test_agesex %>%
  filter(`性别` == 0, `真实年龄` >= 18, `真实年龄` <= 45)

n <- nrow(sub)
m <- mean(sub$iAgeGap_resid_calib_age_and_sex, na.rm = TRUE)
s <- sd(sub$iAgeGap_resid_calib_age_and_sex, na.rm = TRUE)
cat("n =", n, " mean =", round(m, 2), " SD =", round(s, 2),
    " 95%CI = [", round(m - 1.96*s/sqrt(n), 2), ",", round(m + 1.96*s/sqrt(n), 2), "]\n")




set.seed(2026)
d <- test_calib_df %>% filter(!is.na(age_for_calib), !is.na(pred_age_for_calib))
d$sex <- d$sex_for_calib_factor

K <- 5; R <- 100   # 重复 100 次，让结果稳定
gap_mat <- matrix(NA, nrow(d), R)

for (r in 1:R) {
  folds <- sample(rep(1:K, length.out = nrow(d)))
  for (k in 1:K) {
    fit <- lm(pred_age_for_calib ~ age_for_calib + sex, data = d[folds != k, ])
    gap_mat[folds == k, r] <- d$pred_age_for_calib[folds == k] -
      predict(fit, newdata = d[folds == k, ])
  }
}
d$gap_cf <- rowMeans(gap_mat)     # 每人 100 次交叉拟合 gap 的平均

## 残余偏差检验
res <- lm(gap_cf ~ age_for_calib + sex, data = d)
print(summary(res)$coefficients)
print(confint(res))               # age 斜率的 95% CI 应包含 0
cat("mean =", mean(d$gap_cf), " SD =", sd(d$gap_cf), "\n")

## 按年龄段 × 性别的均值
d$age_grp <- cut(d$age_for_calib, c(17, 39, 59, 100), labels = c("18-39","40-59",">=60"))
d %>% group_by(age_grp, sex) %>%
  summarise(n = n(), mean = mean(gap_cf), sd = sd(gap_cf), .groups = "drop") %>% print()

## 保存，供第 5 步绘图使用
write.csv(d[, c("age_for_calib","sex","pred_age_for_calib","gap_cf")],
          "test_gap_crossfit.csv", row.names = FALSE)




## =========================
## 12. Apply age + sex calibration
## =========================

train_df_age_sex_calib <- apply_age_sex_calibration(
  df = train_df,
  age_col = train_age_col,
  pred_col = train_pred_col,
  sex_col = train_sex_col,
  calib_model_age_sex = calib_model_age_sex
)

test_df_age_sex_calib <- apply_age_sex_calibration(
  df = test_df,
  age_col = test_age_col,
  pred_col = test_pred_col,
  sex_col = test_sex_col,
  calib_model_age_sex = calib_model_age_sex
)

hiv_df_age_sex_calib <- apply_age_sex_calibration(
  df = hiv_df,
  age_col = hiv_age_col,
  pred_col = hiv_pred_col,
  sex_col = hiv_sex_col,
  calib_model_age_sex = calib_model_age_sex
)

sle_df_age_sex_calib <- apply_age_sex_calibration(
  df = sle_df,
  age_col = sle_age_col,
  pred_col = sle_pred_col,
  sex_col = sle_sex_col,
  calib_model_age_sex = calib_model_age_sex
)




hiv_df_M2 <- apply_age_sex_calibration(hiv_df, hiv_age_col, hiv_pred_col, hiv_sex_col, calib_model_M2)
sle_df_M2 <- apply_age_sex_calibration(sle_df, sle_age_col, sle_pred_col, sle_sex_col, calib_model_M2)
write_csv_gb18030(hiv_df_M2, paste0("HIV_IPAgeGap_M2sens", tag, ".csv"))
write_csv_gb18030(sle_df_M2, paste0("SLE_IPAgeGap_M2sens", tag, ".csv"))

## =========================
## 13. Save age + sex calibration results
## =========================

write_csv_gb18030(
  train_df_age_sex_calib,
  paste0("train_data_results_with_iAgeGap_resid_calib_age_and_sex_healthy", tag, ".csv")
)

write_csv_gb18030(
  test_df_age_sex_calib,
  paste0("test_data_results_with_iAgeGap_resid_calib_age_and_sex_healthy", tag, ".csv")
)

write_csv_gb18030(
  hiv_df_age_sex_calib,
  paste0("HIV_prediction_results_with_iAgeGap_resid_calib_age_and_sex_healthy", tag, ".csv")
)

write_csv_gb18030(
  sle_df_age_sex_calib,
  paste0("SLE_prediction_results_with_iAgeGap_resid_calib_age_and_sex_healthy", tag, ".csv")
)


## =========================
## 14. Save calibration model coefficients
## =========================

calib_age_coef <- as.data.frame(summary(calib_model_age)$coefficients)
calib_age_coef$term <- rownames(calib_age_coef)
rownames(calib_age_coef) <- NULL

calib_age_sex_coef <- as.data.frame(summary(calib_model_age_sex)$coefficients)
calib_age_sex_coef$term <- rownames(calib_age_sex_coef)
rownames(calib_age_sex_coef) <- NULL

write_csv_gb18030(
  calib_age_coef,
  paste0("calibration_model_age_only_coefficients_healthy", tag, ".csv")
)

write_csv_gb18030(
  calib_age_sex_coef,
  paste0("calibration_model_age_and_sex_coefficients_healthy", tag, ".csv")
)


## =========================
## 15. Quick summaries
## =========================

cat("\n==============================\n")
cat("Quick check: age-only residual iAgeGap\n")
cat("==============================\n")

cat("\nTrain:\n")
print(summary(train_df_age_calib$iAgeGap_resid_calib_age))

cat("\nTest:\n")
print(summary(test_df_age_calib$iAgeGap_resid_calib_age))

cat("\nHIV:\n")
print(summary(hiv_df_age_calib$iAgeGap_resid_calib_age))

cat("\nSLE:\n")
print(summary(sle_df_age_calib$iAgeGap_resid_calib_age))


cat("\n==============================\n")
cat("Quick check: age + sex residual iAgeGap\n")
cat("==============================\n")

cat("\nTrain:\n")
print(summary(train_df_age_sex_calib$iAgeGap_resid_calib_age_and_sex))

cat("\nTest:\n")
print(summary(test_df_age_sex_calib$iAgeGap_resid_calib_age_and_sex))

cat("\nHIV:\n")
print(summary(hiv_df_age_sex_calib$iAgeGap_resid_calib_age_and_sex))

cat("\nSLE:\n")
print(summary(sle_df_age_sex_calib$iAgeGap_resid_calib_age_and_sex))


cat("\n==============================\n")
cat("All output files generated successfully (healthy)\n")
cat("==============================\n")