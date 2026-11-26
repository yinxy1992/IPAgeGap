############################################################
## Residual iAgeGap calibration — M3 (healthy version)
## M3: pred ~ (age + age^2) * sex，在健康测试集上拟合
## CSV encoding: GB18030
############################################################

library(dplyr)
library(readxl)

## =========================
## 1. File paths
## =========================
train_file <- "train_data_results_healthy.csv"
test_file  <- "test_data_results_healthy.csv"
hiv_file   <- "HIV_prediction_results_with_sex_healthy.xlsx"
sle_file   <- "final_results_3_levels_20_70_with_original_ids_and_sex_healthy.csv"


## =========================
## 2. Helper functions
## =========================
read_csv_gb18030 <- function(file) {
  read.csv(file, fileEncoding = "GB18030",
           stringsAsFactors = FALSE, check.names = FALSE)
}

read_csv_gb18030_robust <- function(file) {
  tryCatch(
    read.csv(file, fileEncoding = "GB18030",
             stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      message("标准 GB18030 读取失败，尝试 iconv 清理后重新读取：", file)
      lines_clean <- iconv(readLines(file, warn = FALSE, encoding = "GB18030"),
                           from = "GB18030", to = "UTF-8", sub = "")
      read.csv(text = paste(lines_clean, collapse = "\n"),
               stringsAsFactors = FALSE, check.names = FALSE)
    }
  )
}

write_csv_gb18030 <- function(df, file) {
  write.csv(df, file = file, row.names = FALSE, fileEncoding = "GB18030", na = "")
}

check_required_columns <- function(df, required_cols, dataset_name) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("数据集 ", dataset_name, " 缺少以下列：", paste(missing_cols, collapse = ", "),
         "\n当前实际列名为：", paste(names(df), collapse = ", "))
  }
}

prepare_calib_data <- function(df, age_col, pred_col, sex_col) {
  df %>%
    mutate(
      age_for_calib        = suppressWarnings(as.numeric(.data[[age_col]])),
      pred_age_for_calib   = suppressWarnings(as.numeric(.data[[pred_col]])),
      sex_for_calib        = .data[[sex_col]],
      sex_for_calib_factor = factor(sex_for_calib, levels = c(0, 1),
                                    labels = c("Female", "Male"))
    )
}

## M3 校准：IPAgeGap = 预测年龄 − 同龄同性别健康人的期望预测年龄
apply_M3_calibration <- function(df, age_col, pred_col, sex_col, calib_model) {
  df_tmp <- prepare_calib_data(df, age_col, pred_col, sex_col)
  df_tmp %>%
    mutate(
      iAgeGap_raw              = pred_age_for_calib - age_for_calib,
      pred_age_expected_M3     = predict(calib_model, newdata = df_tmp),
      iAgeGap_resid_calib_M3   = pred_age_for_calib - pred_age_expected_M3,
      pred_age_corrected_M3    = age_for_calib + iAgeGap_resid_calib_M3
    ) %>%
    select(-age_for_calib, -pred_age_for_calib, -sex_for_calib, -sex_for_calib_factor)
}


## =========================
## 3. Read data
## =========================
train_df <- read_csv_gb18030(train_file)
test_df  <- read_csv_gb18030(test_file)
hiv_df   <- read_excel(hiv_file)
sle_df   <- read_csv_gb18030_robust(sle_file)


## =========================
## 4. Define column names
## =========================
train_age_col <- "真实年龄"; train_pred_col <- "预测年龄"; train_sex_col <- "性别"
test_age_col  <- "真实年龄"; test_pred_col  <- "预测年龄"; test_sex_col  <- "性别"
hiv_age_col   <- "实际年龄"; hiv_pred_col   <- "预测年龄"; hiv_sex_col   <- "性别"
sle_age_col   <- "实际年龄"; sle_pred_col   <- "预测年龄"; sle_sex_col   <- "性别"


## =========================
## 5. Check required columns
## =========================
check_required_columns(train_df, c(train_age_col, train_pred_col, train_sex_col), train_file)
check_required_columns(test_df,  c(test_age_col,  test_pred_col,  test_sex_col),  test_file)
check_required_columns(hiv_df,   c(hiv_age_col,   hiv_pred_col,   hiv_sex_col),   hiv_file)
check_required_columns(sle_df,   c(sle_age_col,   sle_pred_col,   sle_sex_col),   sle_file)


## =========================
## 6. Prepare TEST data for calibration
## =========================
test_calib_df <- prepare_calib_data(test_df, test_age_col, test_pred_col, test_sex_col)

cat("\nTest set — missing age:", sum(is.na(test_calib_df$age_for_calib)),
    " missing pred:", sum(is.na(test_calib_df$pred_age_for_calib)), "\n")
print(table(test_calib_df$sex_for_calib_factor, useNA = "ifany"))


## =========================
## 7. Fit M3 calibration model on TEST set
## =========================
calib_model_M3 <- lm(
  pred_age_for_calib ~ (age_for_calib + I(age_for_calib^2)) * sex_for_calib_factor,
  data = test_calib_df
)

cat("\n==============================\nM3 calibration model\n==============================\n")
print(summary(calib_model_M3))


## =========================
## 8. Apply M3 calibration to all datasets
## =========================
train_df_M3 <- apply_M3_calibration(train_df, train_age_col, train_pred_col, train_sex_col, calib_model_M3)
test_df_M3  <- apply_M3_calibration(test_df,  test_age_col,  test_pred_col,  test_sex_col,  calib_model_M3)
hiv_df_M3   <- apply_M3_calibration(hiv_df,   hiv_age_col,   hiv_pred_col,   hiv_sex_col,   calib_model_M3)
sle_df_M3   <- apply_M3_calibration(sle_df,   sle_age_col,   sle_pred_col,   sle_sex_col,   calib_model_M3)


## =========================
## 9. Save results
## =========================
write_csv_gb18030(train_df_M3, "train_data_results_with_iAgeGap_M3_healthy.csv")
write_csv_gb18030(test_df_M3,  "test_data_results_with_iAgeGap_M3_healthy.csv")
write_csv_gb18030(hiv_df_M3,   "HIV_prediction_results_with_iAgeGap_M3_healthy.csv")
write_csv_gb18030(sle_df_M3,   "SLE_prediction_results_with_iAgeGap_M3_healthy.csv")


## =========================
## 10. Save calibration coefficients（6 个系数，保留完整位数）
## =========================
calib_M3_coef <- as.data.frame(summary(calib_model_M3)$coefficients)
calib_M3_coef$term <- rownames(calib_M3_coef)
rownames(calib_M3_coef) <- NULL
write.csv(calib_M3_coef, "calibration_model_M3_coefficients_healthy.csv",
          row.names = FALSE, fileEncoding = "GB18030")   # 不用 na="" 以免截断精度

options(digits = 10); print(coef(calib_model_M3)); options(digits = 7)


## =========================
## 11. Quick summaries
## =========================
cat("\n==============================\nQuick check: M3 residual iAgeGap\n==============================\n")
cat("\nTrain:\n"); print(summary(train_df_M3$iAgeGap_resid_calib_M3))
cat("\nTest:\n");  print(summary(test_df_M3$iAgeGap_resid_calib_M3))
cat("\nHIV:\n");   print(summary(hiv_df_M3$iAgeGap_resid_calib_M3))
cat("\nSLE:\n");   print(summary(sle_df_M3$iAgeGap_resid_calib_M3))

cat("\n==============================\nAll output files generated successfully (healthy, M3)\n==============================\n")