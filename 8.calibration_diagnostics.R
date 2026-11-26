############################################################
## calibration_diagnostics.R
## 用途：校准模型诊断（不生成正式结果）
############################################################
library(dplyr); library(splines); library(ggplot2); library(readxl)

## ===== 0. 配置：每组模型只改这里 =====
suffix    <- "healthy"   # 队列标识，用于结果文件命名
test_file <- "test_data_results_healthy.csv"
hiv_file  <- "HIV_prediction_results_with_sex_healthy.xlsx"
sle_file  <- "final_results_3_levels_20_70_with_original_ids_and_sex_healthy.csv"
age_col_test <- "真实年龄"; pred_col_test <- "预测年龄"; sex_col_test <- "性别"
age_col_dis  <- "实际年龄"          # HIV / SLE 的年龄列名
out_dir <- paste0("diag_", suffix); dir.create(out_dir, showWarnings = FALSE)

## ===== 读取函数（兼容 xlsx 和 GB18030 编码的 csv）=====
read_any <- function(f) {
  if (grepl("\\.xlsx$", f)) return(as.data.frame(read_excel(f)))
  tryCatch(read.csv(f, fileEncoding = "GB18030", check.names = FALSE),
    error = function(e) {
      x <- iconv(readLines(f, warn = FALSE, encoding = "GB18030"), "GB18030", "UTF-8", sub = "")
      read.csv(text = paste(x, collapse = "\n"), check.names = FALSE)
    })
}

## ===== 1. 准备测试集 =====
test <- read_any(test_file)
d <- data.frame(
  age_for_calib        = suppressWarnings(as.numeric(test[[age_col_test]])),
  pred_age_for_calib   = suppressWarnings(as.numeric(test[[pred_col_test]])),
  sex_for_calib_factor = factor(test[[sex_col_test]], levels = c(0, 1),
                                labels = c("Female", "Male"))
) %>% filter(!is.na(age_for_calib), !is.na(pred_age_for_calib), !is.na(sex_for_calib_factor))
cat("测试集 n =", nrow(d), "\n"); print(table(d$sex_for_calib_factor))
cat("测试集年龄范围:", range(d$age_for_calib), "\n")

## ===== 2. M1–M4 交叉拟合比较 =====
forms <- list(
  M1_add_linear = pred_age_for_calib ~ age_for_calib + sex_for_calib_factor,
  M2_int_linear = pred_age_for_calib ~ age_for_calib * sex_for_calib_factor,
  M3_quad_int   = pred_age_for_calib ~ (age_for_calib + I(age_for_calib^2)) * sex_for_calib_factor,
  M4_spline_int = pred_age_for_calib ~ ns(age_for_calib, df = 3) * sex_for_calib_factor
)
d$age_grp <- cut(d$age_for_calib, c(17, 39, 59, 100), labels = c("18-39", "40-59", ">=60"))

set.seed(2026); K <- 5; R <- 50
cmp <- list()
for (nm in names(forms)) {
  gap_mat <- matrix(NA, nrow(d), R)
  for (r in 1:R) {
    folds <- sample(rep(1:K, length.out = nrow(d)))
    for (k in 1:K) {
      fit <- lm(forms[[nm]], data = d[folds != k, ])
      gap_mat[folds == k, r] <- d$pred_age_for_calib[folds == k] -
        predict(fit, newdata = d[folds == k, ])
    }
  }
  d[[paste0("gap_", nm)]] <- rowMeans(gap_mat)
  cell <- d %>% group_by(age_grp, sex_for_calib_factor) %>%
    summarise(m = mean(.data[[paste0("gap_", nm)]]), .groups = "drop")
  yf <- d$sex_for_calib_factor == "Female" & d$age_for_calib <= 45
  full <- lm(forms[[nm]], data = d)
  cmp[[nm]] <- data.frame(model = nm,
    cv_rmse = mean(sqrt(colMeans(gap_mat^2))),
    max_abs_cell_bias = max(abs(cell$m)),
    young_female_mean = mean(d[[paste0("gap_", nm)]][yf]),
    AIC = AIC(full), BIC = BIC(full))
}
cmp_tab <- do.call(rbind, cmp)
print(cmp_tab, digits = 4)
write.csv(cmp_tab, file.path(out_dir, "S_model_comparison.csv"), row.names = FALSE)

## ===== 3. 步骤 1：M3 在全测试集上的最终系数 =====
m3 <- lm(forms$M3_quad_int, data = d)
options(digits = 10)
print(coef(m3))                      # 6 个系数，之后与脚本 A 的输出核对
write.csv(data.frame(term = names(coef(m3)), estimate = unname(coef(m3))),
          file.path(out_dir, "M3_coefficients_from_diag.csv"), row.names = FALSE)
options(digits = 7)

## ===== 4. 步骤 2：10 岁分档 × 性别检查（交叉拟合 gap）=====
summ_tab <- function(gapcol) {
  d %>% mutate(age_dec = cut(age_for_calib, c(17, 29, 39, 49, 59, 69, 100),
         labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))) %>%
    group_by(age_dec, sex_for_calib_factor) %>%
    summarise(n = n(), mean = mean(.data[[gapcol]]), sd = sd(.data[[gapcol]]),
              #lo = mean - 1.96 * sd / sqrt(n), hi = mean + 1.96 * sd / sqrt(n),
              lo = mean - qt(0.975, n - 1) * sd / sqrt(n),hi = mean + qt(0.975, n - 1) * sd / sqrt(n),
              .groups = "drop") %>% as.data.frame()
}
tab_M3 <- summ_tab("gap_M3_quad_int"); print(tab_M3, digits = 3)
tab_M2 <- summ_tab("gap_M2_int_linear")         # M2 作为敏感性分析留档
write.csv(tab_M3, file.path(out_dir, "S_strata_10y_M3.csv"), row.names = FALSE)
write.csv(tab_M2, file.path(out_dir, "S_strata_10y_M2.csv"), row.names = FALSE)

key <- function(sx, lo, hi){
  g <- d$gap_M3_quad_int[d$sex_for_calib_factor == sx & d$age_for_calib >= lo & d$age_for_calib <= hi]
  m <- mean(g); s <- sd(g); n <- length(g);lo1 <- m - qt(0.975, n - 1) * s / sqrt(n);hi1 <- m + qt(0.975, n - 1) * s / sqrt(n)
  sprintf("%s %d-%d: n=%d, mean=%.2f, SD=%.2f, 95%%CI [%.2f, %.2f]",
          sx, lo, hi, n, m, s,lo1,hi1)
}
#m - 1.96 * s / sqrt(n), m + 1.96 * s / sqrt(n))
cat(key("Female", 18, 45), "\n")      # 对应 SLE
cat(key("Male",   18, 45), "\n")      # 对应 HIV
cat(sprintf("Overall (cross-fitted M3): mean=%.3f, SD=%.3f  <- SD 即 z 分数标尺\n",
            mean(d$gap_M3_quad_int), sd(d$gap_M3_quad_int)))

## ===== 5. 步骤 3：疾病队列年龄范围 + 期望曲线 =====
hiv <- read_any(hiv_file); sle <- read_any(sle_file)
for (nm in c("HIV", "SLE")) {
  a <- suppressWarnings(as.numeric((if (nm == "HIV") hiv else sle)[[age_col_dis]]))
  cat(sprintf("%s: 年龄范围 %.1f–%.1f, <18 岁 %d 人, >88 岁 %d 人, 缺失 %d 人\n", nm,
      min(a, na.rm = TRUE), max(a, na.rm = TRUE),
      sum(a < 18, na.rm = TRUE), sum(a > 88, na.rm = TRUE), sum(is.na(a))))
}
grid <- expand.grid(age_for_calib = seq(18, 88, 0.5),
                    sex_for_calib_factor = factor(c("Female", "Male"), levels = c("Female", "Male")))
grid$expected <- predict(m3, newdata = grid)
p <- ggplot() +
  geom_point(data = d, aes(age_for_calib, pred_age_for_calib, colour = sex_for_calib_factor),
             alpha = 0.25, size = 0.8) +
  geom_line(data = grid, aes(age_for_calib, expected, colour = sex_for_calib_factor), linewidth = 1) +
  geom_abline(linetype = 2, colour = "grey40") +
  scale_colour_manual(values = c(Female = "#E2745E", Male = "#4C9AC9")) +
  labs(x = "Chronological age (years)", y = "Predicted age (years)", colour = NULL) +
  theme_bw()
ggsave(file.path(out_dir, "S_expected_curve_M3.pdf"), p, width = 4.5, height = 4)

## ===== 6. 保存交叉拟合 gap，供 Python 作图 =====
out <- d %>% transmute(age_for_calib,
  sex = as.character(sex_for_calib_factor), pred_age_for_calib,
  gap_cf = gap_M3_quad_int, gap_cf_M2 = gap_M2_int_linear)
write.csv(out, file.path(out_dir, "test_gap_crossfit_M3.csv"), row.names = FALSE)
cat("\n诊断完成，输出在文件夹:", out_dir, "\n")