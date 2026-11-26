#################################data of SLE#########################################
library(dplyr)
# library(readxl)
library(jsonlite)

in_SLE_info <- read.csv("E:/backup/data/xiehe-lung/physical_test20251017/数据特征统一-89项-20260630-李亚琪/SLEDAI_results-住院-李晓-20260630.csv",
                        colClasses = c("患者id" = "character","就诊id" = "character"),header = T,fileEncoding = "GB18030")
in_SLE_info_matr <- as.matrix(in_SLE_info)

in_SLE_data <- read.csv("E:/backup/data/xiehe-lung/physical_test20251017/数据特征统一-89项-20260630-李亚琪/new_in_data_0703.csv",
                        colClasses = c("患者id" = "character","就诊id" = "character"),header = T,fileEncoding = "GB18030",check.names = F)
in_SLE_data_matr <- as.matrix(in_SLE_data)

model_data <- read.csv("C:/Users/NCBA/Desktop/model_features.csv",header = T,fileEncoding = "GB18030",check.names = F)

model_data <- as.matrix(model_data)


# ----------------------------------------------------------------------
# 1. 环境准备与数据假设
# ----------------------------------------------------------------------
# 假设以下三个对象已存在于 R 环境中：
#   in_SLE_data_matr  : 原始数据矩阵（或数据框）
#   in_SLE_info_matr  : 信息矩阵（或数据框）
#   model_features    : 包含目标特征列名的矩阵（或向量）
# 注意：如果它们是 matrix 类型，建议先转换为 data.frame 以便于列操作。
# 本代码默认将它们转为 data.frame，并保留列名。
# 转换为 data.frame（如果已经是 data.frame 则无影响）
data_df <- as.data.frame(in_SLE_data_matr, stringsAsFactors = FALSE)
info_df <- as.data.frame(in_SLE_info_matr, stringsAsFactors = FALSE)
model_features <- colnames(model_data)  # 确保是字符向量

# # 定义共有的列名
# patient_id_col <- "患者id"    # 例如 "patient_id"
# visit_id_col   <- "就诊id"    # 例如 "visit_id"
# age_col        <- "年龄"      # 假设年龄在 info_df 中
# 
# # 第 6 列是分级描述列，注意 R 索引从 1 开始
# level_col_index <- 6
# 
# # ----------------------------------------------------------------------
# # 2. 过滤 info 矩阵：去除第 6 列为空或 NA 的行
# # ----------------------------------------------------------------------
# info_clean <- info_df[!is.na(info_df[, level_col_index]) & 
#                         info_df[, level_col_index] != "", , drop = FALSE]

# 定义共有的列名
patient_id_col <- "患者id"    # 例如 "patient_id"
visit_id_col   <- "就诊id"    # 例如 "visit_id"
age_col        <- "年龄"      # 假设年龄在 info_df 中

# 第 5 列是SLE得分列，注意 R 索引从 1 开始
level_col_index <- 5

# ----------------------------------------------------------------------
# 2. 过滤 info 矩阵：去除第 5 列为空或 NA 的行
# ----------------------------------------------------------------------
info_clean <- info_df[!is.na(info_df[, level_col_index]) & 
                        info_df[, level_col_index] != "", , drop = FALSE]

# ----------------------------------------------------------------------
# 3. 定义 SLE 分级映射
# ----------------------------------------------------------------------
info_clean[, level_col_index] <- as.numeric(info_clean[, level_col_index])
# 按区间映射为 SLE_level
info_clean$SLE_level <- ifelse(info_clean[, level_col_index] <= 6, 1,
                               ifelse(info_clean[, level_col_index] <= 12, 2, 3))

# 检查是否有未分级的缺失值（NA）并给出提示
if (any(is.na(info_clean$SLE_level))) {
  warning("存在无法分级的行（第5列为NA），已将 SLE_level 设为 NA，请检查数据。")
}
#####################here

# info_clean <- info_clean[!is.na(info_clean[, 2]) & 
#                            info_clean[, 2] != "", , drop = FALSE]
# 
# info_clean <- info_clean[!is.na(info_clean[, 3]) & 
#                            info_clean[, 3] != "", , drop = FALSE]

# ----------------------------------------------------------------------
# 3. 定义 SLE 分级映射
# ----------------------------------------------------------------------
# level_map <- c(
#   "病情基本无活动" = 1,
#   "轻度活动"       = 2,
#   "中度活动"       = 3,
#   "重度活动"       = 4
# )
# 
# # 将第 6 列转换为数值等级
# info_clean$SLE_level <- level_map[info_clean[, level_col_index]]
# 
# # 检查是否有未映射的值（如有则赋 NA 并警告）
# if (any(is.na(info_clean$SLE_level))) {
#   warning("存在未映射的分级描述，已设为 NA，请检查映射字典。")
# }
# 
# # ----------------------------------------------------------------------
# # 4. 根据就诊 id 匹配数据，构建新矩阵
# # ----------------------------------------------------------------------
# # 新矩阵的列名：SLE_level + model_features 的所有列名
# new_colnames <- c("SLE_level", model_features)
# 
# # 初始化一个空列表，用于逐行存储结果
# result_list <- list()
# 
# # 遍历 info_clean 的每一行（每个就诊 id）
# for (i in seq_len(nrow(info_clean))) {
#   visit_id_val <- info_clean[i, visit_id_col]
#   sle_level    <- info_clean[i, "SLE_level"]
#   
#   # 在 data_df 中找到该就诊 id 对应的行（假设唯一，取第一个匹配）
#   match_rows <- which(data_df[[visit_id_col]] == visit_id_val)
#   if (length(match_rows) == 0) {
#     # 若未找到，则整行填 NA（除了 SLE_level）
#     row_data <- rep(NA, length(model_features))
#     names(row_data) <- model_features
#   } else {
#     # 取第一个匹配行
#     row_idx <- match_rows[1]
#     # 提取 model_features 中存在的列，不存在的设为 NA
#     row_data <- sapply(model_features, function(col) {
#       if (col %in% colnames(data_df)) {
#         data_df[row_idx, col]
#       } else {
#         NA
#       }
#     })
#   }
#   # 组合 SLE_level 和特征数据
#   full_row <- c(SLE_level = sle_level, row_data)
#   result_list[[i]] <- full_row
# }
# 
# # 合并为矩阵（或 data.frame），列名即为 new_colnames
# result_mat <- do.call(rbind, result_list)
# colnames(result_mat) <- new_colnames
# 
# result_mat <- result_mat[-which(is.na(result_mat[,3])),]
# 
# for (i in 1:nrow(result_mat)){
#   if (result_mat[i,3]=="男"){
#     result_mat[i,3] <- 1
#   }
#   else if (result_mat[i,3]=="女"){
#     result_mat[i,3] <- 0
#   }
#   else{
#     message("性别缺失，i===============",i)
#   }
# }
# # 若希望结果是 matrix 类型，可强制转换（但混合类型可能转为字符）
# # result_mat <- as.matrix(result_mat) 
# 
# # 查看结果前几行
# head(result_mat)
# 
# 
# 
# # ----------------------------------------------------------------------
# # 5. 缺失数据分析（识别缺失严重的个体）
# # ----------------------------------------------------------------------
# # 计算每行（除 SLE_level 外）的 NA 个数和比例
# key_features <- c("CD8+CD38+/CD8+%","CD8+DR+/CD8+%","CD8+T细胞绝对值","钠","尿素",
#                   "CD4/CD8%","ALT","前白蛋白","CD8+CD28+/CD8+%","尿酸","记忆CD4+T细胞%","甘油三酯")
# 
# key_data <- result_mat[,which(colnames(result_mat)%in%key_features,T)]
# na_index_to_rm <- unique(which(is.na(key_data)==T,T)[,1])
# valid_result_mat <- result_mat[-na_index_to_rm,]
# ofr_age_index_to_rm <- c(which(as.numeric(valid_result_mat[,"年龄"])>70,T),which(as.numeric(valid_result_mat[,"年龄"])<20,T))
# valid_result_mat1 <- valid_result_mat[-ofr_age_index_to_rm,]
# valid_result_df <- data.frame(valid_result_mat1, stringsAsFactors = FALSE)
# 
# # ----------------------------------------------------------------------
# # 6. 年龄分布分析（假设年龄列在 valid_result_df  中）
# # ----------------------------------------------------------------------
# if (age_col %in% colnames(valid_result_df)) {
#   age_vec <- as.numeric(valid_result_df[[age_col]])
#   cat("\n年龄分布描述：\n")
#   print(summary(age_vec))
#   # 绘制直方图（若需要）
#   hist(age_vec, main = "年龄分布", xlab = "年龄", col = "lightblue")
# } else {
#   cat("注意：在 info 矩阵中未找到年龄列，请指定正确的年龄列名。\n")
# }
# 
# hist(as.numeric(valid_result_df$SLE_level),breaks=30)
# new_result_mat <- apply(valid_result_mat1,2,as.numeric)
# colnames(new_result_mat) <- colnames(valid_result_mat1)
# write.csv(new_result_mat,"test_data_of_SLE.csv",,row.names = F,quote=T,fileEncoding = "GB18030")
# # ----------------------------------------------------------------------
# # 7. 输出最终结果
# # ----------------------------------------------------------------------
# # result_mat 和 result_df 都包含了 SLE_level 和所有特征
# # 您可根据需要保存或继续分析





# 假设已经执行完前面的步骤（info_clean 已构建，model_features 已定义）
# 定义列名常量
# patient_id_col <- "患者id"
# visit_id_col   <- "就诊id"
# age_col        <- "年龄"     # 注意：此列必须存在于 model_features 中

# -------------------- 按就诊id去重，保留 HappenDateTime 最早的一条 --------------------
# 第4列是 JSON 字符串，包含 "HappenDateTime"
json_col_index <- 4

# 提取 HappenDateTime（解析 JSON）
# info_clean$HappenDateTime <- sapply(info_clean[, json_col_index], function(x) {
#   if (is.na(x) || x == "") return(NA)
#   tryCatch({
#     json <- fromJSON(x)
#     json$HappenDateTime
#   }, error = function(e) NA)
# })
info_clean$HappenDateTime <- sapply(info_clean[, json_col_index], function(x) {
  if (is.na(x) || x == "") return(NA)
  # 匹配模式："HappenDateTime": "任意非引号字符（时间）"
  pattern <- '"HappenDateTime":\\s*"([^"]+)"'
  match <- regmatches(x, regexec(pattern, x))
  if (length(match[[1]]) > 1) {
    return(match[[1]][2])
  } else {
    return(NA)
  }
})

# 按就诊id分组，保留 HappenDateTime 最小（最早）的一行
# 先按时间升序排序，然后按就诊id去重（保留第一个）
info_clean <- info_clean[order(info_clean$HappenDateTime, na.last = TRUE), ]
info_clean <- info_clean[!duplicated(info_clean[[visit_id_col]]), ]

# 删除临时列（可选）
info_clean$HappenDateTime <- NULL

# -------------------- 构建带 ID 的 result_mat --------------------
new_colnames <- c("SLE_level", patient_id_col, visit_id_col, model_features)

result_list <- list()
for (i in seq_len(nrow(info_clean))) {
  visit_id_val <- info_clean[i, visit_id_col]
  patient_id_val <- info_clean[i, patient_id_col]
  sle_level    <- info_clean[i, "SLE_level"]
  
  match_rows <- which(data_df[[visit_id_col]] == visit_id_val)
  if (length(match_rows) == 0) {
    # 若找不到匹配的行，填充 NA（但不会生成多余行，因为 info_clean 已去重）
    row_data <- rep(NA, length(model_features))
    names(row_data) <- model_features
  } else {
    row_idx <- match_rows[1]
    row_data <- sapply(model_features, function(col) {
      if (col %in% colnames(data_df)) {
        data_df[row_idx, col]
      } else {
        NA
      }
    })
  }
  full_row <- c(SLE_level = sle_level,
                patient_id = patient_id_val,
                visit_id = visit_id_val,
                row_data)
  result_list[[i]] <- full_row
}

result_mat <- do.call(rbind, result_list)
colnames(result_mat) <- new_colnames

# 查看结果
head(result_mat)


result_mat <- result_mat[-which(is.na(result_mat[,5])),]
for (i in 1:nrow(result_mat)){
  if (result_mat[i,5]=="男"){
    result_mat[i,5] <- 1
  }
  else if (result_mat[i,5]=="女"){
    result_mat[i,5] <- 0
  }
  else{
    # result_mat[i,5] <- NA
    message("性别缺失，i===============",i)
  }
}



features <- unique(colnames(result_mat))

feature_count <- matrix(0,length(features),2)

for (i in 1:length(features)){
  temp_index <- which(colnames(result_mat)==features[i],T)
  feature_count[i,1] <- features[i]
  if (length(temp_index)>0){
    non_na_no <- length(which(is.na(result_mat[,temp_index])==F,T))
    feature_count[i,2] <- non_na_no
  }
  else{
    feature_count[i,2] <- 0
    message(feature_count[i,1])
  }
}

sorted_feature_count <- feature_count[order(as.numeric(feature_count[,2]),decreasing = TRUE),]

all_items <- sorted_feature_count[,1]
all_count10 <- matrix(0,length(all_items),2)
for (i in 1:length(all_items)){
  temp_items <- all_items[1:i]
  temp_all_data <- result_mat[,which(colnames(result_mat)%in%temp_items,T)]
  temp_na_index <- which(is.na(temp_all_data)==T,T)
  if (length(temp_na_index)>0){
    temp_all_na_index <- unique(temp_na_index[,1])
    temp_all_valid_num <- nrow(temp_all_data)-length(temp_all_na_index)
  }
  else{
    temp_all_valid_num <- nrow(result_mat)
  }
  all_count10[i,1] <- all_items[i]
  all_count10[i,2] <- temp_all_valid_num
}
colnames(all_count10) <- c("变量","截至目前可用数据量")

plot(1:nrow(all_count10), as.numeric(all_count10[,2]), type = "l", col = "red", lwd = 2)


SLE_valid_features <- all_count10[1:60,1]
SLE_valid_data <- result_mat[,which(colnames(result_mat)%in%SLE_valid_features,T)]
SLE_na_index <- unique(which(is.na(SLE_valid_data)==T,T)[,1])
new_SLE_valid_data <- SLE_valid_data[-SLE_na_index,]

result_mat_clean <- new_SLE_valid_data



# ----------------------------------------------------------------------
# 5. 缺失数据过滤（基于关键特征）
# ----------------------------------------------------------------------
# key_features <- c("CD8+CD38+/CD8+%", "CD8+DR+/CD8+%", "CD8+T细胞绝对值", "钠", "尿素",
#                   "CD4/CD8%", "ALT", "前白蛋白", "CD8+CD28+/CD8+%", "尿酸", "记忆CD4+T细胞%", "甘油三酯")
# 
# # 确保这些特征在 result_mat 中
# key_cols <- intersect(key_features, colnames(result_mat))
# if (length(key_cols) == 0) warning("未找到任何关键特征列，请检查列名。")
# 
# # 找出关键特征中含有NA的行（任意一个关键特征缺失则剔除）
# na_rows <- apply(result_mat[, key_cols, drop = FALSE], 1, function(x) any(is.na(x)))
# result_mat_clean <- result_mat[!na_rows, ]

# 年龄过滤：删除年龄 >70 或 <20 的行（保留年龄为NA的行）
if (age_col %in% colnames(result_mat_clean)) {
  age_vals <- as.numeric(result_mat_clean[, age_col])
  age_gt70 <- which(age_vals > 70)
  age_lt20 <- which(age_vals < 20)
  age_rows_to_rm <- union(age_gt70, age_lt20)
  if (length(age_rows_to_rm) > 0) {
    result_mat_clean <- result_mat_clean[-age_rows_to_rm, ]
  }
} else {
  warning("年龄列不存在，跳过年龄过滤。")
}

# 此时 result_mat_clean 包含 ID 列、SLE_level 和所有特征
# ----------------------------------------------------------------------
# 6. 提取ID矩阵和数值矩阵
# ----------------------------------------------------------------------
# 提取两列ID矩阵
id_mat <- result_mat_clean[, c(patient_id_col, visit_id_col), drop = FALSE]

# 提取除ID列以外的所有列（包括SLE_level和特征），转为数值矩阵
cols_for_numeric <- setdiff(colnames(result_mat_clean), c(patient_id_col, visit_id_col))
numeric_df <- as.data.frame(result_mat_clean[, cols_for_numeric, drop = FALSE],
                            stringsAsFactors = FALSE)
# 将所有列强制转为数值（非数值变为NA）
numeric_df[] <- lapply(numeric_df, as.numeric)

new_result_mat <- as.matrix(numeric_df)
colnames(new_result_mat) <- cols_for_numeric

# 查看最终结果
head(new_result_mat)
head(id_mat)

# 可选：输出年龄分布（使用 new_result_mat 中的年龄列）
if (age_col %in% colnames(new_result_mat)) {
  age_vec <- new_result_mat[, age_col]
  cat("\n年龄分布描述：\n")
  print(summary(age_vec))
  hist(age_vec, main = "年龄分布", xlab = "年龄", col = "lightblue")
} else {
  cat("注意：年龄列不在数值矩阵中。\n")
}

# 现在您有两个对象：
#   new_result_mat : 纯数值矩阵（包含SLE_level和所有特征）
#   id_mat         : 两列矩阵（患者ID、就诊ID），行与 new_result_mat 一一对应
options(scipen = 999)  # 禁止科学计数法

# 保存 ID 矩阵（将 ID 列转为字符，防止 Excel 科学计数）
id_mat_save <- data.frame(id_mat, stringsAsFactors = FALSE)
write.csv(id_mat_save, file = "id_mat.csv", row.names = FALSE, quote = TRUE,fileEncoding = "GB18030")

new_result_mat_with_na <- matrix(NA,nrow(new_result_mat),length(model_features))
for (i in 1:ncol(new_result_mat)){
  temp_index <- which(model_features==colnames(new_result_mat)[i],T)
  if (length(temp_index)==0){
    message("index i======",i,"=====not found")
  }
  else if(length(temp_index)==1){
    new_result_mat_with_na[,temp_index] <- new_result_mat[,i]
  }
  else{
    message("replicates found by index i======",i)
  }
}
new_result_mat_with_na <- cbind(new_result_mat[,1],new_result_mat_with_na)
colnames(new_result_mat_with_na) <- c("SLE_level",model_features)
# 保存数值矩阵
write.csv(new_result_mat_with_na, file = "new_result_mat.csv", row.names = FALSE,fileEncoding = "GB18030")









######################################################################################

# 定义共有的列名
patient_id_col <- "患者id"    # 例如 "patient_id"
visit_id_col   <- "就诊id"    # 例如 "visit_id"
age_col        <- "年龄"      # 假设年龄在 info_df 中

# 第 5 列是SLE得分列，注意 R 索引从 1 开始
level_col_index <- 5

# ----------------------------------------------------------------------
# 2. 过滤 info 矩阵：去除第 5 列为空或 NA 的行
# ----------------------------------------------------------------------
info_clean <- info_df[!is.na(info_df[, level_col_index]) & 
                        info_df[, level_col_index] != "", , drop = FALSE]

# ----------------------------------------------------------------------
# 3. 定义 SLE 分级映射
# ----------------------------------------------------------------------
info_clean[, level_col_index] <- as.numeric(info_clean[, level_col_index])
# 按区间映射为 SLE_level
info_clean$SLE_level <- ifelse(info_clean[, level_col_index] <= 6, 1,
                               ifelse(info_clean[, level_col_index] <= 12, 2, 3))

# 检查是否有未分级的缺失值（NA）并给出提示
if (any(is.na(info_clean$SLE_level))) {
  warning("存在无法分级的行（第5列为NA），已将 SLE_level 设为 NA，请检查数据。")
}
#####################here


info_clean$HappenDateTime <- sapply(info_clean[, json_col_index], function(x) {
  if (is.na(x) || x == "") return(NA)
  # 匹配模式："HappenDateTime": "任意非引号字符（时间）"
  pattern <- '"HappenDateTime":\\s*"([^"]+)"'
  match <- regmatches(x, regexec(pattern, x))
  if (length(match[[1]]) > 1) {
    return(match[[1]][2])
  } else {
    return(NA)
  }
})

# 按就诊id分组，保留 HappenDateTime 最小（最早）的一行
# 先按时间升序排序，然后按就诊id去重（保留第一个）
info_clean <- info_clean[order(info_clean$HappenDateTime, na.last = TRUE), ]
info_clean <- info_clean[!duplicated(info_clean[[visit_id_col]]), ]

# 删除临时列（可选）
info_clean$HappenDateTime <- NULL

# -------------------- 构建带 ID 的 result_mat --------------------
new_colnames <- c("SLE_level", patient_id_col, visit_id_col, model_features)

result_list <- list()
for (i in seq_len(nrow(info_clean))) {
  visit_id_val <- info_clean[i, visit_id_col]
  patient_id_val <- info_clean[i, patient_id_col]
  sle_level    <- info_clean[i, "SLE_level"]
  
  match_rows <- which(data_df[[visit_id_col]] == visit_id_val)
  if (length(match_rows) == 0) {
    # 若找不到匹配的行，填充 NA（但不会生成多余行，因为 info_clean 已去重）
    row_data <- rep(NA, length(model_features))
    names(row_data) <- model_features
  } else {
    row_idx <- match_rows[1]
    row_data <- sapply(model_features, function(col) {
      if (col %in% colnames(data_df)) {
        data_df[row_idx, col]
      } else {
        NA
      }
    })
  }
  full_row <- c(SLE_level = sle_level,
                patient_id = patient_id_val,
                visit_id = visit_id_val,
                row_data)
  result_list[[i]] <- full_row
}

result_mat <- do.call(rbind, result_list)
colnames(result_mat) <- new_colnames

# 查看结果
head(result_mat)


result_mat <- result_mat[-which(is.na(result_mat[,5])),]
for (i in 1:nrow(result_mat)){
  if (result_mat[i,5]=="男"){
    result_mat[i,5] <- 1
  }
  else if (result_mat[i,5]=="女"){
    result_mat[i,5] <- 0
  }
  else{
    # result_mat[i,5] <- NA
    message("性别缺失，i===============",i)
  }
}



features <- unique(colnames(result_mat))

feature_count <- matrix(0,length(features),2)

for (i in 1:length(features)){
  temp_index <- which(colnames(result_mat)==features[i],T)
  feature_count[i,1] <- features[i]
  if (length(temp_index)>0){
    non_na_no <- length(which(is.na(result_mat[,temp_index])==F,T))
    feature_count[i,2] <- non_na_no
  }
  else{
    feature_count[i,2] <- 0
    message(feature_count[i,1])
  }
}

sorted_feature_count <- feature_count[order(as.numeric(feature_count[,2]),decreasing = TRUE),]

all_items <- sorted_feature_count[,1]
all_count10 <- matrix(0,length(all_items),2)
for (i in 1:length(all_items)){
  temp_items <- all_items[1:i]
  temp_all_data <- result_mat[,which(colnames(result_mat)%in%temp_items,T)]
  temp_na_index <- which(is.na(temp_all_data)==T,T)
  if (length(temp_na_index)>0){
    temp_all_na_index <- unique(temp_na_index[,1])
    temp_all_valid_num <- nrow(temp_all_data)-length(temp_all_na_index)
  }
  else{
    temp_all_valid_num <- nrow(result_mat)
  }
  all_count10[i,1] <- all_items[i]
  all_count10[i,2] <- temp_all_valid_num
}
colnames(all_count10) <- c("变量","截至目前可用数据量")

plot(1:nrow(all_count10), as.numeric(all_count10[,2]), type = "l", col = "red", lwd = 2)


SLE_valid_features <- all_count10[1:60,1]
SLE_valid_data <- result_mat[,which(colnames(result_mat)%in%SLE_valid_features,T)]
SLE_na_index <- unique(which(is.na(SLE_valid_data)==T,T)[,1])
new_SLE_valid_data <- SLE_valid_data[-SLE_na_index,]

result_mat_clean <- new_SLE_valid_data



# ----------------------------------------------------------------------
# 5. 缺失数据过滤（基于关键特征）
# ----------------------------------------------------------------------
# key_features <- c("CD8+CD38+/CD8+%", "CD8+DR+/CD8+%", "CD8+T细胞绝对值", "钠", "尿素",
#                   "CD4/CD8%", "ALT", "前白蛋白", "CD8+CD28+/CD8+%", "尿酸", "记忆CD4+T细胞%", "甘油三酯")
# 
# # 确保这些特征在 result_mat 中
# key_cols <- intersect(key_features, colnames(result_mat))
# if (length(key_cols) == 0) warning("未找到任何关键特征列，请检查列名。")
# 
# # 找出关键特征中含有NA的行（任意一个关键特征缺失则剔除）
# na_rows <- apply(result_mat[, key_cols, drop = FALSE], 1, function(x) any(is.na(x)))
# result_mat_clean <- result_mat[!na_rows, ]

# 年龄过滤：删除年龄 >70 或 <20 的行（保留年龄为NA的行）
if (age_col %in% colnames(result_mat_clean)) {
  age_vals <- as.numeric(result_mat_clean[, age_col])
  age_gt70 <- which(age_vals > 70)
  age_lt20 <- which(age_vals < 20)
  age_rows_to_rm <- union(age_gt70, age_lt20)
  if (length(age_rows_to_rm) > 0) {
    result_mat_clean <- result_mat_clean[-age_rows_to_rm, ]
  }
} else {
  warning("年龄列不存在，跳过年龄过滤。")
}

# 此时 result_mat_clean 包含 ID 列、SLE_level 和所有特征
# ----------------------------------------------------------------------
# 6. 提取ID矩阵和数值矩阵
# ----------------------------------------------------------------------
# 提取两列ID矩阵
id_mat <- result_mat_clean[, c(patient_id_col, visit_id_col), drop = FALSE]

# 提取除ID列以外的所有列（包括SLE_level和特征），转为数值矩阵
cols_for_numeric <- setdiff(colnames(result_mat_clean), c(patient_id_col, visit_id_col))
numeric_df <- as.data.frame(result_mat_clean[, cols_for_numeric, drop = FALSE],
                            stringsAsFactors = FALSE)
# 将所有列强制转为数值（非数值变为NA）
numeric_df[] <- lapply(numeric_df, as.numeric)

new_result_mat <- as.matrix(numeric_df)
colnames(new_result_mat) <- cols_for_numeric

# 查看最终结果
head(new_result_mat)
head(id_mat)
id_mat_bac <- id_mat

#转换为原始id
for (i in 1:nrow(id_mat)){
  temp_id1 <- id_mat[i,1]
  id_mat[i,1] <- tryCatch({
    decode_general(temp_id1)
  }, error = function(e) temp_id1)
  temp_id2 <- id_mat[i,2]
  id_mat[i,2] <- tryCatch({
    decode_general(temp_id2)
  }, error = function(e) temp_id2)
}




# 可选：输出年龄分布（使用 new_result_mat 中的年龄列）
if (age_col %in% colnames(new_result_mat)) {
  age_vec <- new_result_mat[, age_col]
  cat("\n年龄分布描述：\n")
  print(summary(age_vec))
  hist(age_vec, main = "年龄分布", xlab = "年龄", col = "lightblue")
} else {
  cat("注意：年龄列不在数值矩阵中。\n")
}

# 现在您有两个对象：
#   new_result_mat : 纯数值矩阵（包含SLE_level和所有特征）
#   id_mat         : 两列矩阵（患者ID、就诊ID），行与 new_result_mat 一一对应
options(scipen = 999)  # 禁止科学计数法

# 保存 ID 矩阵（将 ID 列转为字符，防止 Excel 科学计数）
id_mat_save <- data.frame(id_mat, stringsAsFactors = FALSE)
write.csv(id_mat_save, file = "id_mat_3_levels.csv", row.names = FALSE, quote = TRUE,fileEncoding = "GB18030")

new_result_mat_with_na <- matrix(NA,nrow(new_result_mat),length(model_features))
for (i in 1:ncol(new_result_mat)){
  temp_index <- which(model_features==colnames(new_result_mat)[i],T)
  if (length(temp_index)==0){
    message("index i======",i,"=====not found")
  }
  else if(length(temp_index)==1){
    new_result_mat_with_na[,temp_index] <- new_result_mat[,i]
  }
  else{
    message("replicates found by index i======",i)
  }
}
new_result_mat_with_na <- cbind(new_result_mat[,1],new_result_mat_with_na)
colnames(new_result_mat_with_na) <- c("SLE_level",model_features)
# 保存数值矩阵
write.csv(new_result_mat_with_na, file = "new_result_mat_3_levels.csv", row.names = FALSE,fileEncoding = "GB18030")



male_count <- 0
female_count <- 0
for (i in unique(result_mat_clean[,2])){
  temp_sex <- as.numeric(result_mat_clean[which(result_mat_clean[,2]==i,T)[1],5])
  if(temp_sex==1){
    male_count <- male_count+1
  } 
  else{
    female_count <- female_count+1
  }
}
