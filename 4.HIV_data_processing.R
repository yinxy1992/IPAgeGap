# data1 <- read.csv("E:/TB数据第一部分_体检数据/TB细胞亚群8项和11项项目_感染内科_李太生王丽娜_2025022814382078TB数据_第一阶段_体检部分/体检明细.csv")
# data2 <- as.matrix(data1)
# Pumch@2078

library(dplyr)
library(xlsx)
options(scipen = 20)

#### out patient info
data_list <- vector("list",11)
data_path <- "E:/backup/data/xiehe-lung/physical_test20251017/TB数据_感染内科_李太生王丽娜_第二阶段_门诊部分/"

for (i in 1:11){
  temp_file_name <- paste(data_path,"门诊检验",i,"_masked.csv",sep="")
  temp_data <- read.csv(temp_file_name,
                        colClasses = c("患者ID" = "character","就诊ID" = "character"))
  data_list[[i]] <- as.matrix(temp_data)
}

phase2_out_data <- Reduce(rbind,data_list)

out_record <- read.xlsx("E:/backup/data/xiehe-lung/physical_test20251017/TB数据_感染内科_李太生王丽娜_第二阶段_门诊部分/门诊就诊记录_masked.xlsx",
                        colClasses = c("患者ID" = "character","就诊ID" = "character"),sheetName = "脱敏数据")

out_record_data <- as.matrix(out_record)

uni_id <- unique(phase2_out_data[,2])
length(uni_id)

inter_id <- unique(out_record_data[which(out_record_data[,2]%in%uni_id,T),2])
length(inter_id)


data3 <- phase2_out_data[which(phase2_out_data[,2]%in%uni_id,T),]
all_tests <- unique(data3[,c(5,8)])

raw_data_matrix <- matrix(NA,length(inter_id),(nrow(all_tests)+6))
test_name <- c()
for (i in 1:nrow(all_tests)){
  temp_name <- paste(all_tests[i,1],all_tests[i,2],sep="---")
  test_name <- c(test_name,temp_name)
}
colnames(raw_data_matrix) <- c("患者id","就诊id","出生日期","实际到检时间","性别","年龄",test_name)

for (i in 1:length(inter_id)){
  temp_id <- inter_id[i]
  temp_index <- which(data3[,2]==temp_id,T)
  temp_data <- data3[temp_index,]
  temp_patient_id <- unique(temp_data[,1])
  raw_data_matrix[i,2] <- temp_id
  if (length(temp_patient_id)==1){
    raw_data_matrix[i,1] <- temp_patient_id
  }
  else{
    message(i)
    message("不止一个患者id")
  }
  for (j in 1:nrow(temp_data)){
    temp_test <- paste(temp_data[j,5],temp_data[j,8],sep="---")
    if (is.na(temp_data[j,6])){
      raw_data_matrix[i,temp_test] <- temp_data[j,7]
    }
    else{
      raw_data_matrix[i,temp_test] <- temp_data[j,6]
    }
    
  }
  temp_index1 <- which(out_record_data[,2]==temp_id,T)
  if (length(temp_index1)==1){
    raw_data_matrix[i,3] <- out_record_data[temp_index1,"出生日期"]
    raw_data_matrix[i,4] <- out_record_data[temp_index1,"就诊时间"]
    raw_data_matrix[i,5] <- out_record_data[temp_index1,"性别"]
    birthday <- as.numeric(as.Date(raw_data_matrix[i,3]))
    test_day <- as.numeric(as.Date(raw_data_matrix[i,4]))
    b1 <- as.POSIXct(raw_data_matrix[i,3], format="%Y-%m-%d", tz="UTC")
    b2 <- as.POSIXct(raw_data_matrix[i,4], format="%Y-%m-%d %H:%M:%S", tz="UTC")
    temp_age <- length(seq(from = b1, to = b2, by = 'month'))-1
    raw_data_matrix[i,6] <- round(temp_age/12,2)
  }
  else{
    message(i)
  }
}

raw_data_matrix[which(raw_data_matrix=="/",T)] <- NA
write.csv(raw_data_matrix,"raw_data_xiehe_out_patient_data.csv",row.names = F,quote = T,fileEncoding = "GB18030")

write.csv(all_tests,"all_test_items_of_xiehe_out_patient_data.csv",row.names = F,quote = T,fileEncoding = "GB18030")

count_matrix <- matrix(0,ncol(raw_data_matrix),3)
for (i in 1:ncol(raw_data_matrix)){
  temp_item <- colnames(raw_data_matrix)[i]
  count_matrix[i,1] <- temp_item
  count_matrix[i,2] <- nrow(raw_data_matrix)
  count_matrix[i,3] <- length(which(is.na(raw_data_matrix[,i])==F,T))
}

write.csv(count_matrix,"count_matrix_of_xiehe_out_patient_data.csv",row.names = F,quote = T,fileEncoding = "GB18030")

data_info <- cbind(all_tests,count_matrix[7:nrow(count_matrix),-1])
write.csv(data_info,"data_info_of_xiehe_out_patient_data.csv",row.names = F,quote = T,fileEncoding = "GB18030")


bbb <- decode_strings(raw_data_matrix[,1])


HIV_id_data <- read_excel("E:/backup/data/xiehe-lung/physical_test20251017/队列年龄筛选HIV_no_B_Z.xlsx")
HIV_id_matr <- as.matrix(HIV_id_data)
HIV_ids <- unique(toupper(HIV_id_matr[,1]))
length(unique(bbb[which(bbb%in%HIV_ids,T)]))

# encoded_HIV_ids <- encode_strings(HIV_ids)
# encoded_HIV_ids_valid <- setdiff(encoded_HIV_ids,NA)
HIV_raw_data_matrix0 <- raw_data_matrix[which(bbb%in%HIV_ids,T),]
count_duplicates_table <- function(vec) {
  # 使用table统计每个元素出现的次数
  freq_table <- table(vec, useNA = "ifany")
  # 统计每个频率出现的次数
  freq_dist <- table(freq_table, useNA = "ifany")
  # 转换为数据框，便于查看和分析
  result <- data.frame(
    repeats = as.numeric(names(freq_dist)),
    count = as.numeric(freq_dist),
    row.names = NULL
  )
  # 按重复次数排序
  result <- result[order(result$repeats), ]
  return(result)
}
count_stat <- count_duplicates_table(HIV_raw_data_matrix0[,1])

HIV_NA_col_counts <- vector("numeric",ncol(HIV_raw_data_matrix0))
for (i in 1:ncol(HIV_raw_data_matrix0)){
  HIV_NA_col_counts[i] <- length(which(is.na(HIV_raw_data_matrix0[,i])==T,T))
}
hist(HIV_NA_col_counts,breaks=100)


HIV_raw_data_matrix <- HIV_raw_data_matrix0[,which(HIV_NA_col_counts<(nrow(HIV_raw_data_matrix0)-10),T)]
HIV_data_items <- matrix("",ncol(HIV_raw_data_matrix),2)
HIV_data_items[,1] <- colnames(HIV_raw_data_matrix)
for (i in 1:ncol(HIV_raw_data_matrix)){
  HIV_data_items[i,2] <- length(which(is.na(HIV_raw_data_matrix[,i])==F,T))
}
colnames(HIV_data_items) <- c("items","valid_count")
write.csv(HIV_data_items,"HIV_data_items.csv",row.names = F,quote = T,,fileEncoding = "GB18030")



HIV_filtered_matr <- matrix(NA,nrow(HIV_raw_data_matrix),ncol(HIV_raw_data_matrix))
HIV_filtered_colnames <- c()
HIV_count <- 0

### 患者id
patient_ids <- decode_strings(HIV_raw_data_matrix[,1])

### 就诊id
visit_ids <- decode_strings(HIV_raw_data_matrix[,2])

### 出生日期
for (i in 1:nrow(HIV_raw_data_matrix)){
  temp_date <- HIV_raw_data_matrix[i,"出生日期"]
  birthday <- as.numeric(as.Date(temp_date))
  HIV_filtered_matr[i,(HIV_count+1)] <- birthday
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"出生日期")

### 实际到检时间
for (i in 1:nrow(HIV_raw_data_matrix)){
  temp_date <- HIV_raw_data_matrix[i,"实际到检时间"]
  test_day <- as.numeric(as.Date(temp_date))
  HIV_filtered_matr[i,(HIV_count+1)] <- test_day
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"实际到检时间")

### 性别
for (i in 1:nrow(HIV_raw_data_matrix)){
  temp_sex <- HIV_raw_data_matrix[i,"性别"]
  if (temp_sex=="男"){
    HIV_filtered_matr[i,(HIV_count+1)] <- 1
  }
  else if (temp_sex=="女"){
    HIV_filtered_matr[i,(HIV_count+1)] <- 0
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"性别")

### 年龄
for (i in 1:nrow(HIV_raw_data_matrix)){
  HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,"年龄"])
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"年龄")

### 45RA+CD4+T细胞%
grep("45RA",colnames(HIV_raw_data_matrix))###104,119
# View(HIV_raw_data_matrix[,grep("45RA",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,104])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,104])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,104])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"45RA+CD4+T细胞%")

### 45RA+CD4+T细胞计数
grep("45RA",colnames(HIV_raw_data_matrix))###104,119
# View(HIV_raw_data_matrix[,grep("45RA",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,119])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,119])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,119])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"45RA+CD4+T细胞计数")

### B淋巴细胞CD19%
grep("B淋巴细胞CD19",colnames(HIV_raw_data_matrix))###47 74
# View(HIV_raw_data_matrix[,grep("B淋巴细胞CD19",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,74])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,74])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,74])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"B淋巴细胞CD19%")

### B淋巴细胞CD19计数
grep("B淋巴细胞CD19",colnames(HIV_raw_data_matrix))###47 74
# View(HIV_raw_data_matrix[,grep("B淋巴细胞CD19",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,47])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,47])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,47])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"B淋巴细胞CD19计数")

### CD4/CD8
grep("CD4/CD8",colnames(HIV_raw_data_matrix))###78
# View(HIV_raw_data_matrix[,grep("CD4/CD8",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,78])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,78])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,78])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD4/CD8")

### CD4+CD28+/CD4+%
grep("+CD28+",colnames(HIV_raw_data_matrix))###12 21 22 73
# View(HIV_raw_data_matrix[,grep("+CD28+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,73])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,73])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,73])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD4+CD28+/CD4+%")

### CD4+CD28+计数
grep("+CD28+",colnames(HIV_raw_data_matrix))###12 21 22 73
# View(HIV_raw_data_matrix[,grep("+CD28+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,22])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,22])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,22])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD4+CD28+计数")

### CD4+T细胞%
grep("T细胞",colnames(HIV_raw_data_matrix))###31
# View(HIV_raw_data_matrix[,grep("T细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,31])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,31])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,31])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD4+T细胞%")

### CD4+T细胞计数
grep("T细胞",colnames(HIV_raw_data_matrix))###43
# View(HIV_raw_data_matrix[,grep("T细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,43])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,43])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,43])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD4+T细胞计数")

### CD8+CD28+/CD8+%
grep("+CD28+",colnames(HIV_raw_data_matrix))###12
# View(HIV_raw_data_matrix[,grep("+CD28+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,12])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,12])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,12])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+CD28+/CD8+%")

### CD8+CD28+计数
grep("+CD28+",colnames(HIV_raw_data_matrix))###21
# View(HIV_raw_data_matrix[,grep("+CD28+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,21])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,21])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,21])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+CD28+计数")

### CD8+CD38+/CD8+%
grep("+CD38+",colnames(HIV_raw_data_matrix))###82
# View(HIV_raw_data_matrix[,grep("+CD38+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,82])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,82])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,82])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+CD38+/CD8+%")

### CD8+CD38+计数
grep("+CD38+",colnames(HIV_raw_data_matrix))###72
# View(HIV_raw_data_matrix[,grep("+CD38+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,72])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,72])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,72])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+CD38+计数")

### CD8+DR+/CD8+%
grep("+DR+",colnames(HIV_raw_data_matrix))###69
# View(HIV_raw_data_matrix[,grep("+DR+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,69])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,69])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,69])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+DR+/CD8+%")

### CD8+DR+计数
grep("+DR+",colnames(HIV_raw_data_matrix))###10
# View(HIV_raw_data_matrix[,grep("+DR+",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,10])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,10])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,10])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+DR+计数")

### CD8+T细胞%
grep("T细胞",colnames(HIV_raw_data_matrix))###34
# View(HIV_raw_data_matrix[,grep("T细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,34])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,34])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,34])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+T细胞%")

### CD8+T细胞计数
grep("T细胞",colnames(HIV_raw_data_matrix))###7
# View(HIV_raw_data_matrix[,grep("T细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,7])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,7])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,7])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"CD8+T细胞计数")

### C反应蛋白(超敏)
grep("C反应蛋白",colnames(HIV_raw_data_matrix))###27,343
# View(HIV_raw_data_matrix[,grep("C反应蛋白",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,27])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,27])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,27])
  }
  else if (!is.na(HIV_raw_data_matrix[i,343])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,343])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,343])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"C反应蛋白(超敏)")

### D-二聚体
grep("D-二聚体",colnames(HIV_raw_data_matrix))###141,363
# View(HIV_raw_data_matrix[,grep("D-二聚体",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,141])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,141])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,363])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,363])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"D-二聚体")

library(stringr)

# 提取包含科学计数法的数字字符串
extract_numbers <- function(x) {
  # 匹配科学计数法和常规数字
  pattern <- "[-+]?\\d*\\.?\\d+([eE][-+]?\\d+)?"
  str_extract(x, pattern)
}

# 转换为数值
convert_to_numeric <- function(x) {
  nums <- extract_numbers(x)
  # 转换，R会自动处理科学计数法
  as.numeric(nums)
}

### HIV-1病毒载量
grep("HIV-1病毒载量",colnames(HIV_raw_data_matrix))###109,140,462
# View(HIV_raw_data_matrix[,grep("HIV-1病毒载量",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,109])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,109])
    temp_value <- convert_to_numeric(extracted)
    temp_value1 <- grep("未测", HIV_raw_data_matrix[i,109])
    if (length(temp_value)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
    }
    else if (length(temp_value1)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- 0
    }
  }
  else if (!is.na(HIV_raw_data_matrix[i,140])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,140])
    temp_value <- convert_to_numeric(extracted)
    temp_value1 <- grep("未测", HIV_raw_data_matrix[i,140])
    if (length(temp_value)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
    }
    else if (length(temp_value1)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- 0
    }
  }
  else if (!is.na(HIV_raw_data_matrix[i,462])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,462])
    temp_value <- convert_to_numeric(extracted)
    temp_value1 <- grep("未测", HIV_raw_data_matrix[i,462])
    if (length(temp_value)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
    }
    else if (length(temp_value1)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- 0
    }
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"HIV-1病毒载量")

### T淋巴细胞CD3%
grep("T淋巴细胞CD3",colnames(HIV_raw_data_matrix))###29
# View(HIV_raw_data_matrix[,grep("T淋巴细胞CD3",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,29])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,29])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,29])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"T淋巴细胞CD3%")

### T淋巴细胞CD3计数
grep("T淋巴细胞CD3",colnames(HIV_raw_data_matrix))###44
# View(HIV_raw_data_matrix[,grep("T淋巴细胞CD3",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,44])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,44])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,44])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"T淋巴细胞CD3计数")

### 白蛋白
grep("白蛋白",colnames(HIV_raw_data_matrix))###54,125,342,420
# View(HIV_raw_data_matrix[,grep("白蛋白",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,54])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,54])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,54])
  }
  else if (!is.na(HIV_raw_data_matrix[i,125])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,125])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,125])
  }
  else if (!is.na(HIV_raw_data_matrix[i,342])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,342])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,342])
  }
  else if (!is.na(HIV_raw_data_matrix[i,420])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,420])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,420])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"白蛋白")

### 白蛋白球蛋白比
grep("白蛋白球蛋白比",colnames(HIV_raw_data_matrix))###41
# View(HIV_raw_data_matrix[,grep("白蛋白球蛋白比",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,41])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,41])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,41])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"白蛋白球蛋白比")


### 白细胞(中性粒细胞酯酶)
grep("中性粒细胞酯酶",colnames(HIV_raw_data_matrix))###42,329,411,457
# View(HIV_raw_data_matrix[,grep("中性粒细胞酯酶",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,42])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,42])
    temp_value <- convert_to_numeric(extracted)
    temp_value1 <- grep("NEG", HIV_raw_data_matrix[i,42])
    if (length(temp_value)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
    }
    else if (length(temp_value1)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- 0
    }
  }
  else if (!is.na(HIV_raw_data_matrix[i,329])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,329])
    temp_value <- convert_to_numeric(extracted)
    temp_value1 <- grep("NEG", HIV_raw_data_matrix[i,329])
    if (length(temp_value)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
    }
    else if (length(temp_value1)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- 0
    }
  }
  else if (!is.na(HIV_raw_data_matrix[i,411])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,411])
    temp_value <- convert_to_numeric(extracted)
    temp_value1 <- grep("NEG", HIV_raw_data_matrix[i,411])
    if (length(temp_value)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
    }
    else if (length(temp_value1)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- 0
    }
  }
  else if (!is.na(HIV_raw_data_matrix[i,457])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,457])
    temp_value <- convert_to_numeric(extracted)
    temp_value1 <- grep("NEG", HIV_raw_data_matrix[i,457])
    if (length(temp_value)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
    }
    else if (length(temp_value1)>0){
      HIV_filtered_matr[i,(HIV_count+1)] <- 0
    }
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"白细胞(中性粒细胞酯酶)")

### 白细胞
grep("白细胞",colnames(HIV_raw_data_matrix))###35,258,269,403
# View(HIV_raw_data_matrix[,grep("白细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,35])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,35])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,258])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,258])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,269])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,269])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,403])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,403])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"白细胞")

### 白细胞介素6
grep("白细胞介素6",colnames(HIV_raw_data_matrix))###122,384
# View(HIV_raw_data_matrix[,grep("白细胞介素6",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,122])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,122])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,384])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,384])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"白细胞介素6")

### 尿比重
grep("比重",colnames(HIV_raw_data_matrix))###83,321,412
# View(HIV_raw_data_matrix[,grep("比重",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,83])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,83])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,321])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,321])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,412])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,412])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"尿比重")

### 丙氨酸氨基转移酶
grep("丙氨酸氨基转移酶",colnames(HIV_raw_data_matrix))###33,340,437
# View(HIV_raw_data_matrix[,grep("丙氨酸氨基转移酶",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,33])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,33])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,33])
  }
  else if (!is.na(HIV_raw_data_matrix[i,340])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,340])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,340])
  }
  else if (!is.na(HIV_raw_data_matrix[i,437])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,437])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,437])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"丙氨酸氨基转移酶")

### 丙型肝炎抗体
grep("丙型肝炎抗体",colnames(HIV_raw_data_matrix))###132,224,365
# View(HIV_raw_data_matrix[,grep("丙型肝炎抗体",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,132])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,132])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,224])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,224])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,365])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,365])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"丙型肝炎抗体")

### 纯真CD4+T细胞%
grep("纯真CD4",colnames(HIV_raw_data_matrix))###101
# View(HIV_raw_data_matrix[,grep("纯真CD4",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,101])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,101])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,101])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"纯真CD4+T细胞%")

###纯真CD4+T细胞计数
grep("纯真CD4",colnames(HIV_raw_data_matrix))###121
# View(HIV_raw_data_matrix[,grep("纯真CD4",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,121])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,121])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"纯真CD4+T细胞计数")

###大血小板比率
grep("大血小板比率",colnames(HIV_raw_data_matrix))###77
# View(HIV_raw_data_matrix[,grep("大血小板比率",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,77])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,77])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"大血小板比率")

### 单核细胞百分比
grep("单核细胞",colnames(HIV_raw_data_matrix))###50
# View(HIV_raw_data_matrix[,grep("单核细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,50])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,50])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"单核细胞百分比")

###单核细胞计数
grep("单核细胞",colnames(HIV_raw_data_matrix))###90
# View(HIV_raw_data_matrix[,grep("单核细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,90])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,90])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"单核细胞计数")

###胆红素
grep("胆红素",colnames(HIV_raw_data_matrix))[c(2,7,10,12)]###46,326,419,455
# View(HIV_raw_data_matrix[,grep("胆红素",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,46])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,46])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,326])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,326])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,419])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,419])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,455])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,455])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"胆红素")

###胆碱酯酶
grep("胆碱酯酶",colnames(HIV_raw_data_matrix))###120,338
# View(HIV_raw_data_matrix[,grep("胆碱酯酶",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,120])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,120])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,120])
  }
  else if (!is.na(HIV_raw_data_matrix[i,338])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,338])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,338])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"胆碱酯酶")

###蛋白(白蛋白)
grep("白蛋白",colnames(HIV_raw_data_matrix))###13,324,413
# View(HIV_raw_data_matrix[,grep("白蛋白",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,13])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,13])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,324])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,324])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,413])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,413])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"蛋白(白蛋白)")

###低密度脂蛋白胆固醇
grep("低密度脂蛋白胆固醇",colnames(HIV_raw_data_matrix))###98,410,436
# View(HIV_raw_data_matrix[,grep("低密度脂蛋白胆固醇",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,98])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,98])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,98])
  }
  else if (!is.na(HIV_raw_data_matrix[i,410])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,410])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,410])
  }
  else if (!is.na(HIV_raw_data_matrix[i,436])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,436])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,436])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"低密度脂蛋白胆固醇")

###钙
grep("钙",colnames(HIV_raw_data_matrix))###62,354,429
# View(HIV_raw_data_matrix[,grep("钙",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,62])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,62])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,62])
  }
  else if (!is.na(HIV_raw_data_matrix[i,354])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,354])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,354])
  }
  else if (!is.na(HIV_raw_data_matrix[i,429])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,429])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,429])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"钙")

###甘油三酯
grep("甘油三酯",colnames(HIV_raw_data_matrix))###97,408,435
# View(HIV_raw_data_matrix[,grep("甘油三酯",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,97])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,97])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,408])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,408])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,435])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,435])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"甘油三酯")

###高密度脂蛋白胆固醇
grep("高密度脂蛋白胆固醇",colnames(HIV_raw_data_matrix))###102,409,439
# View(HIV_raw_data_matrix[,grep("高密度脂蛋白胆固醇",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,102])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,102])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,102])
  }
  else if (!is.na(HIV_raw_data_matrix[i,409])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,409])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,409])
  }
  else if (!is.na(HIV_raw_data_matrix[i,439])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,439])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,439])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"高密度脂蛋白胆固醇")

### 高荧光强度淋巴细胞百分比
grep("高荧光强度淋巴细胞百分比",colnames(HIV_raw_data_matrix))###80
# View(HIV_raw_data_matrix[,grep("高荧光强度淋巴细胞百分比",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,80])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,80])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"高荧光强度淋巴细胞百分比")

### 胱抑素C
grep("胱抑素C",colnames(HIV_raw_data_matrix))###221
# View(HIV_raw_data_matrix[,grep("胱抑素C",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,221])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,221])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,221])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"胱抑素C")

###谷氨酰转肽酶
grep("谷氨酰转肽酶",colnames(HIV_raw_data_matrix))###112,333,422
# View(HIV_raw_data_matrix[,grep("谷氨酰转肽酶",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,112])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,112])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,112])
  }
  else if (!is.na(HIV_raw_data_matrix[i,333])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,333])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,333])
  }
  else if (!is.na(HIV_raw_data_matrix[i,422])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,422])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,422])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"谷氨酰转肽酶")

###红细胞
grep("红细胞",colnames(HIV_raw_data_matrix))###67,260,402
# View(HIV_raw_data_matrix[,grep("红细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,67])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,67])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,260])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,260])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,402])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,402])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"红细胞")

###红细胞体积分布宽度(CV)
grep("红细胞体积分布宽度",colnames(HIV_raw_data_matrix))###86,252
# View(HIV_raw_data_matrix[,grep("红细胞体积分布宽度",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,86])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,86])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,252])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,252])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"红细胞体积分布宽度(CV)")

###红细胞体积分布宽度(SD)
grep("红细胞体积分布宽度",colnames(HIV_raw_data_matrix))###19
# View(HIV_raw_data_matrix[,grep("红细胞体积分布宽度",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,19])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,19])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"红细胞体积分布宽度(SD)")

###红细胞压积
grep("红细胞压积",colnames(HIV_raw_data_matrix))###8,261,404
# View(HIV_raw_data_matrix[,grep("红细胞压积",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,8])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,8])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,261])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,261])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,404])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,404])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"红细胞压积")

###肌酐(酶法)
grep("肌酐",colnames(HIV_raw_data_matrix))###16,348,440
# View(HIV_raw_data_matrix[,grep("肌酐",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,16])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,16])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,16])
  }
  else if (!is.na(HIV_raw_data_matrix[i,348])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,348])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,348])
  }
  else if (!is.na(HIV_raw_data_matrix[i,440])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,440])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,440])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"肌酐(酶法)")

### 记忆CD4+T细胞%
grep("记忆CD4",colnames(HIV_raw_data_matrix))###100
# View(HIV_raw_data_matrix[,grep("记忆CD4",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,100])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,100])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,100])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"记忆CD4+T细胞%")

###记忆CD4+T细胞计数
grep("记忆CD4",colnames(HIV_raw_data_matrix))###103
# View(HIV_raw_data_matrix[,grep("记忆CD4",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,103])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,103])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,103])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"记忆CD4+T细胞计数")

###钾
grep("钾",colnames(HIV_raw_data_matrix))###85,351,426
# View(HIV_raw_data_matrix[,grep("钾",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,85])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,85])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,85])
  }
  else if (!is.na(HIV_raw_data_matrix[i,351])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,351])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,351])
  }
  else if (!is.na(HIV_raw_data_matrix[i,426])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,426])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,426])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"钾")

###碱性磷酸酶
grep("碱性磷酸酶",colnames(HIV_raw_data_matrix))###107,334,423
# View(HIV_raw_data_matrix[,grep("碱性磷酸酶",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,107])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,107])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,107])
  }
  else if (!is.na(HIV_raw_data_matrix[i,334])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,334])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,334])
  }
  else if (!is.na(HIV_raw_data_matrix[i,423])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,423])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,423])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"碱性磷酸酶")

###淋巴细胞
grep("淋巴细胞百分比",colnames(HIV_raw_data_matrix))###15,68
# View(HIV_raw_data_matrix[,grep("淋巴细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,15])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,15])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,68])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,68])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"淋巴细胞百分比")

###淋巴细胞绝对值
grep("淋巴细胞",colnames(HIV_raw_data_matrix))###26
# View(HIV_raw_data_matrix[,grep("淋巴细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,26])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,26])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,26])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"淋巴细胞绝对值")

###氯
grep("氯",colnames(HIV_raw_data_matrix))###75,353,428
# View(HIV_raw_data_matrix[,grep("氯",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,107])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,107])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,107])
  }
  else if (!is.na(HIV_raw_data_matrix[i,334])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,334])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,334])
  }
  else if (!is.na(HIV_raw_data_matrix[i,423])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,423])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,423])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"氯")

###钠
grep("钠",colnames(HIV_raw_data_matrix))###63,352,427
# View(HIV_raw_data_matrix[,grep("钠",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,63])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,63])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,63])
  }
  else if (!is.na(HIV_raw_data_matrix[i,352])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,352])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,352])
  }
  else if (!is.na(HIV_raw_data_matrix[i,427])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,427])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,427])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"钠")

###尿胆原
grep("尿胆原",colnames(HIV_raw_data_matrix))###71,327,414,454
# View(HIV_raw_data_matrix[,grep("尿胆原",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,71])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,71])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,71])
  }
  else if (!is.na(HIV_raw_data_matrix[i,327])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,327])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,327])
  }
  else if (!is.na(HIV_raw_data_matrix[i,414])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,414])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,414])
  }
  else if (!is.na(HIV_raw_data_matrix[i,454])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,454])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,454])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"尿胆原")

###尿素
grep("尿素",colnames(HIV_raw_data_matrix))###57,339,430
# View(HIV_raw_data_matrix[,grep("尿素",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,57])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,57])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,57])
  }
  else if (!is.na(HIV_raw_data_matrix[i,339])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,339])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,339])
  }
  else if (!is.na(HIV_raw_data_matrix[i,430])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,430])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,430])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"尿素")

###尿酸
grep("尿酸",colnames(HIV_raw_data_matrix))###89,355,432
# View(HIV_raw_data_matrix[,grep("尿酸",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,89])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,89])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,89])
  }
  else if (!is.na(HIV_raw_data_matrix[i,355])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,355])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,355])
  }
  else if (!is.na(HIV_raw_data_matrix[i,432])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,432])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,432])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"尿酸")

###平均红细胞体积
grep("平均红细胞体积",colnames(HIV_raw_data_matrix))###76,264
# View(HIV_raw_data_matrix[,grep("平均红细胞体积",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,76])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,76])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,76])
  }
  else if (!is.na(HIV_raw_data_matrix[i,264])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,264])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,76])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"平均红细胞体积")

###平均红细胞血红蛋白
grep("平均红细胞血红蛋白",colnames(HIV_raw_data_matrix))###61,257
# View(HIV_raw_data_matrix[,grep("平均红细胞血红蛋白",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,61])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,61])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,257])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,257])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"平均红细胞血红蛋白")

###平均红细胞血红蛋白浓度
grep("平均红细胞血红蛋白浓度",colnames(HIV_raw_data_matrix))###9,262
# View(HIV_raw_data_matrix[,grep("平均红细胞血红蛋白浓度",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,9])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,9])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,262])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,262])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"平均红细胞血红蛋白浓度")

###平均血小板体积
grep("平均血小板体积",colnames(HIV_raw_data_matrix))###64
# View(HIV_raw_data_matrix[,grep("平均血小板体积",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,64])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,64])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"平均血小板体积")

###葡萄糖
grep("葡萄糖",colnames(HIV_raw_data_matrix))###60,323,431
# View(HIV_raw_data_matrix[,grep("葡萄糖",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,60])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,60])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,323])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,323])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,431])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,431])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"葡萄糖")

###前白蛋白
grep("前白蛋白",colnames(HIV_raw_data_matrix))###23,344
# View(HIV_raw_data_matrix[,grep("前白蛋白",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,23])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,23])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,23])
  }
  else if (!is.na(HIV_raw_data_matrix[i,344])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,344])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,344])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"前白蛋白")

###乳酸脱氢酶
grep("乳酸脱氢酶",colnames(HIV_raw_data_matrix))###99,337,425
# View(HIV_raw_data_matrix[,grep("乳酸脱氢酶",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,99])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,99])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,99])
  }
  else if (!is.na(HIV_raw_data_matrix[i,337])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,337])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,337])
  }
  else if (!is.na(HIV_raw_data_matrix[i,425])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,425])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,425])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"乳酸脱氢酶")

###嗜碱性粒细胞百分比
grep("嗜碱性粒细胞",colnames(HIV_raw_data_matrix))###70
# View(HIV_raw_data_matrix[,grep("嗜碱性粒细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,70])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,70])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"嗜碱性粒细胞百分比")

###嗜碱性粒细胞绝对值
grep("嗜碱性粒细胞",colnames(HIV_raw_data_matrix))###20
# View(HIV_raw_data_matrix[,grep("嗜碱性粒细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,20])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,20])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"嗜碱性粒细胞绝对值")

###嗜酸性粒细胞百分比
grep("嗜酸性粒细胞",colnames(HIV_raw_data_matrix))###24
# View(HIV_raw_data_matrix[,grep("嗜酸性粒细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,24])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,24])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"嗜酸性粒细胞百分比")

###嗜酸性粒细胞绝对值
grep("嗜酸性粒细胞",colnames(HIV_raw_data_matrix))###79
# View(HIV_raw_data_matrix[,grep("嗜酸性粒细胞",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,79])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,79])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"嗜酸性粒细胞绝对值")

###酸碱度
grep("酸碱度",colnames(HIV_raw_data_matrix))###11,320,415
# View(HIV_raw_data_matrix[,grep("酸碱度",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,11])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,11])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,11])
  }
  else if (!is.na(HIV_raw_data_matrix[i,320])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,320])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,320])
  }
  else if (!is.na(HIV_raw_data_matrix[i,415])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,415])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,415])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"酸碱度")

###天门冬氨酸氨基转移酶
grep("天门冬氨酸氨基转移酶",colnames(HIV_raw_data_matrix))###93,335,424
# View(HIV_raw_data_matrix[,grep("天门冬氨酸氨基转移酶",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,93])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,93])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,93])
  }
  else if (!is.na(HIV_raw_data_matrix[i,335])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,335])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,335])
  }
  else if (!is.na(HIV_raw_data_matrix[i,424])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,424])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,424])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"天门冬氨酸氨基转移酶")

###无机磷
grep("无机磷",colnames(HIV_raw_data_matrix))###49,356,433
# View(HIV_raw_data_matrix[,grep("无机磷",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,49])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,49])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,49])
  }
  else if (!is.na(HIV_raw_data_matrix[i,356])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,356])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,356])
  }
  else if (!is.na(HIV_raw_data_matrix[i,433])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,433])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,433])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"无机磷")

###血红蛋白
grep("血红蛋白",colnames(HIV_raw_data_matrix))###9,259,405
# View(HIV_raw_data_matrix[,grep("血红蛋白",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,9])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,9])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,259])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,259])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,405])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,405])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"血红蛋白")

###血小板
grep("血小板",colnames(HIV_raw_data_matrix))###17,263,406
# View(HIV_raw_data_matrix[,grep("血小板",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,17])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,17])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,263])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,263])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,406])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,406])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"血小板")

###血小板体积分布宽度
grep("血小板体积分布宽度",colnames(HIV_raw_data_matrix))###55,130
# View(HIV_raw_data_matrix[,grep("血小板体积分布宽度",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,55])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,55])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(HIV_raw_data_matrix[i,130])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,130])
    temp_value <- convert_to_numeric(extracted)
    temp_number <- as.numeric(temp_value)
    if (temp_number<20){
      HIV_filtered_matr[i,(HIV_count+1)] <- temp_number
    }
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"血小板体积分布宽度")

###血小板压积
grep("血小板压积",colnames(HIV_raw_data_matrix))###87
# View(HIV_raw_data_matrix[,grep("血小板压积",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,87])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,87])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"血小板压积")

###直接胆红素
grep("直接胆红素",colnames(HIV_raw_data_matrix))###45,255,332,421
# View(HIV_raw_data_matrix[,grep("直接胆红素",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,45])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,45])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,45])
  }
  else if (!is.na(HIV_raw_data_matrix[i,255])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,255])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,255])
  }
  else if (!is.na(HIV_raw_data_matrix[i,332])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,332])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,332])
  }
  else if (!is.na(HIV_raw_data_matrix[i,421])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,421])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,421])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"直接胆红素")

###中性粒细胞百分比
grep("中性粒细胞百分比",colnames(HIV_raw_data_matrix))###39
# View(HIV_raw_data_matrix[,grep("中性粒细百分比",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,39])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,39])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"中性粒细胞百分比")

###中性粒细胞绝对值
grep("中性粒细胞绝对值",colnames(HIV_raw_data_matrix))###18
# View(HIV_raw_data_matrix[,grep("中性粒细胞绝对值",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,18])){
    extracted <- extract_numbers(HIV_raw_data_matrix[i,18])
    temp_value <- convert_to_numeric(extracted)
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(temp_value)
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"中性粒细胞绝对值")

###自然杀伤细胞CD16百分比
grep("自然杀伤细胞CD16",colnames(HIV_raw_data_matrix))###40
# View(HIV_raw_data_matrix[,grep("自然杀伤细胞CD16",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,40])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,40])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,40])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"自然杀伤细胞CD16")

###自然杀伤细胞CD16绝对值
grep("自然杀伤细胞CD16",colnames(HIV_raw_data_matrix))###59
# View(HIV_raw_data_matrix[,grep("自然杀伤细胞CD16",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,59])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,59])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,59])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"自然杀伤细胞CD16绝对值")

###总胆固醇
grep("总胆固醇",colnames(HIV_raw_data_matrix))###91,407,434
# View(HIV_raw_data_matrix[,grep("总胆固醇",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,91])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,91])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,91])
  }
  else if (!is.na(HIV_raw_data_matrix[i,407])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,407])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,407])
  }
  else if (!is.na(HIV_raw_data_matrix[i,434])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,434])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,434])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"总胆固醇")

###总胆红素
grep("总胆红素",colnames(HIV_raw_data_matrix))###65,126,254,331
# View(HIV_raw_data_matrix[,grep("总胆红素",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,65])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,65])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,65])
  }
  else if (!is.na(HIV_raw_data_matrix[i,126])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,126])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,126])
  }
  else if (!is.na(HIV_raw_data_matrix[i,254])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,254])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,254])
  }
  else if (!is.na(HIV_raw_data_matrix[i,331])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,331])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,331])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"总胆红素")

###总胆汁酸
grep("总胆汁酸",colnames(HIV_raw_data_matrix))###94,256,336
# View(HIV_raw_data_matrix[,grep("总胆汁酸",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,94])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,94])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,94])
  }
  else if (!is.na(HIV_raw_data_matrix[i,256])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,256])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,256])
  }
  else if (!is.na(HIV_raw_data_matrix[i,336])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,336])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,336])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"总胆汁酸")

###总蛋白
grep("总蛋白",colnames(HIV_raw_data_matrix))###14,341,438
# View(HIV_raw_data_matrix[,grep("总蛋白",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,14])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,14])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,14])
  }
  else if (!is.na(HIV_raw_data_matrix[i,341])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,341])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,341])
  }
  else if (!is.na(HIV_raw_data_matrix[i,438])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,438])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,438])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"总蛋白")

###总二氧化碳
grep("总二氧化碳",colnames(HIV_raw_data_matrix))###30
# View(HIV_raw_data_matrix[,grep("总二氧化碳",colnames(HIV_raw_data_matrix))])
for (i in 1:nrow(HIV_raw_data_matrix)){
  if (!is.na(HIV_raw_data_matrix[i,30])){
    # temp_value <- gsub("[^0-9.]", "", HIV_raw_data_matrix[i,30])
    HIV_filtered_matr[i,(HIV_count+1)] <- as.numeric(HIV_raw_data_matrix[i,30])
  }
}
HIV_count <- HIV_count+1
HIV_filtered_colnames <- c(HIV_filtered_colnames,"总二氧化碳")


HIV_filtered_matr <- HIV_filtered_matr[,1:HIV_count]
colnames(HIV_filtered_matr) <- HIV_filtered_colnames
rownames(HIV_filtered_matr) <- patient_ids

write.csv(HIV_filtered_matr,"xiehe_HIV_filtered_matr.csv",row.names = F,quote=T,fileEncoding = "GB18030")



HIV_features <- unique(HIV_filtered_colnames)

feature_count <- matrix(0,length(HIV_features),2)

for (i in 1:length(HIV_features)){
  temp_index <- which(colnames(HIV_filtered_matr)==HIV_features[i],T)
  feature_count[i,1] <- HIV_features[i]
  if (length(temp_index)>0){
    non_na_no <- length(which(is.na(HIV_filtered_matr[,temp_index])==F,T))
    feature_count[i,2] <- non_na_no
  }
  else{
    feature_count[i,2] <- 0
    message(feature_count[i,1])
  }
}

sorted_feature_count <- feature_count[order(as.numeric(feature_count[,2]),decreasing = TRUE),]

all_items <- sorted_feature_count[,1]
all_count4 <- matrix(0,length(all_items),2)
for (i in 1:length(all_items)){
  temp_items <- all_items[1:i]
  temp_all_data <- HIV_filtered_matr[,which(colnames(HIV_filtered_matr)%in%temp_items,T)]
  temp_na_index <- which(is.na(temp_all_data)==T,T)
  if (length(temp_na_index)>0){
    temp_all_na_index <- unique(temp_na_index[,1])
    temp_all_valid_num <- nrow(temp_all_data)-length(temp_all_na_index)
  }
  else{
    temp_all_valid_num <- nrow(HIV_filtered_matr)
  }
  all_count4[i,1] <- all_items[i]
  all_count4[i,2] <- temp_all_valid_num
}
colnames(all_count4) <- c("变量","截至目前可用数据量")

plot(1:nrow(all_count4), as.numeric(all_count4[,2]), type = "l", col = "red", lwd = 2)
write.csv(all_count4,"all_count4_xiehe_HIV_260113.csv",row.names = F,quote=T,fileEncoding = "GB18030")

to_check_matr <- matrix("",max(dim(male_train)[2],dim(HIV_filtered_matr)[2]),6)

to_check_matr[1:dim(male_train)[2],1] <- colnames(male_train)
to_check_matr[1:dim(all_count4)[1],2] <- all_count4[,1]
to_check_matr[1:dim(all_count4)[1],3] <- all_count4[,2]

colnames(to_check_matr) <- c("原来的特征","HIV特征","HIV有效数据量","是否为原来列表中的特征","是否为新出现的特征","名称不一致需修改HIV特征名")
write.csv(to_check_matr,"features_to_map_20260113.csv",row.names = F,quote=T,fileEncoding = "GB18030")


##### the following 4 lines are from file data_processing_of_xiehe_physical_test-2511.R, with line 5292-5297.
# all_train <- all_data[all_train_index,]##1590   90
# all_test <- all_data[all_test_index,]##400  90
# 
# 
# write.csv(all_train,"xiehe_all_train_data_1124_case2.csv",row.names = F,quote=T,fileEncoding = "GB18030")
# write.csv(all_test,"xiehe_all_test_data_1124_case2.csv",row.names = F,quote=T,fileEncoding = "GB18030")
all_train_from_1124_case2 <- all_train








# previous_features <- c("就诊id","出生日期","实际到检时间",colnames(all_train_from_1124_case2))
# 
# healthy_features_to_remove <- c("α1球蛋白","α2球蛋白","β1球蛋白","β2球蛋白","γ球蛋白","癌胚抗原","血清蛋白电泳白蛋白","电泳白球比","血生化白球比",
#                                 "促甲状腺激素","甲胎蛋白","甲状腺过氧化物酶抗体","甲状腺球蛋白抗体","甲状腺素","尿白蛋白肌酐比值","尿肌酐",
#                                 "尿微量白蛋白","三碘甲状腺原氨酸","收缩压","舒张压","糖化血红蛋白","体质指数","同型半胱氨酸","心率","腰围",
#                                 "游离甲状腺素","游离三碘甲状腺原氨酸","游离脂肪酸","载脂蛋白A1","载脂蛋白B","脂蛋白(a)","平均血小板体积","血小板体积分布宽度")
# healthy_features_left <- setdiff(previous_features,healthy_features_to_remove)
# 
# valid_features <- healthy_features_left
# valid_data <- filtered_matr[,which(colnames(filtered_matr)%in%valid_features,T)]
# na_index <- unique(which(is.na(valid_data)==T,T)[,1])
# new_valid_data0 <- valid_data[-na_index,]
# valid_ids <- which(new_valid_data0[,1]%in%data21[,2],T)
# new_valid_data <- new_valid_data0[valid_ids,]
# 
# new_valid_info <- matrix("",nrow(new_valid_data),2)
# 
# for (i in 1:nrow(new_valid_data)){
#   temp_id <- new_valid_data[i,1]
#   temp_index <- which(data21[,2]==temp_id,T)
#   new_valid_info[i,1] <- temp_id
#   new_valid_info[i,2] <- data21[temp_index,8]
# }
# colnames(new_valid_info) <- c("就诊id","报告单")
# 
# ##### BMI
# grep("体质指数",colnames(new_valid_data))#77
# # fat_index <- which(data_left[,104]>=30,T)
# male_index <- which(new_valid_data[,4]==1,T)
# female_index <- which(new_valid_data[,4]==0,T)
# male_thin_index <- male_index[which(new_valid_data[male_index,77]<22,T)]
# female_thin_index <- female_index[which(new_valid_data[female_index,77]<20,T)]
# 
# # BMI_abnormal_index <- c(fat_index,male_thin_index,female_thin_index)
# BMI_abnormal_index <- c(male_thin_index,female_thin_index)
# length(BMI_abnormal_index) ###1330
# 
# 
# #### diabetes
# grep("糖化血红蛋白",colnames(new_valid_data))#76
# grep("葡萄糖",colnames(new_valid_data))#23
# count0 <- 0
# diabetes_history_index <- c()#糖尿病
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("糖尿病病史",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     message(i)
#     count0 <- count0+1
#     diabetes_history_index <- c(diabetes_history_index,i)
#   }
# }
# dhi <- unique(c(diabetes_history_index,which(new_valid_data[,76]>=6.5,T),which(new_valid_data[,23]>7.1,T)))
# length(dhi)###864
# 
# ###cancer&immune
# cancer_immune_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("癌",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cancer_immune_index <- c(cancer_immune_index,i)
#   }
#   temp_index <- grep("免疫病",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cancer_immune_index <- c(cancer_immune_index,i)
#   }
#   temp_index <- grep("干燥综合征",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cancer_immune_index <- c(cancer_immune_index,i)
#   }
#   temp_index <- grep("系统性红斑狼疮",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cancer_immune_index <- c(cancer_immune_index,i)
#   }
#   temp_index <- grep("类风湿性关节炎",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cancer_immune_index <- c(cancer_immune_index,i)
#   }
#   temp_index <- grep("硬皮病",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cancer_immune_index <- c(cancer_immune_index,i)
#   }
#   temp_index <- grep("皮肌炎",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cancer_immune_index <- c(cancer_immune_index,i)
#   }
# }
# cancer_immune_index <- unique(cancer_immune_index)
# length(cancer_immune_index)###1350
# 
# ### pregnant
# pregnant_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("哺乳期",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     pregnant_index <- c(pregnant_index,i)
#   }
# }
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("怀孕",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     pregnant_index <- c(pregnant_index,i)
#   }
# }
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("产后6个月",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     pregnant_index <- c(pregnant_index,i)
#   }
# }
# length(pregnant_index)###7
# 
# 
# ## ultrasonic
# ultrasonic_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("BI-RADS 4b",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     ultrasonic_index <- c(ultrasonic_index,i)
#   }
# }
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("建议乳腺钼靶检查",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     ultrasonic_index <- c(ultrasonic_index,i)
#   }
# }
# length(ultrasonic_index)###75
# 
# 
# ### cardiac infarction
# cardiac_infarction_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("冠心病支架术后",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cardiac_infarction_index <- c(cardiac_infarction_index,i)
#   }
# }
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("心梗",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     cardiac_infarction_index <- c(cardiac_infarction_index,i)
#   }
# }
# length(cardiac_infarction_index)###142
# 
# 
# ### CBC
# CBC_index <- c()
# grep("血红蛋白",colnames(new_valid_data))#27
# # grep("中性粒细胞绝对值",colnames(new_valid_data))#28
# grep("血小板",colnames(new_valid_data))#28
# grep("白细胞",colnames(new_valid_data))#6
# yt65_index <- which(new_valid_data[,5]<=65,T)
# ot65_index <- which(new_valid_data[,5]>65,T)
# CBC_index <- c(CBC_index,yt65_index[which(new_valid_data[yt65_index,27]<110,T)])
# ot65_male_index <- ot65_index[which(new_valid_data[ot65_index,4]==1,T)]
# ot65_female_index <- ot65_index[which(new_valid_data[ot65_index,4]==0,T)]
# CBC_index <- c(CBC_index,ot65_index[which(new_valid_data[ot65_male_index,27]<81,T)])
# CBC_index <- c(CBC_index,ot65_index[which(new_valid_data[ot65_female_index,27]<74,T)])
# 
# # CBC_index <- c(CBC_index,which(new_valid_data[,28]<1.5,T))
# CBC_index <- c(CBC_index,which(new_valid_data[,28]<100,T))
# CBC_index <- c(CBC_index,which(new_valid_data[,6]>10,T))
# CBC_index <- unique(CBC_index)
# length(CBC_index)###256
# 
# 
# ### URT
# URT_index <- c()
# 
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("尿糖阳性",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     URT_index <- c(URT_index,i)
#   }
# }
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("蛋白\\(白蛋白\\)",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     temp_index1 <- grep("蛋白\\(白蛋白\\)：TRACE",new_valid_info[i,2])
#     if (length(temp_index1)==0){
#       URT_index <- c(URT_index,i)
#     }
#   }
# }
# 
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("红细胞\\(潜血\\)",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     temp_index1 <- grep("红细胞\\(潜血\\)：TRACE",new_valid_info[i,2])
#     if (length(temp_index1)==0){
#       URT_index <- c(URT_index,i)
#     }
#   }
# }
# URT_index <- unique(URT_index)
# length(URT_index)####716
# 
# ### kidney
# kidney_index <- c()
# grep("肌酐",colnames(new_valid_data))#13
# grep("尿素",colnames(new_valid_data))#18
# male_index <- which(new_valid_data[,4]==1,T)
# female_index <- which(new_valid_data[,4]==0,T)
# kidney_index <- c(kidney_index,male_index[which(new_valid_data[male_index,13]>104,T)])
# kidney_index <- c(kidney_index,female_index[which(new_valid_data[female_index,13]>84,T)])
# kidney_index <- c(kidney_index,male_index[which(new_valid_data[male_index,18]>9.7,T)])
# kidney_index <- c(kidney_index,female_index[which(new_valid_data[female_index,18]>9.2,T)])
# kidney_index <- unique(kidney_index)
# length(kidney_index)####236
# 
# ### liver
# liver_index <- c()
# grep("ALT",colnames(new_valid_data))#7
# grep("天门冬氨酸氨基转移酶",colnames(new_valid_data))#25
# grep("碱性磷酸酶",colnames(new_valid_data))#15
# # male_index <- which(data_left[,5]==1,T)
# # female_index <- which(data_left[,5]==0,T)
# liver_index <- c(liver_index,which(new_valid_data[,7]>50,T))
# liver_index <- c(liver_index,which(new_valid_data[,25]>40,T))
# liver_index <- c(liver_index,which(new_valid_data[,15]>125,T))
# 
# yt65_index <- which(new_valid_data[,5]<=65,T)
# ot65_index <- which(new_valid_data[,5]>65,T)
# liver_index <- c(liver_index,yt65_index[which(new_valid_data[yt65_index,15]>125,T)])
# ot65_male_index <- ot65_index[which(new_valid_data[ot65_index,4]==1,T)]
# ot65_female_index <- ot65_index[which(new_valid_data[ot65_index,4]==0,T)]
# liver_index <- c(liver_index,ot65_index[which(new_valid_data[ot65_male_index,15]>270,T)])
# liver_index <- c(liver_index,ot65_index[which(new_valid_data[ot65_female_index,15]>282,T)])
# 
# liver_index <- unique(liver_index)
# length(liver_index)####1018
# 
# ### MPP
# MPP_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("M蛋白",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     MPP_index <- c(MPP_index,i)
#   }
# }
# length(MPP_index)####77
# 
# 
# ### tumor
# library(stringr)
# tumor_index <- c()
# 
# pattern <- "癌胚抗原：\\s*([0-9]+\\.?[0-9]*)\\s*ng/m"
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("癌胚抗原",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     match_result <- str_match(new_valid_info[i,2], pattern)
#     if (!is.na(match_result[1,2])){
#       value <- as.numeric(match_result[1,2])
#       if (value>5){
#         tumor_index <- c(tumor_index,i)
#       }
#     }
#     
#   }
# }
# 
# pattern <- "糖链抗原72-4：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("糖链抗原72-4",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     match_result <- str_match(new_valid_info[i,2], pattern)
#     if (!is.na(match_result[1,2])){
#       value <- as.numeric(match_result[1,2])
#       if (value>9.8){
#         tumor_index <- c(tumor_index,i)
#       }
#     }
#     
#   }
# }
# 
# pattern <- "糖链抗原15-3：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("糖链抗原15-3",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     match_result <- str_match(new_valid_info[i,2], pattern)
#     if (!is.na(match_result[1,2])){
#       value <- as.numeric(match_result[1,2])
#       if (value>25){
#         tumor_index <- c(tumor_index,i)
#       }
#     }
#     
#   }
# }
# 
# pattern <- "糖链抗原19-9：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("糖链抗原19-9",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     match_result <- str_match(new_valid_info[i,2], pattern)
#     if (!is.na(match_result[1,2])){
#       value <- as.numeric(match_result[1,2])
#       if (value>37){
#         tumor_index <- c(tumor_index,i)
#       }
#     }
#     
#   }
# }
# 
# pattern <- "糖链抗原125：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("糖链抗原125",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     match_result <- str_match(new_valid_info[i,2], pattern)
#     if (!is.na(match_result[1,2])){
#       value <- as.numeric(match_result[1,2])
#       if (value>35){
#         tumor_index <- c(tumor_index,i)
#       }
#     }
#     
#   }
# }
# 
# # grep("糖链抗原",colnames(data_left))#88
# # grep("癌胚抗原",colnames(data_left))#47
# # tumor_index <- c(tumor_index,which(data_left[,88]>37,T))
# # tumor_index <- c(tumor_index,which(data_left[,47]>5,T))
# length(tumor_index)####632
# 
# ### antibody
# antibody_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("自身抗体谱异常",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     match_result <- str_match(new_valid_info[i,2], "抗核抗体\\(IgG型\\)([\\s\\S]*?)\\(<1:40 \\)\n")[1,2]
#     temp_index1 <- grep("1:80",match_result)
#     if (length(temp_index1)==0){
#       antibody_index <- c(antibody_index,i)
#     }
#   }
# }
# length(antibody_index)####767
# 
# 
# ### CCD
# CCD_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("心脏彩超",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     echo_pos <- str_locate(new_valid_info[i,2], "心脏彩超")[1, "start"]
#     next_20_chars <- str_sub(new_valid_info[i,2], echo_pos, echo_pos + 19)
#     has_severe <- str_detect(next_20_chars, "重度")
#     if (has_severe){
#       CCD_index <- c(CCD_index,i)
#     }
#   }
# }
# length(CCD_index)####1
# 
# ### Blood lipids
# grep("总胆固醇",colnames(new_valid_data))##29
# grep("低密度脂蛋白胆固醇",colnames(new_valid_data))##55
# grep("甘油三酯",colnames(new_valid_data))##9
# blood_lipids_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("血脂异常",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     if (new_valid_data[i,29]>5.7){
#       blood_lipids_index <- c(blood_lipids_index,i)
#     }
#     if (new_valid_data[i,55]>3.37){
#       blood_lipids_index <- c(blood_lipids_index,i)
#     }
#     if (new_valid_data[i,9]>1.7){
#       blood_lipids_index <- c(blood_lipids_index,i)
#     }
#   }
#   temp_index1 <- grep("脂肪肝",new_valid_info[i,2])
#   if (length(temp_index1)>0){
#     blood_lipids_index <- c(blood_lipids_index,i)
#   }
# }
# blood_lipids_index <- unique(blood_lipids_index)
# length(blood_lipids_index)####7019
# 
# # ### LCT
# # LCT_index <- c()
# # for (i in 1:nrow(new_valid_info)){
# #   temp_index <- grep("LCT异常",new_valid_info[i,7])
# #   if (length(temp_index)>0){
# #     LCT_index <- c(LCT_index,i)
# #   }
# # }
# # length(LCT_index)####122
# 
# # ### lung
# # lung_index <- c()
# # for (i in 1:nrow(new_valid_info)){
# #   temp_index <- grep("磨玻璃结节",new_valid_info[i,7])
# #   if (length(temp_index)>0){
# #     lung_index <- c(lung_index,i)
# #   }
# # }
# # length(lung_index)####142
# 
# ###inflammation
# grep("超敏C反应蛋白",colnames(new_valid_data))##51
# inflammation_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("血沉",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     temp_value <- sub(".*?血沉：(.*?) mm/h.*", "\\1", new_valid_info[i,2])
#     if (length(temp_value)>0){
#       temp_num <- as.numeric(temp_value)
#       if (temp_num>20){
#         inflammation_index <- c(inflammation_index,i)
#       }
#     }
#   }
#   if (new_valid_data[i,51]>10){
#     inflammation_index <- c(inflammation_index,i)
#   }
#   
# }
# inflammation_index <- unique(inflammation_index)
# length(inflammation_index)####369
# 
# 
# 
# ###ARF
# ARF_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("类风湿因子异常",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     temp_value <- sub(".*?类风湿因子：(.*?) IU/ml.*", "\\1", new_valid_info[i,2])
#     if (length(temp_value)>0){
#       temp_num <- as.numeric(temp_value)
#       if (temp_num>20){
#         ARF_index <- c(ARF_index,i)
#       }
#     }
#   }
#   
# }
# 
# length(ARF_index)####349
# 
# ### PET
# PET_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index <- grep("代谢异常增高",new_valid_info[i,2])
#   if (length(temp_index)>0){
#     PET_index <- c(PET_index,i)
#   }
# }
# length(PET_index)####3
# 
# 
# new_blood_lipids_index <- c()
# for (i in 1:nrow(new_valid_info)){
#   temp_index1 <- grep("脂肪肝",new_valid_info[i,2])
#   if (length(temp_index1)>0){
#     new_blood_lipids_index <- c(new_blood_lipids_index,i)
#   }
# }
# new_blood_lipids_index <- unique(new_blood_lipids_index)
# length(new_blood_lipids_index)####5005
# 
# abnormal_index <- c(BMI_abnormal_index,hyh_index,dhi,cancer_immune_index,pregnant_index,ultrasonic_index,cardiac_infarction_index,CBC_index,URT_index,kidney_index,liver_index,MPP_index,tumor_index,antibody_index,CCD_index,new_blood_lipids_index,inflammation_index,ARF_index,PET_index)
# abnormal_index <- unique(abnormal_index)
# length(abnormal_index)###7745，剩下2506
# 
# data_left1 <- new_valid_data[-abnormal_index,]
previous_data_left1 <- data_left1

all_data <- previous_data_left1[,-c(1:3)]
all_data[,c(1,2)] <- all_data[,c(2,1)]
colnames(all_data)[c(1,2)] <- colnames(all_data)[c(2,1)]
healthy_features_to_remove <- c("α1球蛋白","α2球蛋白","β1球蛋白","β2球蛋白","γ球蛋白","癌胚抗原","血清蛋白电泳白蛋白","电泳白球比",
                                "促甲状腺激素","甲胎蛋白","甲状腺过氧化物酶抗体","甲状腺球蛋白抗体","甲状腺素","尿白蛋白肌酐比值","尿肌酐",
                                "尿微量白蛋白","三碘甲状腺原氨酸","收缩压","舒张压","糖化血红蛋白","体质指数","同型半胱氨酸","心率","腰围",
                                "游离甲状腺素","游离三碘甲状腺原氨酸","游离脂肪酸","载脂蛋白A1","载脂蛋白B","脂蛋白(a)","平均血小板体积",
                                "血小板体积分布宽度","尿比重","尿胆原","尿酸碱度","血钙","总二氧化碳","尿酸","无机磷","碱性磷酸酶","氯",
                                "谷氨酰转肽酶","总胆汁酸","胆碱酯酶","超敏C反应蛋白")

to_remove_index <- which(colnames(all_data)%in%healthy_features_to_remove,T)
all_data <- all_data[,-to_remove_index]
all_data[,1] <- all_data[,1]/100


set.seed(123456)

all_train_index <- c()
all_test_index <- c()
all_temp_index1 <- which(all_data[,1]<=0.3,T)
all_temp_num1 <- ceiling(length(all_temp_index1)*0.2)
all_temp_index12 <- sample(1:length(all_temp_index1),all_temp_num1)
all_temp_test_index1 <- all_temp_index1[all_temp_index12]
all_temp_train_index1 <- all_temp_index1[-all_temp_index12]
all_train_index <- c(all_train_index,all_temp_train_index1)
all_test_index <- c(all_test_index,all_temp_test_index1)


all_temp_index2 <- which(all_data[,1]>0.3 & all_data[,1]<=0.6,T)
all_temp_num2 <- ceiling(length(all_temp_index2)*0.2)
all_temp_index22 <- sample(1:length(all_temp_index2),all_temp_num2)
all_temp_test_index2 <- all_temp_index2[all_temp_index22]
all_temp_train_index2 <- all_temp_index2[-all_temp_index22]
all_train_index <- c(all_train_index,all_temp_train_index2)
all_test_index <- c(all_test_index,all_temp_test_index2)

all_temp_index3 <- which(all_data[,1]>0.6,T)
all_temp_num3 <- ceiling(length(all_temp_index3)*0.2)
all_temp_index32 <- sample(1:length(all_temp_index3),all_temp_num3)
all_temp_test_index3 <- all_temp_index3[all_temp_index32]
all_temp_train_index3 <- all_temp_index3[-all_temp_index32]
all_train_index <- c(all_train_index,all_temp_train_index3)
all_test_index <- c(all_test_index,all_temp_test_index3)


all_train <- all_data[all_train_index,]
all_test <- all_data[all_test_index,]


write.csv(all_train,"xiehe_all_train_data_260114_case2.csv",row.names = F,quote=T,fileEncoding = "GB18030")
write.csv(all_test,"xiehe_all_test_data_260114_case2.csv",row.names = F,quote=T,fileEncoding = "GB18030")



####################matching HIV data to previous data############

###delete feature names not included in the model
features_to_del <- c("出生日期","实际到检时间","B淋巴细胞CD19%","CD4+T细胞%",
                     "CD8+T细胞%","T淋巴细胞CD3%","自然杀伤细胞CD16",
                     "CD8+DR+计数","CD4+CD28+计数","CD8+CD28+计数","CD8+CD38+计数",
                     "淋巴细胞绝对值","45RA+CD4+T细胞%","纯真CD4+T细胞计数",
                     "记忆CD4+T细胞计数","45RA+CD4+T细胞计数","单核细胞计数",
                     "嗜酸性粒细胞绝对值","中性粒细胞绝对值","嗜碱性粒细胞绝对值",
                     "平均血小板体积","血小板体积分布宽度","红细胞体积分布宽度(SD)",
                     "大血小板比率","高荧光强度淋巴细胞百分比","HIV-1病毒载量",
                     "胱抑素C","丙型肝炎抗体","D-二聚体","白细胞介素6",
                     "白细胞(中性粒细胞酯酶)","蛋白(白蛋白)","胆红素","尿比重",
                     "尿胆原","酸碱度","钙","总二氧化碳","尿酸","无机磷",
                     "碱性磷酸酶","氯","谷氨酰转肽酶","总胆汁酸","胆碱酯酶",
                     "C反应蛋白(超敏)")

HIV_colnames_left <- setdiff(HIV_filtered_colnames,features_to_del)
HIV_filtered_matr_left <- HIV_filtered_matr[,which(HIV_filtered_colnames%in%HIV_colnames_left,T)]
HIV_features <- unique(HIV_colnames_left)

feature_count <- matrix(0,length(HIV_features),2)

for (i in 1:length(HIV_features)){
  temp_index <- which(colnames(HIV_filtered_matr_left)==HIV_features[i],T)
  feature_count[i,1] <- HIV_features[i]
  if (length(temp_index)>0){
    non_na_no <- length(which(is.na(HIV_filtered_matr_left[,temp_index])==F,T))
    feature_count[i,2] <- non_na_no
  }
  else{
    feature_count[i,2] <- 0
    message(feature_count[i,1])
  }
}

sorted_feature_count <- feature_count[order(as.numeric(feature_count[,2]),decreasing = TRUE),]

all_items <- sorted_feature_count[,1]
all_count4 <- matrix(0,length(all_items),2)
for (i in 1:length(all_items)){
  temp_items <- all_items[1:i]
  temp_all_data <- HIV_filtered_matr_left[,which(colnames(HIV_filtered_matr_left)%in%temp_items,T)]
  temp_na_index <- which(is.na(temp_all_data)==T,T)
  if (length(temp_na_index)>0){
    temp_all_na_index <- unique(temp_na_index[,1])
    temp_all_valid_num <- nrow(temp_all_data)-length(temp_all_na_index)
  }
  else{
    temp_all_valid_num <- nrow(HIV_filtered_matr_left)
  }
  all_count4[i,1] <- all_items[i]
  all_count4[i,2] <- temp_all_valid_num
}
colnames(all_count4) <- c("变量","截至目前可用数据量")

plot(1:nrow(all_count4), as.numeric(all_count4[,2]), type = "l", col = "red", lwd = 2)

###revise feature names as previous data
HIV_colnames_revised <- colnames(HIV_filtered_matr_left)

temp_index <- which(HIV_colnames_revised=="CD4/CD8",T)
HIV_colnames_revised[temp_index] <- "CD4/CD8%"

temp_index <- which(HIV_colnames_revised=="B淋巴细胞CD19计数",T)
HIV_colnames_revised[temp_index] <- "B淋巴细胞CD19"

temp_index <- which(HIV_colnames_revised=="CD4+T细胞计数",T)
HIV_colnames_revised[temp_index] <- "CD4+T细胞绝对值"

temp_index <- which(HIV_colnames_revised=="CD8+T细胞计数",T)
HIV_colnames_revised[temp_index] <- "CD8+T细胞绝对值"

temp_index <- which(HIV_colnames_revised=="T淋巴细胞CD3计数",T)
HIV_colnames_revised[temp_index] <- "CD3绝对值"

temp_index <- which(HIV_colnames_revised=="自然杀伤细胞CD16绝对值",T)
HIV_colnames_revised[temp_index] <- "NK细胞绝对值"

temp_index <- which(HIV_colnames_revised=="丙氨酸氨基转移酶",T)
HIV_colnames_revised[temp_index] <- "ALT"

temp_index <- which(HIV_colnames_revised=="白蛋白",T)
HIV_colnames_revised[temp_index] <- "血白蛋白"

temp_index <- which(HIV_colnames_revised=="肌酐(酶法)",T)
HIV_colnames_revised[temp_index] <- "肌酐"

temp_index <- which(HIV_colnames_revised=="白蛋白球蛋白比",T)
HIV_colnames_revised[temp_index] <- "血生化白球比"

temp_index <- which(HIV_colnames_revised=="淋巴细胞",T)
HIV_colnames_revised[temp_index] <- "淋巴细胞百分比"

temp_index <- which(HIV_colnames_revised=="红细胞体积分布宽度(CV)",T)
HIV_colnames_revised[temp_index] <- "红细胞体积分布宽度"

colnames(HIV_filtered_matr_left) <- HIV_colnames_revised

length(which(colnames(all_train)%in%colnames(HIV_filtered_matr_left),T))
length(which(colnames(HIV_filtered_matr_left)%in%colnames(all_train),T))

HIV_test_data <- HIV_filtered_matr_left
for (i in 1:ncol(all_train)){
  temp_index <- which(HIV_colnames_revised==colnames(all_train)[i],T)
  HIV_test_data[,i] <- HIV_filtered_matr_left[,temp_index]
}
colnames(HIV_test_data) <- colnames(all_train)

na_index1 <- unique(which(is.na(HIV_test_data)==T,T)[,1])
HIV_test_data1 <- HIV_test_data[-na_index1,]
dim(HIV_test_data1)

valid_index <- which(bbb%in%HIV_ids,T)
valid_ids <- bbb[valid_index]
valid_ids <- valid_ids[-na_index1]
length(valid_ids)
valid_time <- raw_data_matrix[valid_index,4]
valid_time <- valid_time[-na_index1]
length(valid_time)
valid_info <- cbind(valid_ids,valid_time)
colnames(valid_info) <- c("患者ID","到检时间")

write.csv(HIV_test_data1,"xiehe_HIV_test_data_260114.csv",row.names = F,quote=T,fileEncoding = "GB18030")

write.csv(valid_info,"xiehe_HIV_valid_info_260114.csv",row.names = F,quote=T,fileEncoding = "GB18030")



HIV_res <- read_excel("E:/backup/data/xiehe-lung/physical_test20251017/HIV_results_20260114/xiehe_HIV_prediction_results.xlsx")

HIV_res <- as.matrix(HIV_res)

uni_HIV_ids <- unique(HIV_res[,5])
first_time_gap <- c()
first_time_age <- c()
old_young <- c()
for (i in 1:length(uni_HIV_ids)){
  temp_index <- which(HIV_res[,5]==uni_HIV_ids[i],T)
  if (length(temp_index)==1){
    first_time_gap <- c(first_time_gap,HIV_res[temp_index,4])
    first_time_age <- c(first_time_age,HIV_res[temp_index,1])
    old_young <- c(old_young,HIV_res[temp_index,9])
  }
  else{
    temp_time <- HIV_res[temp_index,6]
    temp_time_list <- as.numeric(as.Date(temp_time))
    temp_min_index <- which.min(temp_time_list)
    first_time_gap <- c(first_time_gap,HIV_res[temp_index[temp_min_index],4])
    first_time_age <- c(first_time_age,HIV_res[temp_index[temp_min_index],1])
    old_young <- c(old_young,HIV_res[temp_index[temp_min_index],9])
  }
}

first_time_gap <- as.numeric(first_time_gap)
first_time_age <- as.numeric(first_time_age)
old_young <- as.numeric(old_young)
hist(round(first_time_gap),breaks=50)
hist(round(first_time_age),breaks=50)

plot(round(as.numeric(HIV_res[,1])),cumsum(as.numeric(HIV_res[,9])))
length(HIV_res[,9])
length(uni_HIV_ids)

plot(as.numeric(first_time_age),as.numeric(first_time_gap))
plot(first_time_age,cumsum(old_young),xlab = "首次到检年龄", ylab = "AgeGap大于0数量累积")




# plot(first_time_age,cumsum(old_young_reversed))

# 数据准备
# 假设first_time_age和old_young是两个向量
# 将old_young倒序后计算累积和
old_young_reversed <- rev(old_young)
cumulative_sum <- cumsum(old_young_reversed)

# 设置图形参数
par(
  family = "serif",          # 使用serif字体，符合科研论文标准
  cex.axis = 1.1,            # 坐标轴标签大小
  cex.lab = 1.3,             # 坐标轴标题大小
  font.lab = 2,              # 坐标轴标题加粗
  las = 1,                   # 坐标轴标签水平显示
  mar = c(5, 5, 4, 2) + 0.1, # 图形边距
  mgp = c(3, 1, 0)           # 坐标轴标签位置
)

# 创建图形文件
png(
  filename = "cumulative_age_gap.png",  # 输出文件名
  width = 4,                            # 宽度4英寸
  height = 3,                           # 高度3英寸
  units = "in",                         # 单位为英寸
  res = 600,                            # 分辨率600dpi
  bg = "white"                          # 背景白色
)

# 绘制散点图
plot(
  x = first_time_age,
  y = cumulative_sum,
  type = "b",                  # "b"表示点和线都绘制
  pch = 19,                    # 实心圆点
  col = "#2E8B57",             # 科研绿 (Seagreen)
  lwd = 2,                     # 线宽
  cex = 1.2,                   # 点的大小
  xlab = "首次到检年龄",       # 横轴标签
  ylab = "AgeGap大于0数量累积", # 纵轴标签
  cex.main = 1.5,              # 主标题大小
  font.main = 2,               # 主标题加粗
  panel.first = grid(          # 添加网格线
    col = "gray90",
    lty = "dotted",
    lwd = 0.5
  ),
  bty = "l"                    # 图形边框只显示左边和下边
)

# 添加图例（可选）
legend(
  "topright",                  # 图例位置
  legend = c("累积数量"),      # 图例文本
  col = c("#2E8B57"),          # 颜色
  pch = 19,                    # 点形状
  lty = 1,                     # 线型
  lwd = 2,                     # 线宽
  bg = "white",                # 背景色
  box.col = "gray50",          # 边框颜色
  cex = 0.9                    # 图例大小
)

# 关闭图形设备，保存图片
dev.off()
