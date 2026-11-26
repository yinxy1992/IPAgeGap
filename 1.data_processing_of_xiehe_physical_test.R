
library(dplyr)
data1 <- read.csv("./体检明细.csv")
data2 <- as.matrix(data1)
rm(data1)

data11 <- read.csv("./体检首页+基本信息.csv")
data21 <- as.matrix(data11)
na_index <- unique(c(which(data21[,5]=="",T),c(which(data21[,9]=="",T))))
data21 <- data21[-na_index,]

uni_id <- unique(data2[,2])
length(uni_id)

inter_id <- unique(data21[which(data21[,2]%in%uni_id,T),2])
length(inter_id)

data3 <- data2[which(data2[,2]%in%uni_id,T),]
all_tests <- unique(data3[,c(4,5,9)])

raw_data_matrix <- matrix(NA,length(inter_id),(nrow(all_tests)+6))
test_name <- c()
for (i in 1:nrow(all_tests)){
  temp_name <- paste(all_tests[i,1],all_tests[i,2],all_tests[i,3],sep="---")
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
    temp_test <- paste(temp_data[j,4],temp_data[j,5],temp_data[j,9],sep = "---")
    raw_data_matrix[i,temp_test] <- temp_data[j,8]
  }
  temp_index1 <- which(data21[,2]==temp_id,T)
  if (length(temp_index1)==1){
    raw_data_matrix[i,3] <- data21[temp_index1,"出生日期"]
    raw_data_matrix[i,4] <- data21[temp_index1,"实际到检时间"]
    raw_data_matrix[i,5] <- data21[temp_index1,"性别"]
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

raw_data_matrix[which(raw_data_matrix=="[NULL]",T)] <- NA
write.csv(raw_data_matrix,"raw_data_xiehe_physical_test1106.csv",row.names = F,quote = T,fileEncoding = "GB18030")

########################################################################
##############those features include in the model#######################
########################################################################
filtered_matr <- matrix(NA,nrow(raw_data_matrix),ncol(raw_data_matrix))
filtered_colnames <- c()
count <- 0

### 就诊id
filtered_matr[,(count+1)] <- as.numeric(raw_data_matrix[,2])
count <- count+1
filtered_colnames <- c(filtered_colnames,"就诊id")

### 出生日期
for (i in 1:nrow(raw_data_matrix)){
  temp_date <- raw_data_matrix[i,"出生日期"]
  birthday <- as.numeric(as.Date(temp_date))
  filtered_matr[i,(count+1)] <- birthday
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"出生日期")

### 实际到检时间
for (i in 1:nrow(raw_data_matrix)){
  temp_date <- raw_data_matrix[i,"实际到检时间"]
  test_day <- as.numeric(as.Date(temp_date))
  filtered_matr[i,(count+1)] <- test_day
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"实际到检时间")

### 性别
for (i in 1:nrow(raw_data_matrix)){
  temp_sex <- raw_data_matrix[i,"性别"]
  if (temp_sex=="男"){
    filtered_matr[i,(count+1)] <- 1
  }
  else if (temp_sex=="女"){
    filtered_matr[i,(count+1)] <- 0
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"性别")

### 年龄
for (i in 1:nrow(raw_data_matrix)){
  filtered_matr[i,(count+1)] <- as.numeric(raw_data_matrix[i,"年龄"])
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"年龄")



### 全血细胞分析 *白细胞
grep("*白细胞",colnames(raw_data_matrix))###329,391
# View(raw_data_matrix[,grep("*白细胞",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,329])){
    filtered_matr[i,(count+1)] <- as.numeric(raw_data_matrix[i,329])
  }
  else if (!is.na(raw_data_matrix[i,391])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,391])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"白细胞")

### *丙氨酸氨基转移酶
grep("*丙氨酸氨基转移酶",colnames(raw_data_matrix))###92,525,731
# View(raw_data_matrix[,grep("*丙氨酸氨基转移酶",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,92])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,92])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,525])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,525])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,731])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,731])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"ALT")


### *钙
grep("*钙",colnames(raw_data_matrix))###172,544
# View(raw_data_matrix[,grep("*钙",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,172])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,172])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,544])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,544])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血钙")

### *甘油三酯
grep("*甘油三酯",colnames(raw_data_matrix))###161,480,511
# View(raw_data_matrix[,grep("*甘油三酯",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,161])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,161])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,480])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,480])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,511])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,511])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"甘油三酯")

### *谷氨酰转肽酶
grep("*谷氨酰转肽酶",colnames(raw_data_matrix))###207,621
# View(raw_data_matrix[,grep("*谷氨酰转肽酶",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,207])){
    # temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,207])
    filtered_matr[i,(count+1)] <- as.numeric(raw_data_matrix[i,207])
  }
  else if (!is.na(raw_data_matrix[i,621])){
    # temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,621])
    filtered_matr[i,(count+1)] <- as.numeric(raw_data_matrix[i,621])
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"谷氨酰转肽酶")

### *红细胞
grep("*红细胞",colnames(raw_data_matrix))###83
# View(raw_data_matrix[,grep("*红细胞",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,83])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,83])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"红细胞")

### *红细胞压积
grep("*红细胞压积",colnames(raw_data_matrix))###261
# View(raw_data_matrix[,grep("*红细胞压积",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,261])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,261])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"红细胞压积")

### *肌酐(酶法)
grep("*肌酐",colnames(raw_data_matrix))###245,451
# View(raw_data_matrix[,grep("*肌酐",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,245])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,245])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,451])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,451])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"肌酐")

# ### *肌酸激酶
# grep("*肌酸激酶",colnames(raw_data_matrix))###162,173
# # View(raw_data_matrix[,grep("*肌酸激酶",colnames(raw_data_matrix))])
# for (i in 1:nrow(raw_data_matrix)){
#   if (!is.na(raw_data_matrix[i,162])){
#     temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,162])
#     filtered_matr[i,(count+1)] <- as.numeric(temp_value)
#   }
#   else if (!is.na(raw_data_matrix[i,173])){
#     temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,173])
#     filtered_matr[i,(count+1)] <- as.numeric(temp_value)
#   }
# }
# count <- count+1
# filtered_colnames <- c(filtered_colnames,"肌酸激酶")

### *钾
grep("*钾",colnames(raw_data_matrix))###87,578
# View(raw_data_matrix[,grep("*钾",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,87])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,87])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,578])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,578])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"钾")

### *碱性磷酸酶
grep("*碱性磷酸酶",colnames(raw_data_matrix))###20,517
# View(raw_data_matrix[,grep("*碱性磷酸酶",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,20])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,20])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,517])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,517])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"碱性磷酸酶")

### *氯
grep("*氯",colnames(raw_data_matrix))###223,550
# View(raw_data_matrix[,grep("*氯",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,223])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,223])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,550])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,550])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"氯")

### *钠
grep("*钠",colnames(raw_data_matrix))###252,546
# View(raw_data_matrix[,grep("*钠",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,252])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,252])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,546])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,546])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"钠")

### *尿素
grep("*尿素",colnames(raw_data_matrix))###255,554
# View(raw_data_matrix[,grep("*尿素",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,255])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,255])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,554])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,554])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿素")

### *尿酸
grep("*尿酸",colnames(raw_data_matrix))###221,521
# View(raw_data_matrix[,grep("*尿酸",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,221])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,221])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,521])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,521])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿酸")

### *平均红细胞体积
grep("*平均红细胞体积",colnames(raw_data_matrix))###67
# View(raw_data_matrix[,grep("*平均红细胞体积",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,67])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,67])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"平均红细胞体积")

### *平均红细胞血红蛋白
grep("*平均红细胞血红蛋白",colnames(raw_data_matrix))###174
# View(raw_data_matrix[,grep("*平均红细胞血红蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,174])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,174])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"平均红细胞血红蛋白")

### *平均红细胞血红蛋白浓度
grep("*平均红细胞血红蛋白浓度",colnames(raw_data_matrix))###202
# View(raw_data_matrix[,grep("*平均红细胞血红蛋白浓度",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,202])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,202])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"平均红细胞血红蛋白浓度")

### *葡萄糖
grep("*葡萄糖",colnames(raw_data_matrix))###171,524
# View(raw_data_matrix[,grep("*葡萄糖",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,171])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,171])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,524])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,524])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"葡萄糖")

### *乳酸脱氢酶
grep("*乳酸脱氢酶",colnames(raw_data_matrix))###11,558
# View(raw_data_matrix[,grep("*乳酸脱氢酶",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,11])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,11])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,558])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,558])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"乳酸脱氢酶")

### *天门冬氨酸氨基转移酶
grep("*天门冬氨酸氨基转移酶",colnames(raw_data_matrix))###150,547
# View(raw_data_matrix[,grep("*天门冬氨酸氨基转移酶",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,150])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,150])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,547])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,547])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"天门冬氨酸氨基转移酶")

### *无机磷
grep("*无机磷",colnames(raw_data_matrix))###294,549
# View(raw_data_matrix[,grep("*无机磷",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,294])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,294])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,549])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,549])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"无机磷")

### *血红蛋白
grep("*血红蛋白",colnames(raw_data_matrix))###17
# View(raw_data_matrix[,grep("*血红蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,17])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,17])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血红蛋白")

### *血小板
grep("*血小板",colnames(raw_data_matrix))###110,440
# View(raw_data_matrix[,grep("*血小板",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,110])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,110])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,440])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,440])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血小板")

### *总胆固醇
grep("*总胆固醇",colnames(raw_data_matrix))###282,512,542
# View(raw_data_matrix[,grep("*总胆固醇",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,282])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,282])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,512])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,512])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,542])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,542])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"总胆固醇")

### 纯真CD4+T细胞%
# grep("45RA",colnames(raw_data_matrix))###287,416
# View(raw_data_matrix[,grep("45RA",colnames(raw_data_matrix))])
grep("CD4+",colnames(raw_data_matrix))###157,325
# View(raw_data_matrix[,grep("CD4+",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,157])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,157])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,325])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,325])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"纯真CD4+T细胞%")

### B淋巴细胞CD19
grep("B淋巴细胞CD19",colnames(raw_data_matrix))###220,479,829,1057
# View(raw_data_matrix[,grep("B淋巴细胞CD19",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,220])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,220])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,479])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,479])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,829])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,829])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1057])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1057])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"B淋巴细胞CD19")

### CD4/CD8%
grep("CD4/CD8",colnames(raw_data_matrix))###178,307,690,696
# View(raw_data_matrix[,grep("CD4/CD8",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,178])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,178])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,307])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,307])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,690])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,690])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,696])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,696])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD4/CD8%")

## CD4+CD28+/CD4+%
grep("CD28+",colnames(raw_data_matrix))###72,309,782,1017
# View(raw_data_matrix[,grep("CD28+",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,72])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,72])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,309])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,309])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,782])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,782])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1017])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1017])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD4+CD28+/CD4+%")

### CD4+T细胞绝对值
grep("CD4+",colnames(raw_data_matrix))###311,1014
# View(raw_data_matrix[,grep("CD4+",colnames(raw_data_matrix))])
grep("T辅助细胞CD3+",colnames(raw_data_matrix))###199,884
# View(raw_data_matrix[,grep("T辅助细胞CD3+",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,311])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,311])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1014])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1014])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,199])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,199])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,884])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,884])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD4+T细胞绝对值")

## CD8+CD28+/CD8+%
grep("CD28+",colnames(raw_data_matrix))###351,375,596,794
# View(raw_data_matrix[,grep("CD28+",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,351])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,351])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,375])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,375])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,596])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,596])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,794])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,794])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD8+CD28+/CD8+%")

## CD8+CD38+/CD8+%
grep("CD38+",colnames(raw_data_matrix))###59,399,465,699
# View(raw_data_matrix[,grep("CD38+",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,59])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,59])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,399])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,399])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,465])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,465])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,699])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,699])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD8+CD38+/CD8+%")

## CD8+DR+/CD8+%
grep("DR+",colnames(raw_data_matrix))###167,314,328,502
# View(raw_data_matrix[,grep("DR+",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,167])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,167])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,314])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,314])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,328])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,328])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,502])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,502])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD8+DR+/CD8+%")

### CD8+T细胞绝对值
grep("CD8+",colnames(raw_data_matrix))###332,1022
# View(raw_data_matrix[,grep("CD8+",colnames(raw_data_matrix))])
grep("细胞毒细胞CD3",colnames(raw_data_matrix))###265,920
# View(raw_data_matrix[,grep("细胞毒细胞CD3",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,332])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,332])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1022])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1022])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,265])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,265])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,920])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,920])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD8+T细胞绝对值")


## T淋巴细胞CD3
grep("T淋巴细胞CD3",colnames(raw_data_matrix))###15,434,803,1063
# View(raw_data_matrix[,grep("T淋巴细胞CD3",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,15])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,15])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,434])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,434])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,803])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,803])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1063])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1063])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"CD3绝对值")

### α1球蛋白
grep("α1球蛋白",colnames(raw_data_matrix))###279,613
# View(raw_data_matrix[,grep("α1球蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,279])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,279])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,613])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,613])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"α1球蛋白")

### α2球蛋白
grep("α2球蛋白",colnames(raw_data_matrix))###104,676
# View(raw_data_matrix[,grep("α2球蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,104])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,104])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,676])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,676])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"α2球蛋白")

### β1球蛋白
grep("β1球蛋白",colnames(raw_data_matrix))###175,714
# View(raw_data_matrix[,grep("β1球蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,175])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,175])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,714])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,714])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"β1球蛋白")

### β2球蛋白
grep("β2球蛋白",colnames(raw_data_matrix))###169,728
# View(raw_data_matrix[,grep("β2球蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,169])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,169])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,728])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,728])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"β2球蛋白")

### γ球蛋白
grep("γ球蛋白",colnames(raw_data_matrix))###142,725
# View(raw_data_matrix[,grep("γ球蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,142])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,142])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,725])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,725])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"γ球蛋白")

### 癌胚抗原
grep("癌胚抗原",colnames(raw_data_matrix))###122
# View(raw_data_matrix[,grep("癌胚抗原",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,122])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,122])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"癌胚抗原")

### 血清蛋白电泳白蛋白
grep("白蛋白",colnames(raw_data_matrix))###81,687
# View(raw_data_matrix[,grep("白蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,81])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,81])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,687])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,687])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血清蛋白电泳白蛋白")

### 血白蛋白
grep("白蛋白",colnames(raw_data_matrix))###312,523
# View(raw_data_matrix[,grep("白蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,312])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,312])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,523])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,523])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血白蛋白")

### 白蛋白/球蛋白
grep("白蛋白/球蛋白",colnames(raw_data_matrix))###48,734
# View(raw_data_matrix[,grep("白蛋白/球蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,48])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,48])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,734])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,734])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"电泳白球比")

### 白蛋白球蛋白比
grep("白蛋白球蛋白比",colnames(raw_data_matrix))###192,606
# View(raw_data_matrix[,grep("白蛋白球蛋白比",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,192])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,192])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,606])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,606])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血生化白球比")

### 比重
grep("比重",colnames(raw_data_matrix))###74
# View(raw_data_matrix[,grep("比重",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,74])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,74])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿比重")

### 超敏C反应蛋白
grep("超敏C反应蛋白",colnames(raw_data_matrix))###195,541
# View(raw_data_matrix[,grep("超敏C反应蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,195])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,195])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,541])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,541])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"超敏C反应蛋白")

### 促甲状腺激素
grep("促甲状腺激素",colnames(raw_data_matrix))###111,506,833
# View(raw_data_matrix[,grep("促甲状腺激素",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,111])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,111])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,506])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,506])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,833])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,833])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"促甲状腺激素")

### 单核细胞百分比
grep("单核细胞百分比",colnames(raw_data_matrix))###179
# View(raw_data_matrix[,grep("单核细胞百分比",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,179])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,179])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"单核细胞百分比")

### 胆碱酯酶
grep("胆碱酯酶",colnames(raw_data_matrix))###55
# View(raw_data_matrix[,grep("胆碱酯酶",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,55])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,55])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"胆碱酯酶")

### 低密度脂蛋白胆固醇
grep("低密度脂蛋白胆固醇",colnames(raw_data_matrix))###146,518,583
# View(raw_data_matrix[,grep("低密度脂蛋白胆固醇",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,146])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,146])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,518])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,518])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,583])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,583])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"低密度脂蛋白胆固醇")

### 高密度脂蛋白胆固醇
grep("高密度脂蛋白胆固醇",colnames(raw_data_matrix))###58,531,548
# View(raw_data_matrix[,grep("高密度脂蛋白胆固醇",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,58])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,58])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,531])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,531])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,548])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,548])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"高密度脂蛋白胆固醇")

### 红细胞体积分布宽度
grep("红细胞体积分布宽度",colnames(raw_data_matrix))###244,264,335
# View(raw_data_matrix[,grep("红细胞体积分布宽度",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,244])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,244])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,335])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,335])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"红细胞体积分布宽度")

## 记忆CD4+T细胞%
grep("记忆",colnames(raw_data_matrix))###14,333
# View(raw_data_matrix[,grep("记忆",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,14])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,14])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,333])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,333])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"记忆CD4+T细胞%")


### 甲胎蛋白
grep("甲胎蛋白",colnames(raw_data_matrix))###248
# View(raw_data_matrix[,grep("甲胎蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,248])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,248])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"甲胎蛋白")

### 甲状腺过氧化物酶抗体
grep("甲状腺过氧化物酶抗体",colnames(raw_data_matrix))###281,591
# View(raw_data_matrix[,grep("甲状腺过氧化物酶抗体",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,281])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,281])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,591])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,591])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"甲状腺过氧化物酶抗体")

### 甲状腺球蛋白抗体
grep("甲状腺球蛋白抗体",colnames(raw_data_matrix))###23,559
# View(raw_data_matrix[,grep("甲状腺球蛋白抗体",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,23])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,23])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,559])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,559])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"甲状腺球蛋白抗体")

### 甲状腺素
grep("甲状腺素",colnames(raw_data_matrix))###186,509,1011
# View(raw_data_matrix[,grep("甲状腺素",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,186])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,186])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,509])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,509])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1011])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1011])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"甲状腺素")

### 类风湿因子
grep("类风湿因子",colnames(raw_data_matrix))###78,458,522
# View(raw_data_matrix[,grep("类风湿因子",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,78])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,78])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,458])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,458])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,522])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,522])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"类风湿因子")

### 免疫球蛋白A
grep("免疫球蛋白A",colnames(raw_data_matrix))###361,387,950
# View(raw_data_matrix[,grep("免疫球蛋白A",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,361])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,361])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,387])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,387])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,950])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,950])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"免疫球蛋白A")

### 免疫球蛋白G
grep("免疫球蛋白G",colnames(raw_data_matrix))###151,474,871
# View(raw_data_matrix[,grep("免疫球蛋白G",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,151])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,151])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,474])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,474])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,871])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,871])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"免疫球蛋白G")

### 免疫球蛋白M
grep("免疫球蛋白M",colnames(raw_data_matrix))###40,379,949
# View(raw_data_matrix[,grep("免疫球蛋白M",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,40])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,40])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,379])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,379])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,949])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,949])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"免疫球蛋白M")

### 淋巴细胞百分比
grep("淋巴细胞百分比",colnames(raw_data_matrix))###318
# View(raw_data_matrix[,grep("淋巴细胞百分比",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,318])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,318])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"淋巴细胞百分比")

### 尿白蛋白肌酐比值
grep("尿白蛋白肌酐比值",colnames(raw_data_matrix))###160,785
# View(raw_data_matrix[,grep("尿白蛋白肌酐比值",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,160])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,160])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,785])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,785])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿白蛋白肌酐比值")

### 尿胆原
grep("尿胆原",colnames(raw_data_matrix))###345
# View(raw_data_matrix[,grep("尿胆原",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,345])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,345])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿胆原")

### 尿肌酐
grep("尿肌酐",colnames(raw_data_matrix))###198,784
# View(raw_data_matrix[,grep("尿肌酐",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,198])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,198])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,784])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,784])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿肌酐")

### 尿微量白蛋白
grep("尿微量白蛋白",colnames(raw_data_matrix))###214,733
# View(raw_data_matrix[,grep("尿微量白蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,214])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,214])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,733])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,733])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿微量白蛋白")

### 凝血酶时间
grep("凝血酶时间",colnames(raw_data_matrix))###54
# View(raw_data_matrix[,grep("凝血酶时间",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,54])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,54])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"凝血酶时间")

### 平均血小板体积
grep("平均血小板体积",colnames(raw_data_matrix))###162
# View(raw_data_matrix[,grep("平均血小板体积",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,162])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,162])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"平均血小板体积")

### 前白蛋白
grep("前白蛋白",colnames(raw_data_matrix))###155,1098
# View(raw_data_matrix[,grep("前白蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,155])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,155])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1098])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1098])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"前白蛋白")

### 三碘甲状腺原氨酸
grep("三碘甲状腺原氨酸",colnames(raw_data_matrix))###224,557,1012
# View(raw_data_matrix[,grep("三碘甲状腺原氨酸",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,224])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,224])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,557])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,557])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1012])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1012])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"三碘甲状腺原氨酸")

# ### 身高
# grep("身高",colnames(raw_data_matrix))###252
# # View(raw_data_matrix[,grep("身高",colnames(raw_data_matrix))])
# for (i in 1:nrow(raw_data_matrix)){
#   if (!is.na(raw_data_matrix[i,252])){
#     temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,252])
#     filtered_matr[i,(count+1)] <- as.numeric(temp_value)
#   }
# }
# count <- count+1
# filtered_colnames <- c(filtered_colnames,"身高")

### 嗜碱性粒细胞百分比
grep("嗜碱性粒细胞百分比",colnames(raw_data_matrix))###164
# View(raw_data_matrix[,grep("嗜碱性粒细胞百分比",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,164])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,164])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"嗜碱性粒细胞百分比")

### 嗜酸性粒细胞百分比
grep("嗜酸性粒细胞百分比",colnames(raw_data_matrix))###147
# View(raw_data_matrix[,grep("嗜酸性粒细胞百分比",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,147])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,147])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"嗜酸性粒细胞百分比")

### 收缩压
grep("收缩压",colnames(raw_data_matrix))###96
# View(raw_data_matrix[,grep("收缩压",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,96])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,96])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"收缩压")

### 舒张压
grep("舒张压",colnames(raw_data_matrix))###305
# View(raw_data_matrix[,grep("舒张压",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,305])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,305])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"舒张压")

### 尿酸碱度
grep("酸碱度",colnames(raw_data_matrix))###116
# View(raw_data_matrix[,grep("酸碱度",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,116])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,116])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"尿酸碱度")

### 糖化白蛋白含量
grep("糖化白蛋白含量",colnames(raw_data_matrix))###132
# View(raw_data_matrix[,grep("糖化白蛋白含量",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,132])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,132])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"糖化白蛋白含量")

### 糖化血红蛋白
grep("糖化血红蛋白",colnames(raw_data_matrix))###286
# View(raw_data_matrix[,grep("糖化血红蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,286])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,286])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"糖化血红蛋白")


### 体质指数
grep("体质指数",colnames(raw_data_matrix))###316,446,600
# View(raw_data_matrix[,grep("体质指数",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,316])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,316])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,446])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,446])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,600])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,600])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"体质指数")



### 同型半胱氨酸
grep("同型半胱氨酸",colnames(raw_data_matrix))###120
# View(raw_data_matrix[,grep("同型半胱氨酸",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,120])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,120])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"同型半胱氨酸")

### 胃蛋白酶原I
grep("胃蛋白酶原I",colnames(raw_data_matrix))###182
# View(raw_data_matrix[,grep("胃蛋白酶原I",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,182])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,182])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"胃蛋白酶原I")

### 胃蛋白酶原I/II
grep("胃蛋白酶原II",colnames(raw_data_matrix))###71
# View(raw_data_matrix[,grep("胃蛋白酶原I",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,71])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,71])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"胃蛋白酶原I/II")

### 心率
grep("心率",colnames(raw_data_matrix))###98,917
# View(raw_data_matrix[,grep("心率",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,98])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,98])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,917])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,917])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"心率")

### 血小板体积分布宽度
grep("血小板体积分布宽度",colnames(raw_data_matrix))###268
# View(raw_data_matrix[,grep("血小板体积分布宽度",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,268])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,268])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血小板体积分布宽度")

### 血小板压积
grep("血小板压积",colnames(raw_data_matrix))###35
# View(raw_data_matrix[,grep("血小板压积",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,35])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,35])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"血小板压积")

### 腰围
grep("腰围",colnames(raw_data_matrix))###90
# View(raw_data_matrix[,grep("腰围",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,90])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,90])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"腰围")

### 游离甲状腺素
grep("游离甲状腺素",colnames(raw_data_matrix))###304,590,819
# View(raw_data_matrix[,grep("游离甲状腺素",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,304])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,304])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,590])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,590])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,819])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,819])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"游离甲状腺素")

### 游离前列腺特异性抗原
grep("游离前列腺特异性抗原",colnames(raw_data_matrix))###124,346
# View(raw_data_matrix[,grep("游离前列腺特异性抗原",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,124])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,124])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,346])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,346])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"游离前列腺特异性抗原")

### 游离三碘甲状腺原氨酸
grep("游离三碘甲状腺原氨酸",colnames(raw_data_matrix))###7,620,824
# View(raw_data_matrix[,grep("游离三碘甲状腺原氨酸",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,7])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,7])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,620])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,620])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,824])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,824])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"游离三碘甲状腺原氨酸")

### 游离脂肪酸
grep("游离脂肪酸",colnames(raw_data_matrix))###217
# View(raw_data_matrix[,grep("游离脂肪酸",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,217])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,217])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"游离脂肪酸")

### 载脂蛋白A1
grep("载脂蛋白A1",colnames(raw_data_matrix))###141
# View(raw_data_matrix[,grep("载脂蛋白A1",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,141])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,141])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"载脂蛋白A1")

### 载脂蛋白B
grep("载脂蛋白B",colnames(raw_data_matrix))###133
# View(raw_data_matrix[,grep("载脂蛋白B",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,133])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,133])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"载脂蛋白B")

### 脂蛋白(a)
grep("脂蛋白",colnames(raw_data_matrix))###121
# View(raw_data_matrix[,grep("脂蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,121])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,121])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"脂蛋白(a)")

### 中性粒细胞百分比
grep("中性粒细胞百分比",colnames(raw_data_matrix))###290
# View(raw_data_matrix[,grep("中性粒细胞百分比",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,290])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,290])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"中性粒细胞百分比")

### 直接胆红素
grep("直接胆红素",colnames(raw_data_matrix))###82,510
# View(raw_data_matrix[,grep("直接胆红素",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,82])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,82])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,510])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,510])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"直接胆红素")

## NK细胞绝对值
grep("自然杀伤细胞CD16",colnames(raw_data_matrix))###44,321,774,1070
# View(raw_data_matrix[,grep("自然杀伤细胞CD16",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,44])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,44])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,321])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,321])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,774])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,774])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,1070])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,1070])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"NK细胞绝对值")

### 总胆红素
grep("总胆红素",colnames(raw_data_matrix))###156,553
# View(raw_data_matrix[,grep("总胆红素",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,156])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,156])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,553])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,553])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"总胆红素")

### 总蛋白
grep("总蛋白",colnames(raw_data_matrix))###185,515
# View(raw_data_matrix[,grep("总蛋白",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,185])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,185])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
  else if (!is.na(raw_data_matrix[i,515])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,515])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"总蛋白")

### 总胆汁酸
grep("总胆汁酸",colnames(raw_data_matrix))###277
# View(raw_data_matrix[,grep("总胆汁酸",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,277])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,277])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"总胆汁酸")

### 总二氧化碳
grep("总二氧化碳",colnames(raw_data_matrix))###63
# View(raw_data_matrix[,grep("总二氧化碳",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,63])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,63])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"总二氧化碳")

### 总免疫球蛋白E
grep("总免疫球蛋白E",colnames(raw_data_matrix))###125
# View(raw_data_matrix[,grep("总免疫球蛋白E",colnames(raw_data_matrix))])
for (i in 1:nrow(raw_data_matrix)){
  if (!is.na(raw_data_matrix[i,125])){
    temp_value <- gsub("[^0-9.]", "", raw_data_matrix[i,125])
    filtered_matr[i,(count+1)] <- as.numeric(temp_value)
  }
}
count <- count+1
filtered_colnames <- c(filtered_colnames,"总免疫球蛋白E")



filtered_matr <- filtered_matr[,1:count]
colnames(filtered_matr) <- filtered_colnames

write.csv(filtered_matr,"xiehe_filtered_matr_260312.csv",row.names = F,quote=T,fileEncoding = "GB18030")





features <- unique(filtered_colnames)

feature_count <- matrix(0,length(features),2)

for (i in 1:length(features)){
  temp_index <- which(colnames(filtered_matr)==features[i],T)
  feature_count[i,1] <- features[i]
  if (length(temp_index)>0){
    non_na_no <- length(which(is.na(filtered_matr[,temp_index])==F,T))
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
  temp_all_data <- filtered_matr[,which(colnames(filtered_matr)%in%temp_items,T)]
  temp_na_index <- which(is.na(temp_all_data)==T,T)
  if (length(temp_na_index)>0){
    temp_all_na_index <- unique(temp_na_index[,1])
    temp_all_valid_num <- nrow(temp_all_data)-length(temp_all_na_index)
  }
  else{
    temp_all_valid_num <- nrow(filtered_matr)
  }
  all_count4[i,1] <- all_items[i]
  all_count4[i,2] <- temp_all_valid_num
}
colnames(all_count4) <- c("变量","截至目前可用数据量")

plot(1:nrow(all_count4), as.numeric(all_count4[,2]), type = "l", col = "red", lwd = 2)
write.csv(all_count4,"all_count4_xiehe_060304.csv",row.names = F,quote=T,fileEncoding = "GB18030")
valid_features <- all_count4[1:95,1]
valid_data <- filtered_matr[,which(colnames(filtered_matr)%in%valid_features,T)]
na_index <- unique(which(is.na(valid_data)==T,T)[,1])
new_valid_data <- valid_data[-na_index,]


# new_valid_data <- new_valid_data0[which(new_valid_data0[,5]>=60,T),]


male_data <- new_valid_data[which(new_valid_data[,4]==1,T),-c(1:4)]
female_data <- new_valid_data[which(new_valid_data[,4]==0,T),-c(1:4)]
dim(male_data)#7636   91
dim(female_data)#2615   91





######### remove severe disease



new_valid_info <- matrix("",nrow(new_valid_data),2)

for (i in 1:nrow(new_valid_data)){
  temp_id <- new_valid_data[i,1]
  temp_index <- which(data21[,2]==temp_id,T)
  new_valid_info[i,1] <- temp_id
  new_valid_info[i,2] <- data21[temp_index,8]
}
colnames(new_valid_info) <- c("就诊id","报告单")


###cancer&immune
cancer_immune_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("癌",new_valid_info[i,2])
  if (length(temp_index)>0){
    cancer_immune_index <- c(cancer_immune_index,i)
  }
  temp_index <- grep("免疫病",new_valid_info[i,2])
  if (length(temp_index)>0){
    cancer_immune_index <- c(cancer_immune_index,i)
  }
  temp_index <- grep("干燥综合征",new_valid_info[i,2])
  if (length(temp_index)>0){
    cancer_immune_index <- c(cancer_immune_index,i)
  }
  temp_index <- grep("系统性红斑狼疮",new_valid_info[i,2])
  if (length(temp_index)>0){
    cancer_immune_index <- c(cancer_immune_index,i)
  }
  temp_index <- grep("类风湿性关节炎",new_valid_info[i,2])
  if (length(temp_index)>0){
    cancer_immune_index <- c(cancer_immune_index,i)
  }
  temp_index <- grep("硬皮病",new_valid_info[i,2])
  if (length(temp_index)>0){
    cancer_immune_index <- c(cancer_immune_index,i)
  }
  temp_index <- grep("皮肌炎",new_valid_info[i,2])
  if (length(temp_index)>0){
    cancer_immune_index <- c(cancer_immune_index,i)
  }
}
cancer_immune_index <- unique(cancer_immune_index)
length(cancer_immune_index)###1350

## ultrasonic
ultrasonic_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("BI-RADS 4b",new_valid_info[i,2])
  if (length(temp_index)>0){
    ultrasonic_index <- c(ultrasonic_index,i)
  }
}
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("建议乳腺钼靶检查",new_valid_info[i,2])
  if (length(temp_index)>0){
    ultrasonic_index <- c(ultrasonic_index,i)
  }
}
length(ultrasonic_index)###75


### MPP
MPP_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("M蛋白",new_valid_info[i,2])
  if (length(temp_index)>0){
    MPP_index <- c(MPP_index,i)
  }
}
length(MPP_index)####77


### tumor
library(stringr)
tumor_index <- c()

pattern <- "癌胚抗原：\\s*([0-9]+\\.?[0-9]*)\\s*ng/m"
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("癌胚抗原",new_valid_info[i,2])
  if (length(temp_index)>0){
    match_result <- str_match(new_valid_info[i,2], pattern)
    if (!is.na(match_result[1,2])){
      value <- as.numeric(match_result[1,2])
      if (value>5){
        tumor_index <- c(tumor_index,i)
      }
    }
    
  }
}

pattern <- "糖链抗原72-4：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("糖链抗原72-4",new_valid_info[i,2])
  if (length(temp_index)>0){
    match_result <- str_match(new_valid_info[i,2], pattern)
    if (!is.na(match_result[1,2])){
      value <- as.numeric(match_result[1,2])
      if (value>9.8){
        tumor_index <- c(tumor_index,i)
      }
    }
    
  }
}

pattern <- "糖链抗原15-3：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("糖链抗原15-3",new_valid_info[i,2])
  if (length(temp_index)>0){
    match_result <- str_match(new_valid_info[i,2], pattern)
    if (!is.na(match_result[1,2])){
      value <- as.numeric(match_result[1,2])
      if (value>25){
        tumor_index <- c(tumor_index,i)
      }
    }
    
  }
}

pattern <- "糖链抗原19-9：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("糖链抗原19-9",new_valid_info[i,2])
  if (length(temp_index)>0){
    match_result <- str_match(new_valid_info[i,2], pattern)
    if (!is.na(match_result[1,2])){
      value <- as.numeric(match_result[1,2])
      if (value>37){
        tumor_index <- c(tumor_index,i)
      }
    }
    
  }
}

pattern <- "糖链抗原125：\\s*([0-9]+\\.?[0-9]*)\\s*U/ml"
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("糖链抗原125",new_valid_info[i,2])
  if (length(temp_index)>0){
    match_result <- str_match(new_valid_info[i,2], pattern)
    if (!is.na(match_result[1,2])){
      value <- as.numeric(match_result[1,2])
      if (value>35){
        tumor_index <- c(tumor_index,i)
      }
    }
    
  }
}

# grep("糖链抗原",colnames(data_left))#88
# grep("癌胚抗原",colnames(data_left))#47
# tumor_index <- c(tumor_index,which(data_left[,88]>37,T))
# tumor_index <- c(tumor_index,which(data_left[,47]>5,T))
length(tumor_index)####632

### antibody
antibody_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("自身抗体谱异常",new_valid_info[i,2])
  if (length(temp_index)>0){
    match_result <- str_match(new_valid_info[i,2], "抗核抗体\\(IgG型\\)([\\s\\S]*?)\\(<1:40 \\)\n")[1,2]
    temp_index1 <- grep("1:80",match_result)
    if (length(temp_index1)==0){
      antibody_index <- c(antibody_index,i)
    }
  }
}
length(antibody_index)####767


### lung
lung_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("磨玻璃结节",new_valid_info[i,2])
  if (length(temp_index)>0){
    lung_index <- c(lung_index,i)
  }
}
length(lung_index)####157

###inflammation
grep("超敏C反应蛋白",colnames(new_valid_data))##51
inflammation_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("血沉",new_valid_info[i,2])
  if (length(temp_index)>0){
    temp_value <- sub(".*?血沉：(.*?) mm/h.*", "\\1", new_valid_info[i,2])
    if (length(temp_value)>0){
      temp_num <- as.numeric(temp_value)
      if (temp_num>20){
        inflammation_index <- c(inflammation_index,i)
      }
    }
  }
  if (new_valid_data[i,51]>10){
    inflammation_index <- c(inflammation_index,i)
  }
  
}
inflammation_index <- unique(inflammation_index)
length(inflammation_index)####369



###ARF
ARF_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("类风湿因子异常",new_valid_info[i,2])
  if (length(temp_index)>0){
    temp_value <- sub(".*?类风湿因子：(.*?) IU/ml.*", "\\1", new_valid_info[i,2])
    if (length(temp_value)>0){
      temp_num <- as.numeric(temp_value)
      if (temp_num>20){
        ARF_index <- c(ARF_index,i)
      }
    }
  }
  
}

length(ARF_index)####349

### PET
PET_index <- c()
for (i in 1:nrow(new_valid_info)){
  temp_index <- grep("代谢异常增高",new_valid_info[i,2])
  if (length(temp_index)>0){
    PET_index <- c(PET_index,i)
  }
}
length(PET_index)####3



abnormal_index <- c(cancer_immune_index,ultrasonic_index,MPP_index,tumor_index,antibody_index,lung_index,inflammation_index,ARF_index,PET_index)
abnormal_index <- unique(abnormal_index)
length(abnormal_index)###2872，剩下7379

data_left1 <- new_valid_data[-abnormal_index,]


all_data <- data_left1[,-c(1:3)]
all_data[,c(1,2)] <- all_data[,c(2,1)]
colnames(all_data)[c(1,2)] <- colnames(all_data)[c(2,1)]

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


all_temp_index2 <- which(all_data[,1]>0.3 & all_data[,1]<=0.4,T)
all_temp_num2 <- ceiling(length(all_temp_index2)*0.2)
all_temp_index22 <- sample(1:length(all_temp_index2),all_temp_num2)
all_temp_test_index2 <- all_temp_index2[all_temp_index22]
all_temp_train_index2 <- all_temp_index2[-all_temp_index22]
all_train_index <- c(all_train_index,all_temp_train_index2)
all_test_index <- c(all_test_index,all_temp_test_index2)

all_temp_index3 <- which(all_data[,1]>0.4 & all_data[,1]<=0.5,T)
all_temp_num3 <- ceiling(length(all_temp_index3)*0.2)
all_temp_index32 <- sample(1:length(all_temp_index3),all_temp_num3)
all_temp_test_index3 <- all_temp_index3[all_temp_index32]
all_temp_train_index3 <- all_temp_index3[-all_temp_index32]
all_train_index <- c(all_train_index,all_temp_train_index3)
all_test_index <- c(all_test_index,all_temp_test_index3)

all_temp_index4 <- which(all_data[,1]>0.5 & all_data[,1]<=0.6,T)
all_temp_num4 <- ceiling(length(all_temp_index4)*0.2)
all_temp_index42 <- sample(1:length(all_temp_index4),all_temp_num4)
all_temp_test_index4 <- all_temp_index4[all_temp_index42]
all_temp_train_index4 <- all_temp_index4[-all_temp_index42]
all_train_index <- c(all_train_index,all_temp_train_index4)
all_test_index <- c(all_test_index,all_temp_test_index4)

all_temp_index5 <- which(all_data[,1]>0.6 & all_data[,1]<=0.7,T)
all_temp_num5 <- ceiling(length(all_temp_index5)*0.2)
all_temp_index52 <- sample(1:length(all_temp_index5),all_temp_num5)
all_temp_test_index5 <- all_temp_index5[all_temp_index52]
all_temp_train_index5 <- all_temp_index5[-all_temp_index52]
all_train_index <- c(all_train_index,all_temp_train_index5)
all_test_index <- c(all_test_index,all_temp_test_index5)

all_temp_index6 <- which(all_data[,1]>0.7,T)
all_temp_num6 <- ceiling(length(all_temp_index6)*0.2)
all_temp_index62 <- sample(1:length(all_temp_index6),all_temp_num6)
all_temp_test_index6 <- all_temp_index6[all_temp_index62]
all_temp_train_index6 <- all_temp_index6[-all_temp_index62]
all_train_index <- c(all_train_index,all_temp_train_index6)
all_test_index <- c(all_test_index,all_temp_test_index6)


all_train <- all_data[all_train_index,]
all_test <- all_data[all_test_index,]


write.csv(all_train,"xiehe_all_train_data_20260312.csv",row.names = F,quote=T,fileEncoding = "GB18030")
write.csv(all_test,"xiehe_all_test_data_20260312.csv",row.names = F,quote=T,fileEncoding = "GB18030")

