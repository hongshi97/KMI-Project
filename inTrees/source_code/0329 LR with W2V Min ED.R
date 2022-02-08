library(dplyr)
library(glm2)
library(caret)
library(e1071)
library(pROC)

df <- read.csv('kyrbs2019_boy.csv')
df <- df %>% select(2:158)
head(df)

# 모든 변수를 일단 명목형으로 변환
for (i in names(df)){
  df[[i]] <- as.factor(df[[i]]) 
}
str(df)

# AGE는 연속형이니까 다시 숫자형으로 변환
df$AGE <- as.numeric(df$AGE)

# 순서형 변수인 변수들 factor(ordered) 이용해서 전처리
df$F_BR <- ordered(df$F_BR, levels=c(1:8))
df$F_FRUIT <- ordered(df$F_FRUIT, levels = c(1:7))
df$F_SODA <- ordered(df$F_SODA, levels = c(1:7))
df$F_CAFFEINE <- ordered(df$F_CAFFEINE, levels = c(1:7))
df$F_SWDRINK <- ordered(df$F_CAFFEINE, levels = c(1:7))
df$F_FASTFOOD <- ordered(df$F_FASTFOOD, levels = c(1:7))
df$F_VEG <- ordered(df$F_VEG, levels = c(1:7))
df$F_MILK <- ordered(df$F_MILK, levels = c(1:7))
df$F_CS <- ordered(df$F_CS, levels = c(1:7))
df$F_WAT <- ordered(df$F_WAT, levels = c(1:5))
df$PA_TOT <- ordered(df$PA_TOT, levels = c(1:8))
df$PA_VIG <- ordered(df$PA_VIG, levels = c(1:6))
df$PA_MSC <- ordered(df$PA_MSC, levels = c(1:6))
df$PA_WLK <- ordered(df$PA_WLK, levels = c(1:8))
df$PA_PE_FQ <- ordered(df$PA_PE_FQ, levels = c(1:4))
df$PA_ST <- ordered(df$PA_ST, levels = c(1:5))
df$PA_TRA <- ordered(df$PA_TRA, levels = c(1:8))
df$M_STR <- ordered(df$M_STR, levels = c(1:5))
df$M_SLP_EN <- ordered(df$M_SLP_EN, levels = c(1:5))
df$O_BR_FQ <- ordered(df$O_BR_FQ, levels = c(1:10))
df$O_BR_S <- ordered(df$O_BR_S, levels = c(1:4))
df$O_DC_FQ <- ordered(df$O_DC_FQ, levels = c(1:5))
df$PR_OH <- ordered(df$PR_OH, levels = c(1:5))
df$HW_SPML_S <- ordered(df$HW_SPML_S, levels = c(1:4))
df$HW_SPRM_S <- ordered(df$HW_SPRM_S, levels = c(1:4))
df$HW_SPML_H <- ordered(df$HW_SPML_H, levels = c(1:4))
df$HW_SPRM_H <- ordered(df$HW_SPRM_H, levels = c(1:4))
df$HW_SPGO_H <- ordered(df$HW_SPGO_H, levels = c(1:4))
df$I_SB_FR <- ordered(df$I_SB_FR, levels = c(1:5))
df$I_SB_BK <- ordered(df$I_SB_BK, levels = c(1:5))
df$I_SB_EX <- ordered(df$I_SB_EX, levels = c(1:5))
df$V_TRT <- ordered(df$V_TRT, levels = c(1:7))
df$TC_SND_H <- ordered(df$TC_SND_H, levels = c(1:8))
df$TC_SND_S <- ordered(df$TC_SND_S, levels = c(1:8))
df$TC_SND_P <- ordered(df$TC_SND_P, levels = c(1:8))
df$E_S_RCRD <- ordered(df$E_S_RCRD, levels = c(1:5))
df$E_SES <- ordered(df$E_SES, levels = c(1:5))
df$GRADE <- ordered(df$GRADE, levels = c(1:6))
df$SleepAvg_WK <- ordered(df$SleepAvg_WK, levels = c(0:20))
df$PA_SWD_S <- ordered(df$PA_SWD_S, levels = c(0:14))
df$PA_SWD_N <- ordered(df$PA_SWD_N, levels = c(0:7))
df$PA_SWK_S <- ordered(df$PA_SWK_S, levels = c(0:11))
df$PA_SWK_N <- ordered(df$PA_SWK_N, levels = c(0:12))
df$PA_WLK_MM <- ordered(df$PA_WLK_MM, levels = c(0:4))
df$PA_TRA_MM <- ordered(df$PA_TRA_MM, levels = c(0:3))
df$INT_WD_MM <- ordered(df$INT_WD_MM, levels = c(0:5))
df$INT_WK_MM <- ordered(df$INT_WK_MM, levels = c(0:10))
df$O_SYMP1_1 <- ordered(df$O_SYMP1_1, levels = c(0:2))
df$AC_FAGE <- ordered(df$AC_FAGE, levels = c(1:14))
df$AC_DAYS <- ordered(df$AC_DAYS, levels = c(0:7))
df$AC_AMNT <- ordered(df$AC_AMNT, levels = c(0:5))
df$AC_DRUNK <- ordered(df$AC_DRUNK, levels = c(0:4))
df$TC_DAYS <- ordered(df$TC_DAYS, levels = c(0:7))
df$TC_EC_MN <- ordered(df$TC_EC_MN, levels = c(0:7))
df$TC_HTP_MN <- ordered(df$TC_HTP_MN, levels = c(0:7))
df$TC_FAGE <- ordered(df$TC_FAGE, levels = c(1:14))
df$TC_AMNT <- ordered(df$TC_AMNT, levels = c(0:6))
df$DR_LT <- ordered(df$DR_LT, levels = c(0:3))
df$AS_DG_YR <- ordered(df$AS_DG_YR, levels = c(0:2))

str(df)

# Train / Test Split ( BMI 변수 기준 층화 추출 )
set.seed(10)
p <- 0.7
strats <- df$BMI

rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, function(x) sample(x, length(x) * p)))))

train <- df[idx,]
test <- df[-idx,]

train
test

table(df$BMI) / nrow(df)
table(train$BMI) / nrow(train)
table(test$BMI) / nrow(test)


# for문 활용 - 독립 변수 한 개씩 추가하면서 반복적으로 로지스틱 회귀모형 수립해서 모델 평가하기 ( Accuracy, Recall, Precision, F1 score, AUC, pR2)

# (W2V Min ED 상위 순서로 변수 목록 + BMI 변수) 이름을 rf_col에 넣기
rf_col <- c('E_LT_F',
            'E_FM_YBS_8',
            'E_FM_OBS_7',
            'E_LT_M',
            'WC_MN_M01',
            'AC_DRUNK',
            'AS_DG_YR',
            'RH_DG_LT',
            'E_FM_GM_6',
            'TC_GHW_H',
            'TC_DAYS',
            'O_SYMP6',
            'O_BR_SLP',
            'O_BR_FQ',
            'TC_AMNT',
            'M_STR',
            'SleepAvg_WK',
            'TC_SND_H',
            'INT_WD_MM',
            'M_SLP_EN',
            'INT_WK_MM',
            'I_SB_EX',
            'PR_HT',
            'F_CAFFEINE',
            'F_FRUIT',
            'F_SODA',
            'TC_SND_S',
            'AC_AMNT',
            'AC_DAYS',
            'O_DC_FQ',
            'E_S_RCRD',
            'TC_SND_P',
            'F_BR',
            'F_MILK',
            'PA_SWD_N',
            'PA_ST',
            'PA_SWD_S',
            'PA_SWK_S',
            'PA_TOT',
            'PA_SWK_N'
            ,'BMI')
length(rf_col)

train <- train %>% select(rf_col)
test <- test %>% select(rf_col)


accuracy_list <- c()
recall_list <- c()
precision_list <- c()
f1_score_list <- c()
added_var_list <- c()
auc_list_rf <- c()
r2_list_rf <- c()


for (i in 1:(length(rf_col)-1)){
  print(c(i,'번째 변수의 모델링을 시작합니다.'))
  train_glm <- train[,c(rf_col[1:i], 'BMI')]
  test_glm <- test[,c(rf_col[1:i], 'BMI')]
  
  glm2_model <- glm2(as.factor(BMI) ~ ., family = binomial, data = train_glm)
  
  test_glm_x <- subset(test_glm, select=-BMI)
  test_glm_y <- test_glm$BMI
  y_pred_probability <- predict(glm2_model, test_glm_x, type = 'response')   # 예측결과값이 0이나 1이 아닌 확률로 나옴
  y_pred <- ifelse(y_pred_probability > 0.5, 1, 0)                           # 예측결과값이 0.5보다 크면 1, 0.5보다 작으면 0으로 재할당
  y_pred <- factor(y_pred, levels= c(0,1))
  
  # 추가된 변수 표시
  added_var_list <- append(added_var_list, rf_col[i])
  Added_Var <- added_var_list
  
  cf = confusionMatrix(test_glm_y,y_pred, positive = "1")
  cm <- data.frame(cf[2])
  cm
  
  # Accuracy 표시
  accuracy <- (cm[1,3] + cm[4,3]) / (cm[1,3] + cm[2,3] + cm[3,3] + cm[4,3])
  accuracy_list <- append(accuracy_list, round(accuracy,4))
  Accuracy <- accuracy_list
  
  # Recall 표시
  recall <- cm[4,3]/(cm[2,3] + cm[4,3])
  recall_list <- append(recall_list, round(recall,4))
  Recall <- recall_list
  
  # Precision 표시
  precision <- cm[4,3]/(cm[3,3]+cm[4,3])
  precision_list <- append(precision_list, round(precision,4))
  Precision <- precision_list
  
  # F1 Score 표시
  f1_score <- 2*((precision*recall)/(precision+recall))
  f1_score_list <- append(f1_score_list, round(f1_score,4))
  F1_Score <- f1_score_list
  
  # AUC 표시
  auc <- auc(roc(test_glm_y, y_pred_probability))
  auc_list_rf <- append(auc_list_rf, round(auc,4))
  AUC <- auc_list_rf
  
  # R2 표시
  pR2 <- 1 - glm2_model$deviance / glm2_model$null.deviance
  r2_list_rf <- append(r2_list_rf, round(pR2, 4))
  R2 <- r2_list_rf
}

result <- data.frame(Added_Var, Accuracy, Recall, Precision, F1_Score, AUC, R2)
result

# W2V 최소 ED 이용 LR 성능 csv파일로 저장
write.csv(result, "Min ED LR result.csv")
