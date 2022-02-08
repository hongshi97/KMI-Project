library(glm2)
library(dplyr)
library(caret)
library(e1071)
library(pROC)

max_X_train <- read.csv('max_X_train.csv')
max_X_test <- read.csv('max_X_test.csv')
max_y_train <- read.csv('max_y_train.csv')
max_y_test <- read.csv('max_y_test.csv')


max_X_train <- max_X_train %>% select(2:38)
max_X_test <- max_X_test %>% select(2:38)
max_y_train <- max_y_train %>% select(2)
max_y_test <- max_y_test %>% select(2)

max_train <- cbind(max_X_train, max_y_train)
max_test <- cbind(max_X_test, max_y_test)

names(max_train)

for (i in names(max_train)){
  print(i) 
}

for (i in names(max_train)){
  max_train[[i]] <- as.factor(max_train[[i]]) 
}

for (i in names(max_test)){
  max_test[[i]] <- as.factor(max_test[[i]]) 
}

str(max_train)
str(max_test)

# LR 모델링 테스트
LR<-glm(BMI ~ SleepAvg_WK+E_LT_M+E_LT_F+E_FM_YBS_8+INT_WK_MM+E_FM_OBS_7+E_FM_GM_6+TC_GHW_QT+AC_DRUNK+GRADE+V_TRT+INT_WD_MM+WC_MN_M01+AC_DAYS+PR_HT+E_SES+ECZ_DG_YR+E_RES+
          F_CAFFEINE+HW_SPRM_S+O_BR_FQ+PA_SWK_N+TC_SND_S+F_FRUIT+TC_SND_P+M_STR+PA_SWD_S+PA_SWK_S+HW_S_R+TC_SND_H+PA_SWD_N+I_SCH_ED2+F_CS_F04+
          O_SYMP5+O_SYMP6+PA_PE_FQ+PA_TRA, family=binomial, data=max_train)  # 독립 변수 한 개만 넣은 상황
LR

# Confusion Matrix ( 다시 해보기 )
fitted.results <- predict(LR,newdata=subset(max_test,select=c(1:37)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results <- factor(fitted.results, levels = c(0,1))
max_test$BMI <- factor(max_test$BMI, levels = c(0,1))


cf = confusionMatrix(max_test$BMI,fitted.results, positive = '1')
cf
cf[3]
cf[4]

# Confusin matrix를 데이터프레임으로 전환해서 직접 Accuracy, Recall, Precision, F1 score 계산하는 수식 만들기
# 참고 : test 데이터 셋에 비만인 애들은 1715명

cm <- data.frame(cf[2])
cm
accuracy <- (cm[1,3] + cm[4,3]) / (cm[1,3] + cm[2,3] + cm[3,3] + cm[4,3])
accuracy

recall <- cm[4,3]/(cm[2,3] + cm[4,3])
recall

precision <- cm[4,3]/(cm[3,3]+cm[4,3])
precision

f1 <- 2*((precision*recall)/(precision+recall))
f1

# AUC ( 다시 해보기 )
Probability = predict(LR, newdata=subset(max_test,select=c(1:37)), type = 'response')

ROC = roc(max_test$BMI,Probability)
ROC[9]

plot.roc(ROC,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")

# R^2 계산 ( 로지스틱 회귀분석에서는 McFadden pseudo R square를 사용해서 모델 적합도를 판단한다고 함 )
pR2 <- 1 - LR$deviance / LR$null.deviance
pR2

