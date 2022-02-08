library(glm2)
library(dplyr)
library(caret)
library(e1071)
library(pROC)

min_X_train <- read.csv('min_X_train.csv')
min_X_test <- read.csv('min_X_test.csv')
min_y_train <- read.csv('min_y_train.csv')
min_y_test <- read.csv('min_y_test.csv')


min_X_train <- min_X_train %>% select(2:38)
min_X_test <- min_X_test %>% select(2:38)
min_y_train <- min_y_train %>% select(2)
min_y_test <- min_y_test %>% select(2)

min_train <- cbind(min_X_train, min_y_train)
min_test <- cbind(min_X_test, min_y_test)

names(min_train)

for (i in names(min_train)){
  min_train[[i]] <- as.factor(min_train[[i]]) 
}

for (i in names(min_test)){
  min_test[[i]] <- as.factor(min_test[[i]]) 
}

str(min_train)
str(min_train)

# LR 모델링 테스트
LR<-glm(BMI ~ E_LT_M+E_FM_YBS_8+WC_MN_M01+E_LT_F+AC_DRUNK+E_RES+TC_GHW_QT+E_SES+
          HW_SPRM_S+V_TRT+I_SCH_ED2+E_FM_GM_6+F_CS_F04+O_SYMP5+GRADE+
          O_SYMP6+HW_S_R+ECZ_DG_YR+SleepAvg_WK+INT_WD_MM+F_CAFFEINE+O_BR_FQ+
          F_FRUIT+PR_HT+TC_SND_S+E_FM_OBS_7+TC_SND_P+AC_DAYS+TC_SND_H+
          M_STR+PA_SWK_S+PA_SWD_S+INT_WK_MM+PA_PE_FQ+PA_SWD_N+PA_TRA+PA_SWK_N, family=binomial, data=min_train)
LR

# Confusion Matrix ( 다시 해보기 )
fitted.results <- predict(LR,newdata=subset(min_test,select=c(1:37)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results <- factor(fitted.results, levels = c(0,1))
min_test$BMI <- factor(min_test$BMI, levels = c(0,1))


cf = confusionMatrix(min_test$BMI,fitted.results, positive = '1')
cf
cf[3]
cf[4]


# AUC ( 다시 해보기 )
Probability = predict(LR, newdata=subset(min_test,select=c(1:37)), type = 'response')

ROC = roc(min_test$BMI,Probability)
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

