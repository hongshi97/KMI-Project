library(dplyr)

df <- read.csv('kyrbs2019_boy.csv')
df <- df %>% select(2:158)

# df 변수명 파악
names(df)

# 모든 변수를 일단 명목형으로 변환
for (i in names(df)){
  if ( i != 'AGE'){
    df[[i]] <- as.factor(df[[i]]) 
  }
}
class(df$AGE)

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

# PR_HT는 비만의 결과로 드러나는 변수라 판단하여 제거
df <- subset(df, select = -(PR_HT))

str(df)

# 순서형 변수 모음

ordered <- c()
for (i in names(df)){
  if (class(df[[i]]) == c("ordered", "factor")){
    ordered <- c(ordered,i)
  }
}
length(ordered)

# 명목형 변수 모음
nominal <- c()
for (i in names(df)){
  if (class(df[[i]]) == c("factor")){
    nominal <- c(nominal,i)
  }
}
length(nominal)

# 연속형 변수 모음
numerical <- c()
for (i in names(df)){
  if (class(df[[i]]) == c("numeric")){
    numerical <- c(numerical,i)
  }
}
length(numerical)

# 확인
length(ordered) + length(nominal) + length(numerical)


#### 상관계수 이용해서 변수 줄이기 ####
library(rcompanion)
library(lsr)

# BMI(종속 변수)와 명목형 변수들 간에 상관 계수 확인
# Cramer의 V2 값이 클수록 변수 간의 관계가 더 강하고, V2 값이 작을수록 변수 간의 관계가 약하다는 것을 나타냅니다. 
# 0 값은 연관성이 없음을 나타냅니다. 1 값은 변수 간에 매우 강한 연관성이 있음


nominal_vars<- c()
cramerV <- c()

for (i in nominal){
  temp <- table(df[[i]],df$BMI)
  nominal_vars <- c(nominal_vars,i)
  cramerV <- c(cramerV,cramersV(temp))
  nominal_cor <- data.frame(nominal_vars,cramerV)
}  
nominal_cor
nominal_cor <- arrange(nominal_cor, -cramerV)
nominal_important_vars <- nominal_cor %>% filter(cramerV >= 0.03) %>% select(nominal_vars)
nominal_important_vars <- nominal_important_vars$nominal_vars


# BMI(종속 변수)(명목형)와 순서형 변수들 간에 상관 계수 확인
library(polycor)
ordered_vars <- c()
polychor_vals <- c()

for (i in ordered){
  temp <- table(df[[i]], df$BMI)
  ordered_vars <- c(ordered_vars, i)
  polychor_vals <- c(polychor_vals,polychor(temp))
  abs_poly_vals <- abs(polychor_vals)
  ordered_cor <- data.frame(ordered_vars, polychor_vals, abs_poly_vals)
  
}
ordered_cor <- arrange(ordered_cor, -abs_poly_vals)
ordered_important_vars <- ordered_cor %>% filter(abs_poly_vals >= 0.06) %>% select(ordered_vars)
ordered_important_vars <- ordered_important_vars$ordered_vars

# BMI(종속 변수)와 연속형 변수들 간에 상관 계수 확인
cor.test(as.integer(df$BMI), as.integer(df$AGE))

# 상관 계수 기준 한 번 필터링 거친 변수들
cor_vars <- c(nominal_important_vars, ordered_important_vars, 'AGE')
cor_vars


################# 각 변수별 척도에 맞게 변수 타입 설정 끝 ##################

df_rf <- df %>% select(cor_vars)

# 사용된 변수들 메모리에서 삭제
rm(abs_poly_vals, cramerV, i, nominal, nominal_vars, nominal_important_vars, ordered_important_vars,ordered,ordered_vars,polychor_vals,temp,cor_vars, numerical, nominal_cor, ordered_cor)

str(df_rf)

table(df_rf$BMI) # 비만율 : 20.64%

# 랜덤 포레스트 모델링 (상관계수로 필터링한 독립 변수 사용한 경우)
library(inTrees)
library(RRF)
library(caret)

train.index <- createDataPartition(df_rf$BMI, p = .75, list = FALSE)
train <- df[ train.index,]
test  <- df[-train.index,]
table(train$BMI) # 약 20.64%  비만인 사람 5495명 
table(test$BMI)  # 약 20.64%  비만인 사람 1429명


#train <- upSample(subset(train, select = -BMI), train$BMI)
#table(train$Class)

# Class라고 바뀐 변수명 다시 BMI로 바꾸기
#library(reshape2)
#train <- rename(train, c('BMI' = Class))
#table(train$BMI)

X_cor_train <- train[,2:(ncol(df_rf))]
target_cor_train <- train[,"BMI"]
X_cor_test <- test[,2:(ncol(df_rf))]
target_cor_test <- test[,'BMI']

# Grid Search

ntrees = c(300,400,500,600)
mtrys = c(5,10,15,20)
nodesizes = c(1,5,10,15,20)

ntree_value <- c()
mtry_value <- c()
nodesize_value <- c()
F1_score <- c()

#temp_data <- X_cor_train[1:5,]

for (ntree in ntrees){
  for (mtry in mtrys){
    for (nodesize in nodesizes){
      
      rf_cor <- RRF(X_cor_train, as.factor(target_cor_train), ntree = ntree, mtry = mtry, nodesize = nodesize)
      
      bmi.pred <- predict(rf_cor, X_cor_test)
      temp <- confusionMatrix(data = bmi.pred, reference = target_cor_test, positive = '1')
      F1_result <- temp$byClass[7]
      
      ntree_value <- append(ntree_value, ntree)
      mtry_value <- append(mtry_value, mtry)
      nodesize_value <- append(nodesize_value, nodesize)
      F1_score <- c(F1_score, F1_result)
      }
    }
  }

rf_cor_pt_result <- data.frame(ntree_value, mtry_value, nodesize_value, F1_score)
rf_cor_pt_result
rf_cor_pt_result <- arrange(rf_cor_pt_result, -F1_score)
rf_cor_pt_result  # ntree = 400, mtry = 20, nodesize = 1일 때가 F1 Score가 0.3267로 그나마 가장 높음

###################################################################################################################### 


rf_cor <- RRF(X_cor_train,as.factor(target_cor_train)) # build an ordinary RF
rf_cor                             # 상관계수로 독립 변수들 걸러낸 후 수립된 랜덤 포레스트의 OOB error rate: 19.8%

bmi.pred <- predict(rf_cor, X_cor_test)
confusionMatrix(data = bmi.pred, reference = target_cor_test, positive = '1')



# tuneRRF를 이용해서 mtry 파라미터 튜닝
rf_col.tune <- tuneRRF(X_cor_train, target_cor_train, stepFactor=2, doBest = TRUE)


# 랜덤 포레스트 모델링 (전체 독립 변수 사용)
train.index <- createDataPartition(df$BMI, p = .75, list = FALSE)
train <- df[ train.index,]
test  <- df[-train.index,]

X_train <- train[,1:(ncol(df))-1]
target_train <- train[,'BMI']
X_test <- test[,1:(ncol(df))-1]
target_test <- test[,'BMI']

rrf.grid = expand.grid(
  .mtry = c(7,10,20,30,40,50,100,200,300,500) ,
  .ntree = c(7,10,20,30,40,50,100,200,300,500),
  .nodesize = c(7,10,20,30,40,50,100,200,300,500)
)

rf <- RRF(X_train,as.factor(target_train),tuneGrid = rrf.grid)

#rf <- RRF(X_train,as.factor(target_train)) # build an ordinary RF
rf                             # 상관계수로 독립 변수들 걸러낸 후 수립된 랜덤 포레스트의 OOB error rate: 19.8%

bmi.pred <- predict(rf, X_test)
cm <- confusionMatrix(data = bmi.pred, reference = target_test, positive = '1')
cm$byClass
rf$ntree
rf$mtry

rf.tune <- tuneRRF(X_train, target_train, stepFactor=3, doBest = TRUE)






####### 수립된 랜덤 포레스트로부터 Rule 추출 ####### 

treeList <- RF2List(rf)

# maxdepth = n 으로 수정하면 length가 n까지로 허용됨
ruleExec <- extractRules(treeList,X, maxdepth = 8) # transform to R-executable rules

sum(duplicated(ruleExec)) # ruleExec에서 중복된 행이 몇 개 있는지 확인

ruleExec <- unique(ruleExec)

sum(duplicated(ruleExec)) # 중복된 행이 있는지 재확인


ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
ruleMetric <- unique(ruleMetric)
ruleMetric


# pruneRule()적용하지 않은 케이스
freqPattern <- getFreqPattern(ruleMetric, minsup = 0.0020, minconf = 0.6, minlen = 1, maxlen = 8) 
# minsup = 0.0020으로 해보기 (because, df에서 비만인 사람의 비율이 약 20%임)
freqPattern <- unique(freqPattern)
freqPattern <- presentRules(freqPattern, colnames(X))
freqPattern <- data.frame(freqPattern)

pred1_rules <- freqPattern %>% filter(pred == 1) # pred = 1인 Rule들만 추출한 결과 8개의 Rule만 존재함,,,
pred1_rules

pred1_rules <- arrange(pred1_rules, len, condition)
pred1_rules

write.csv(pred1_rules, '0531_misup0.20%,minconf60%_no_prune.csv')


#==================================================================================================#
# pruneRule() 적용한 케이스 ( maxDecay 0.05부터 시작해서 값 0.01씩 줄여보기 )
temp_rule <- pruneRule(ruleMetric, X, target, maxDecay = 0.05, typeDecay = 2) # maxdepth = 8 결과 : 18071 행 존재
temp_rule <- unique(temp_rule) # temp_rule에 다시 unique한 결과 6107개의 행 존재!!!!!!! 위에서 unique()한 거에서 중복된 규칙들을 제대로 다 제거하지 못한 듯!!!

# pruneRule()을 한 결과를 getFreqPattern()함수에 input으로 넣기
freqPattern_prune <- getFreqPattern(temp_rule, minsup = 0.0020, minconf = 0.6, minlen = 1, maxlen = 8) 

freqPattern_prune <- presentRules(freqPattern_prune, colnames(X))
freqPattern_prune <- data.frame(freqPattern_prune)

pred1_rules_prune <- freqPattern_prune %>% filter(pred == 1) # pred = 1인 Rule들만 추출한 결과 8개의 Rule만 존재함,,,
pred1_rules_prune
pred1_rules_prune <- arrange(pred1_rules_prune, len, condition)
pred1_rules_prune

write.csv(pred1_rules_prune, '0531_minsup0.20%,minconf60%_prune.csv')
