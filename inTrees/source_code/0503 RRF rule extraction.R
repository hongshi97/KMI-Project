library(dplyr)

df <- read.csv('kyrbs2019_boy.csv')
head(df)
df <- df %>% select(2:158)
head(df)

# df 변수명 파악
names(df)

# 청건행 데이터 순서형 변수 파악 워드 파일의 각 변수 table()로 9999 값 있는지 확인
table(df$F_BR)
table(df$F_FRUIT)
table(df$F_SODA)
table(df$F_CAFFEINE)
table(df$F_SWDRINK)
table(df$F_FASTFOOD)
table(df$F_VEG)
table(df$F_MILK)
table(df$F_CS)
table(df$F_WAT)
table(df$PA_TOT)
table(df$PA_VIG)
table(df$PA_MSC)
table(df$PA_WLK)
table(df$PA_PE_FQ)
table(df$PA_ST)
table(df$PA_TRA)
table(df$M_STR)
table(df$M_SLP_EN)
table(df$O_BR_FQ)
table(df$O_BR_S)
table(df$O_DC_FQ)
table(df$PR_OH)
table(df$HW_SPML_S)
table(df$HW_SPRM_S)
table(df$HW_SPML_H)
table(df$HW_SPRM_H)
table(df$HW_SPGO_H)
table(df$I_SB_FR)
table(df$I_SB_BK)
table(df$I_SB_EX)
table(df$V_TRT)
table(df$TC_SND_H)
table(df$TC_SND_S)
table(df$TC_SND_P)
table(df$E_S_RCRD)
table(df$E_SES)
table(df$GRADE)
table(df$SleepAvg_WK)
table(df$PA_SWD_S)
table(df$PA_SWD_N)
table(df$PA_SWK_S)
table(df$PA_SWK_N)
table(df$PA_WLK_MM)
table(df$PA_TRA_MM)
table(df$INT_WD_MM)
table(df$INT_WK_MM)
# ==> 9999(비해당)이 있는 변수가 하나도 없네,,,? 주피터에서 데이터 전처리 과정에서 혹시 9999가 들어간 변수들 버렸는지 확인하기
# ==> 주피터 데이터 전처리에서는 이상 없음
table(df$E_LT_SM)

# 모든 변수를 일단 명목형으로 변환
for (i in names(df)){
  if ( i != 'AGE'){
    df[[i]] <- as.factor(df[[i]]) 
  }
}
str(df)
table(df$AGE)
typeof(df$AGE)

# 순서형 변수인 변수들 factor(ordered) 이용해서 전처리
df$F_BR <- ordered(df$F_BR, levels=c(1:8))
df$F_BR

df$F_FRUIT <- ordered(df$F_FRUIT, levels = c(1:7))
df$F_FRUIT

df$F_SODA <- ordered(df$F_SODA, levels = c(1:7))
df$F_SODA

df$F_CAFFEINE <- ordered(df$F_CAFFEINE, levels = c(1:7))
df$F_CAFFEINE

df$F_SWDRINK <- ordered(df$F_CAFFEINE, levels = c(1:7))
df$F_SWDRINK

df$F_FASTFOOD <- ordered(df$F_FASTFOOD, levels = c(1:7))
df$F_FASTFOOD

df$F_VEG <- ordered(df$F_VEG, levels = c(1:7))
df$F_VEG

df$F_MILK <- ordered(df$F_MILK, levels = c(1:7))
df$F_MILK

df$F_CS <- ordered(df$F_CS, levels = c(1:7))
df$F_CS

df$F_WAT <- ordered(df$F_WAT, levels = c(1:5))
df$F_WAT

df$PA_TOT <- ordered(df$PA_TOT, levels = c(1:8))
df$PA_TOT

df$PA_VIG <- ordered(df$PA_VIG, levels = c(1:6))
df$PA_VIG

df$PA_MSC <- ordered(df$PA_MSC, levels = c(1:6))
df$PA_MSC

df$PA_WLK <- ordered(df$PA_WLK, levels = c(1:8))
df$PA_WLK

df$PA_PE_FQ <- ordered(df$PA_PE_FQ, levels = c(1:4))
df$PA_PE_FQ

df$PA_ST <- ordered(df$PA_ST, levels = c(1:5))
df$PA_ST

df$PA_TRA <- ordered(df$PA_TRA, levels = c(1:8))
df$PA_TRA

df$M_STR <- ordered(df$M_STR, levels = c(1:5))
df$M_STR

df$M_SLP_EN <- ordered(df$M_SLP_EN, levels = c(1:5))
df$M_SLP_EN

df$O_BR_FQ <- ordered(df$O_BR_FQ, levels = c(1:10))
df$O_BR_FQ

df$O_BR_S <- ordered(df$O_BR_S, levels = c(1:4))
df$O_BR_S

df$O_DC_FQ <- ordered(df$O_DC_FQ, levels = c(1:5))
df$O_DC_FQ

df$PR_OH <- ordered(df$PR_OH, levels = c(1:5))
df$PR_OH

df$HW_SPML_S <- ordered(df$HW_SPML_S, levels = c(1:4))
df$HW_SPML_S

df$HW_SPRM_S <- ordered(df$HW_SPRM_S, levels = c(1:4))
df$HW_SPRM_S

df$HW_SPML_H <- ordered(df$HW_SPML_H, levels = c(1:4))
df$HW_SPML_H

df$HW_SPRM_H <- ordered(df$HW_SPRM_H, levels = c(1:4))
df$HW_SPRM_H

df$HW_SPGO_H <- ordered(df$HW_SPGO_H, levels = c(1:4))
df$HW_SPGO_H

df$I_SB_FR <- ordered(df$I_SB_FR, levels = c(1:5))
df$I_SB_FR

df$I_SB_BK <- ordered(df$I_SB_BK, levels = c(1:5))
df$I_SB_BK

df$I_SB_EX <- ordered(df$I_SB_EX, levels = c(1:5))
df$I_SB_EX

df$V_TRT <- ordered(df$V_TRT, levels = c(1:7))
df$V_TRT

df$TC_SND_H <- ordered(df$TC_SND_H, levels = c(1:8))
df$TC_SND_H

df$TC_SND_S <- ordered(df$TC_SND_S, levels = c(1:8))
df$TC_SND_S

df$TC_SND_P <- ordered(df$TC_SND_P, levels = c(1:8))
df$TC_SND_P

df$E_S_RCRD <- ordered(df$E_S_RCRD, levels = c(1:5))
df$E_S_RCRD

df$E_SES <- ordered(df$E_SES, levels = c(1:5))
df$E_SES

df$GRADE <- ordered(df$GRADE, levels = c(1:6))
df$GRADE

table(df$SleepAvg_WK)
df$SleepAvg_WK <- ordered(df$SleepAvg_WK, levels = c(0:20))
df$SleepAvg_WK

table(df$PA_SWD_S)
df$PA_SWD_S <- ordered(df$PA_SWD_S, levels = c(0:14))
df$PA_SWD_S

table(df$PA_SWD_N)
df$PA_SWD_N <- ordered(df$PA_SWD_N, levels = c(0:7))
df$PA_SWD_N

table(df$PA_SWK_S)
df$PA_SWK_S <- ordered(df$PA_SWK_S, levels = c(0:11))
df$PA_SWK_S

table(df$PA_SWK_N)
df$PA_SWK_N <- ordered(df$PA_SWK_N, levels = c(0:12))
df$PA_SWK_N

df$PA_WLK_MM <- ordered(df$PA_WLK_MM, levels = c(0:4))
df$PA_WLK_MM

df$PA_TRA_MM <- ordered(df$PA_TRA_MM, levels = c(0:3))
df$PA_TRA_MM

table(df$INT_WD_MM)
df$INT_WD_MM <- ordered(df$INT_WD_MM, levels = c(0:5))
df$INT_WD_MM

table(df$INT_WK_MM)
df$INT_WK_MM <- ordered(df$INT_WK_MM, levels = c(0:10))
df$INT_WK_MM

table(df$O_SYMP1_1)
df$O_SYMP1_1 <- ordered(df$O_SYMP1_1, levels = c(0:2))
df$O_SYMP1_1

table(df$AC_FAGE)
df$AC_FAGE <- ordered(df$AC_FAGE, levels = c(1:14))
df$AC_FAGE

table(df$AC_DAYS)
df$AC_DAYS <- ordered(df$AC_DAYS, levels = c(0:7))
df$AC_DAYS

table(df$AC_AMNT)
df$AC_AMNT <- ordered(df$AC_AMNT, levels = c(0:5))
df$AC_AMNT

table(df$AC_DRUNK)
df$AC_DRUNK <- ordered(df$AC_DRUNK, levels = c(0:4))
df$AC_DRUNK

table(df$TC_DAYS)
df$TC_DAYS <- ordered(df$TC_DAYS, levels = c(0:7))
df$TC_DAYS

table(df$TC_EC_MN)
df$TC_EC_MN <- ordered(df$TC_EC_MN, levels = c(0:7))
df$TC_EC_MN

table(df$TC_HTP_MN)
df$TC_HTP_MN <- ordered(df$TC_HTP_MN, levels = c(0:7))
df$TC_HTP_MN

table(df$TC_FAGE)
df$TC_FAGE <- ordered(df$TC_FAGE, levels = c(1:14))
df$TC_FAGE

table(df$TC_AMNT)
df$TC_AMNT <- ordered(df$TC_AMNT, levels = c(0:6))
df$TC_AMNT

table(df$DR_LT)
df$DR_LT <- ordered(df$DR_LT, levels = c(0:3))
df$DR_LT

table(df$AS_DG_YR)
df$AS_DG_YR <- ordered(df$AS_DG_YR, levels = c(0:2))
df$AS_DG_YR

# PR_HT는 비만의 결과로 드러나는 변수라 판단하여 제거
df <- subset(df, select = -(PR_HT))

str(df)

################# 각 변수별 척도에 맞게 변수 타입 설정 끝 ##################

# 랜덤 포레스트 모델링
library(inTrees)
library(RRF)

X <- df[,1:(ncol(df)-1)]
target <- df[,"BMI"]

rf <- RRF(X,as.factor(target)) # build an ordinary RF

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
freqPattern <- getFreqPattern(ruleMetric, minsup = 0.0024, minconf = 0.7, minlen = 1, maxlen = 8) 
freqPattern <- unique(freqPattern)
freqPattern <- presentRules(freqPattern, colnames(X))
freqPattern <- data.frame(freqPattern)

pred1_rules <- freqPattern %>% filter(pred == 1) # pred = 1인 Rule들만 추출한 결과 8개의 Rule만 존재함,,,
pred1_rules


#==================================================================================================#
#==================================================================================================#

# pruneRule() 적용한 케이스 ( maxDecay 0.05부터 시작해서 값 0.01씩 줄여보기 )
temp_rule <- pruneRule(ruleMetric, X, target, maxDecay = 0.05, typeDecay = 2) # maxdepth = 8 결과 : 18071 행 존재
temp_rule <- unique(temp_rule) # temp_rule에 다시 unique한 결과 6107개의 행 존재!!!!!!! 위에서 unique()한 거에서 중복된 규칙들을 제대로 다 제거하지 못한 듯!!!

# pruneRule()을 한 결과를 getFreqPattern()함수에 input으로 넣기
freqPattern <- getFreqPattern(temp_rule, minsup = 0.0024, minconf = 0.7, minlen = 1, maxlen = 8) 

# max_depth =8, minconf = 0.8로 설정한 결과 24개의 Rule만 추출됨

freqPattern <- presentRules(freqPattern, colnames(X))
freqPattern

write.csv(freqPattern, '0503_RF_rules_maxdep_8,minconf_0.7.csv')

