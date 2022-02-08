library(dplyr)

df <- read.csv('kyrbs2019_boy.csv')
df <- df %>% select(2:158)

# 모든 변수를 일단 명목형으로 변환
for (i in names(df)){
  if ( i != 'AGE'){
    df[[i]] <- as.factor(df[[i]]) 
  }
}

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

### Feature 카테고리별로 RF 수립 ###

# 주관적 상태
subjective <- c('PR_HT','PR_BI') 
# 현재 분석 대상 df에는 없는 변수임.


# 식생활
eating <- c('F_BR','F_FRUIT','F_SODA','F_CAFFEINE','F_SWDRINK','F_FASTFOOD','F_VEG','F_MILK','F_EDU','F_CS','F_CS_F01','F_CS_F02','F_CS_F03','F_CS_F04','F_CS_F05','F_CS_F06','F_CS_F07','F_CS_F08','F_CS_F09','F_CS_F10','F_CS_F11','F_CS_F12','F_WAT')


# 신체활동
activity <- c('PA_TOT','PA_VIG','PA_MSC','PA_WLK','PA_WLK_MM','SitTime','PA_PE_FQ','PA_ST','PA_TRA','PA_TRA_MM')


# 비만 및 체중 조절
weight <- c('WC_MN','WC_MN_M01','WC_MN_M02','WC_MN_M03','WC_MN_M04','WC_MN_M05','WC_MN_M06','WC_MN_M07','WC_MN_M08','WC_MN_M09','WC_MN_M10')


# 정신 건강
mind <- c('M_STR','M_SLP_EN','M_SAD','M_SUI_CON','M_SUI_PLN','M_SUI_ATT')


# 구강건강
mouth <- c('O_BR_FQ','O_BR_SLP','O_BR_S','O_OCP1','O_OCP2','O_OCP3','O_OCP4','O_OCP5','O_DC_FQ','O_SLNT','O_SCL','PR_OH','O_SYMP1','O_SYMP1_1','O_SYMP2','O_SYMP3','O_SYMP4','O_SYMP5','O_SYMP6','O_EDU')


# 개인위생
hygiene <- c('HW_SPML_S','HW_SPRM_S','HW_SPML_H','HW_SPRM_H','HW_SPGO_H','HW_S_R','HW_EDU')


# 손상예방
prevent <- c('I_SB_FR','I_SB_BK','I_SB_EX','I_SCH_TRT','I_SCH_ED1','I_SCH_ED2','I_SCH_ED3','I_SCH_ED4','I_SCH_ED5')


# 폭력
brutal <- c('V_TRT')


# 음주
drink <- c('AC_LT','AC_FAGE','AC_DAYS','AC_DRUNK','AC_ACCESS')


# 흡연
smoke <- c('TC_LT','TC_DAYS','TC_EC_LT','TC_EC_MN','TC_HTP_LT','TC_HTP_MN','TC_FAGE','TC_DAGE','TC_AMNT','TC_ACCESS','TC_QT_YR','TC_SND_H','TC_SND_S','TC_SND_P','TC_GHW','TC_GHW_H','TC_GHW_QT')


# 성행태
sexual <- c('SEX','S_SI','S_CONT','S_CONT_M','S_EDU')


# 약물
drug <- c('DR_EXP','DR_LT')


# 아토피,천식
atopy <- c('AS_DG_LT','AS_DG_YR','RH_DG_LT','RH_DG_YR','ECZ_DG_LT','ECZ_DG_YR')


# 인터넷중독
internet <- c('INT_WD','INT_WK','INT_WD_MM','INT_WK_MM')


# 일반적 특성
general <- c('AGE','AGE_M','E_S_RCRD','E_SES','E_RES','GRADE','A_FM','E_FM_F_1','E_FM_SF_2','E_FM_M_3','E_FM_SM_4','E_FM_GF_F','E_FM_GM_6','E_FM_OBS_7','E_FM_YBS_8','E_FM_NO_9','E_LT_F','E_LT_SF','E_LT_M','E_LT_SM','E_EDU_F','E_KRN_F','E_BORN_F','E_EDU_M','E_KRN_M','E_BORN_M')

general_X <- df %>% select(general)





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
freqPattern <- getFreqPattern(ruleMetric, minsup = 0.0020, minconf = 0.6, minlen = 1, maxlen = 8) 
# minsup = 0.0020으로 해보기 (because, df에서 비만인 사람의 비율이 약 20%임)
freqPattern <- unique(freqPattern)
freqPattern <- presentRules(freqPattern, colnames(X))
freqPattern <- data.frame(freqPattern)

pred1_rules <- freqPattern %>% filter(pred == 1) # pred = 1인 Rule들만 추출한 결과 8개의 Rule만 존재함,,,
pred1_rules

pred1_rules <- arrange(pred1_rules, len, condition)
pred1_rules

write.csv(pred1_rules, "0511_misup0.20%,minconf60%_no_prune.csv")


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

write.csv(pred1_rules_prune, '0508_minsup0.24%,minconf60%_prune.csv')

