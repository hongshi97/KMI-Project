library(dplyr)

df <- read.csv('kyrbs2019_boy.csv')

# csv 파일 불러오면서 생긴 index 열과 비만의 결과로 나타나는 변수는 분석에서 제외
df <- subset(df, select = -c(X,PR_HT))
df
attach(df)

rules <- read.csv('0508 no_prune_family_pattern.csv')
rules

# 수작업 support, confidence 계산
# Family Pattern의 Support 계산
# X -> Y 에서 support 계산식: X와 Y를 모두 포함하는 거래 수 /전체 거래수
sum(E_EDU_F %in% c('1','2') & BMI == '1') / nrow(df)
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & BMI == '1') / nrow(df)
sum(E_EDU_F %in% c('1','2') & O_BR_S %in% c('2','3','4') & BMI == '1') / nrow(df)
sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & BMI == '1') / nrow(df)
sum(E_EDU_F %in% c('1','2','4') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2') & BMI == '1') / nrow(df)
sum(INT_WK_MM %in% c('0','2','3','4','5','6','7','8','9','10') & F_WAT %in% c('3','4','5') & BMI == '1') / nrow(df)
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & BMI == '1') / nrow(df)
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & O_BR_S %in% c('2','3','4') & BMI == '1') / nrow(df)
sum(E_EDU_F %in% c('1','2','4') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(E_EDU_F %in% c('1','2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4') & BMI == '1') / nrow(df)
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2','4') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & BMI == '1') / nrow(df)
sum(GRADE %in% c('1','3','4','5','6') & PA_MSC %in% c('1','3') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(INT_WK_MM %in% c('0','2','3','4','5','6','7','8','9','10') & F_WAT %in% c('3','4','5') & WC_MN %in% c('2') & BMI == '1') / nrow(df)
sum(PA_MSC %in% c('1','3') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4') & BMI == '1') / nrow(df)
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4') & BMI == '1') / nrow(df)
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2','4') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(GRADE %in% c('1','3','4','5','6') & F_WAT %in% c('1','3','4','5') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)
sum(GRADE %in% c('1','3','4','5','6') & PA_MSC %in% c('1','3') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / nrow(df)



# Family Pattern의 Confidence 계산
# X -> Y 에서 Confidence 계산식: X와 Y를 모두 포함하는 거래 수 / X가 포함된 거래수 
sum(E_EDU_F %in% c('1','2') & BMI == '1') / sum(E_EDU_F %in% c('1','2'))
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & BMI == '1') / sum(AGE>13.5 & E_EDU_F %in% c('1','2'))
sum(E_EDU_F %in% c('1','2') & O_BR_S %in% c('2','3','4') & BMI == '1') / sum(E_EDU_F %in% c('1','2') & O_BR_S %in% c('2','3','4'))
sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & BMI == '1') / sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2'))
sum(E_EDU_F %in% c('1','2','4') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(E_EDU_F %in% c('1','2','4') & WC_MN_M03 %in% c('1'))
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2') & BMI == '1') / sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2'))
sum(INT_WK_MM %in% c('0','2','3','4','5','6','7','8','9','10') & F_WAT %in% c('3','4','5') & BMI == '1') / sum(INT_WK_MM %in% c('0','2','3','4','5','6','7','8','9','10') & F_WAT %in% c('3','4','5'))
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & BMI == '1') / sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN %in% c('2'))
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN_M03 %in% c('1'))
sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & O_BR_S %in% c('2','3','4') & BMI == '1') / sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & O_BR_S %in% c('2','3','4'))
sum(E_EDU_F %in% c('1','2','4') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(E_EDU_F %in% c('1','2','4') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1'))
sum(E_EDU_F %in% c('1','2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4') & BMI == '1') / sum(E_EDU_F %in% c('1','2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4'))
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2','4') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2','4') & WC_MN_M03 %in% c('1') )
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & BMI == '1') / sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2') & WC_MN %in% c('2'))
sum(GRADE %in% c('1','3','4','5','6') & PA_MSC %in% c('1','3') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(GRADE %in% c('1','3','4','5','6') & PA_MSC %in% c('1','3') & WC_MN_M03 %in% c('1'))
sum(INT_WK_MM %in% c('0','2','3','4','5','6','7','8','9','10') & F_WAT %in% c('3','4','5') & WC_MN %in% c('2') & BMI == '1') / sum(INT_WK_MM %in% c('0','2','3','4','5','6','7','8','9','10') & F_WAT %in% c('3','4','5') & WC_MN %in% c('2'))
sum(PA_MSC %in% c('1','3') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(PA_MSC %in% c('1','3') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1'))
sum(WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4') & BMI == '1') / sum(WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4'))
sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(AGE>13.5 & E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1'))
sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4') & BMI == '1') / sum(E_EDU_F %in% c('1','2') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & O_BR_S %in% c('2','3','4'))
sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2','4') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(GRADE %in% c('1','3','4','5','6') & E_EDU_F %in% c('1','2','4') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1'))
sum(GRADE %in% c('1','3','4','5','6') & F_WAT %in% c('1','3','4','5') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(GRADE %in% c('1','3','4','5','6') & F_WAT %in% c('1','3','4','5') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') )
sum(GRADE %in% c('1','3','4','5','6') & PA_MSC %in% c('1','3') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1') & BMI == '1') / sum(GRADE %in% c('1','3','4','5','6') & PA_MSC %in% c('1','3') & WC_MN %in% c('2') & WC_MN_M03 %in% c('1'))

detach(df)
