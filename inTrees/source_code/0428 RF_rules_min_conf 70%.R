library(dplyr)

df <- read.csv('kyrbs2019_boy.csv')

# csv 파일 불러오면서 생긴 index 열과 비만의 결과로 나타나는 변수는 분석에서 제외
df <- subset(df, select = -c(X,PR_HT))
df
rule_0.7conf <- read.csv('0421_RF_rules_maxdep_8,minconf_0.7.csv')

rule_0.7conf
rule_0.7conf <- subset(rule_0.7conf, select = -(X))
rule_0.7conf

rule_0.7conf_ordered <- arrange(rule_0.7conf, len, condition)
rule_0.7conf_ordered                  

write.csv(rule_0.7conf_ordered, '0501_rule_0.7conf.csv')

rule_0.7conf_pred_0.1 <- rule_0.7conf_ordered %>% filter(pred == 1)
rule_0.7conf_pred_0.1

# Family Pattern의 Support 계산
# AGE = 1은 만 12살을 의미, AGE = 7은 만 18살을 의미
sum((df$AGE >= 13.5)&df$BMI ==1 ) / nrow(df)
sum((df$PA_MSC %in% c('1','3'))&df$BMI ==1 ) / nrow(df)
sum((df$WC_MN %in% c('2'))&df$BMI ==1 ) / nrow(df)
sum((df$WC_MN_M03 %in% c('1'))&df$BMI ==1 ) / nrow(df)

sum((df$AGE >= 13.5) & (df$WC_MN %in% c('2')) &(df$BMI ==1)) / nrow(df)
sum((df$AGE >= 13.5) & (df$WC_MN_M03 %in% c('1')) &(df$BMI ==1)) / nrow(df)

# Family Pattern의 Confidence 계산
# AGE = 1은 만 12살을 의미, AGE = 7은 만 18살을 의미
sum((df$AGE >= 13.5)&df$BMI ==1 ) / sum(df$AGE >= 13.5)
sum((df$PA_MSC %in% c('1','3'))&df$BMI ==1 ) / sum(df$PA_MSC %in% c('1','3'))
sum((df$WC_MN %in% c('2'))&df$BMI ==1 ) / sum(df$WC_MN %in% c('2'))
sum((df$WC_MN_M03 %in% c('1'))&df$BMI ==1 ) / sum(df$WC_MN_M03 %in% c('1'))

sum((df$AGE >= 13.5) & (df$WC_MN %in% c('2')) &(df$BMI ==1)) / sum((df$AGE >= 13.5) & (df$WC_MN %in% c('2')))
sum((df$AGE >= 13.5) & (df$WC_MN_M03 %in% c('1')) &(df$BMI ==1)) / sum((df$AGE >= 13.5) & (df$WC_MN_M03 %in% c('1')))
