library(dplyr)

df <- read.csv('kyrbs2019_boy.csv')
# csv 파일 불러오면서 생긴 index 열과 비만의 결과로 나타나는 변수는 분석에서 제외
df <- subset(df, select = -c(X,PR_HT))

rule_0.8conf <- read.csv('0421_RF_rules_maxdep_8,minconf_0.8.csv')
rule_0.75conf <- read.csv('0421_RF_rules_maxdep_8,minconf_0.75.csv')

rule_0.8conf <- subset(rule_0.8conf, select = -(X))
rule_0.75conf <- subset(rule_0.75conf, select = -(X))

rule_0.8conf$condition


# 패턴에 해당하는 모든 레코드를 추출해서 중복된 레코드를  제거하는 코드 만들어보는 중,,,
temp_df = data.frame()
for (i in rule_0.8conf$condition){
  temp_df <- rbind(temp_df, df %>% filter(i))
}
# filter()에 input으로 logical vector가 들어가야하는데 i가 현재 chr 형이라서 안 됨

# 위 방법 아직 해결 못해서 수작업으로,,, 
df1 <- df %>% filter(WC_MN %in% c('2'))
df2 <- df %>% filter(AGE>2.5 & WC_MN_M03 %in% c('1'))
df3 <- df %>% filter(PA_MSC %in% c('1','3'))
df4 <- df %>% filter(F_WAT %in% c('1','3','4','5'))
df5 <- df %>% filter(PA_MSC %in% c('1','2'))
df6 <- df %>% filter(PA_MSC %in% c('2') & WC_MN %in% c('1','4'))
df7 <- df %>% filter(O_BR_SLP %in% c('1'))

temp_df = data.frame()
temp_df <- rbind(temp_df,df1)
temp_df <- rbind(temp_df, df2)
temp_df <- rbind(temp_df, df3)
temp_df <- rbind(temp_df, df4)
temp_df <- rbind(temp_df, df5)
temp_df <- rbind(temp_df, df6)
temp_df <- rbind(temp_df, df7)

table(df$BMI)
df_0.8 <- unique(temp_df)      # confidence 0.8인 Rule에 해당하는 레코드들 모은 결과 27602개나 됨... 원본 데이터셋인 df에서의 비만인 사람 수 = 5718명
table(df_0.8$BMI)             # confidence 0.8인 Rule에 해당하는 레코드들을 모은 데이터프레임에서 비만인 사람 수 = 5714명

rules_0.8 <- sort(rule_0.8conf$condition)
rules_0.8
