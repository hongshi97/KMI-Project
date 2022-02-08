# 필요 패키지 설치 및 데이터 불러오기
install.packages("sas7bdat") #SAS파일을 R로 불러올 “sas7bdat” 패키지를 설치
library(sas7bdat)	#설치한 “sas7bdat”패키지를 프로젝트 내부로 불러오기

install.packages("dplyr") #데이터를 필터링 및 변수이름을 재설정할 “dplyr” 패키지를 설치
library(dplyr) #설치한 “dplyr”패키지를 프로젝트 내부로 불러오기

install.packages("mice") #결측치를 대체할 “mice” 패키지를 설치
library(mice) #설치한 “mice”패키지를 프로젝트 내부로 불러오기

install.packages("caret") #훈련데이터와 테스트데이터를 나눌 “caret” 패키지를 설치
library(caret) #설치한 “caret”패키지를 프로젝트 내부로 불러오기

install.packages("rpart") #의사결정나무를 만드는 “rpart” 패키지를 설치
library(rpart) #설치한 “rpart”패키지를 프로젝트 내부로 불러오기

install.packages("rpart.plot") #의사결정나무를 시각화할 “rpart.plot” 패키지를 설치
library(rpart.plot) #설치한 “rpart.plot”패키지를 프로젝트 내부로 불러오기

# 2018년 국민건강영양조사 원시자료 가져오기 및 PHQ-9에 설문한 인원들만 불러오기 
data2018 <- read.sas7bdat(("hn18_all.sas7bdat")) #국민건강영양조사 데이터 불러오기
df <- data2018 #분석용 데이터셋 복사
df <- df %>% filter(mh_PHQ_S %in% c(0:27)) #국민건강영양조사에서 PHQ-9을 실시한 사람만 불러오기
