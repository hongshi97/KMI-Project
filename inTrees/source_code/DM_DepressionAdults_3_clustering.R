# 군집분석을 실시할 수면시간, 근로시간, 우울증을 가져와 결측치를 다중대체법으로 대체
cluster_df <- df %>% select(EC_wht_23, Total_slp, mh_PHQ_S) #근로시간, 수면시간, PHQ-9 점수만 가져와 데이터프레임 생성
cluster_df <- mice(cluster_df, m=5, printFlag = F, seed=1234) #mice패키지로 대체 가능한 결측치 집합 5개 생성
cluster_df <- complete(cluster_df, 2) #2번 집합으로 결측치를 대체

# 우울증을 정상(PHQ-9 10점 미만), 중등도 우울증(PHQ-9 10점이상 15점미만), 우울증 위험군(PHQ-9 15점이상)으로 분류
cluster_df$mh_PHQ_S[cluster_df$mh_PHQ_S < 10] <- 0 #PHQ-9점수가 10점 미만인 사람을 0으로 할당
cluster_df$mh_PHQ_S[(cluster_df$mh_PHQ_S >= 10) & (cluster_df$mh_PHQ_S < 15)] <- 1 #PHQ-9점수가 10점 이상 15점 미만인 사람을 1로 할당
cluster_df$mh_PHQ_S[cluster_df$mh_PHQ_S >= 15] <- 2 #PHQ-9점수가 15점 이상인 사람을 2로 할당

# 근로시간과 수면시간 정규화
x <- cbind(cluster_df$EC_wht_23,cluster_df$Total_slp) #근로시간과 수면시간을 정규화 하기위해 근로시간과 수면시간만 불러오기
for (i in 1:2){
  x[,i] <- (x[,i]-mean(x[,i]))/sd(x[,i])
} #for문을 이용하여 두 변수를 정규화 실시(평균을 빼고 표준편차로 나눈다)

# SSE를 통해 최적의 군집개수 도출
wssplot <- function(data, nc = 15, seed = 1000) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")} #군집 개수별로 SSE를 계산
wssplot(x) #군집 개수별로 SSE값을 그래프로 시각화

# 최적의 군집개수(4개)로 클러스터링 실시
clu_best <- kmeans(x,4) #4개의 군집이 최적으로 해석되어 KMEANS알고리즘을 이용하여 군집분석
table(clu_best$cluster) #클러스터별 할당된 인원수 파악
clu_best$centers #클러스터들의 중심점 파악

# Cluster시각화 및 개수확인
qplot(x[,1],x[,2], color = clu_best$cluster , data = cluster_df) #나눠진 군집을 시각화
table(cluster_df$mh_PHQ_S, clu_best$cluster) #군집내 PHQ-9점수에 따라 인원 분류

# Cluster가 나뉘어진 군집별 행번호 분류
index1 <- clu_best$cluster == 1 #1번 군집에 해당하는 사람들의 행번호를 지정
index2 <- clu_best$cluster == 2 #2번 군집에 해당하는 사람들의 행번호를 지정
index3 <- clu_best$cluster == 3 #3번 군집에 해당하는 사람들의 행번호를 지정
index4 <- clu_best$cluster == 4 #4번 군집에 해당하는 사람들의 행번호를 지정

# 해당하는 행번호의 data를 가져오기
df_c1 <- cluster_df[index1,] #1번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장
df_c2 <- cluster_df[index2,] #2번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장
df_c3 <- cluster_df[index3,] #3번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장
df_c4 <- cluster_df[index4,] #4번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장

# boxplot으로 군집별 수면시간 비교 및 시각화
par(mfrow=(c(1,4))) #4가지 그래프를 한 번에 보기위해 PLOT 칸을 4분할
boxplot(df_c1$Total_slp, main = "고근로 수면부족군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #1번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c2$Total_slp, main = "고근로 수면과잉군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #2번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c3$Total_slp, main = "저근로 수면과잉군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #3번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c4$Total_slp, main = "저근로 수면부족군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #4번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인

# boxplot으로 군집별 근로시간 비교 및 시각화
boxplot(df_c1$EC_wht_23, main = "고근로 수면부족군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #1번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c2$EC_wht_23, main = "고근로 수면과잉군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #2번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c3$EC_wht_23, main = "저근로 수면과잉군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #3번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c4$EC_wht_23, main = "저근로 수면부족군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #4번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인

# 각 군집의 근로시간과 수면시간의 평균 계산
a1 <- mean(df_c1$EC_wht_23) #1번 군집의 근로시간 평균값을 저장
b1 <-mean(df_c1$Total_slp) #1번 군집의 수면시간 평균값을 저장
a2 <-mean(df_c2$EC_wht_23) #2번 군집의 근로시간 평균값을 저장
b2<- mean(df_c2$Total_slp) #2번 군집의 수면시간 평균값을 저장
a3<-mean(df_c3$EC_wht_23) #3번 군집의 근로시간 평균값을 저장
b3<-mean(df_c3$Total_slp) #3번 군집의 수면시간 평균값을 저장
a4<-mean(df_c4$EC_wht_23) #4번 군집의 근로시간 평균값을 저장
b4<-mean(df_c4$Total_slp) #4번 군집의 수면시간 평균값을 저장

# 각 군집별 근로시간 및 노동시간의 평균 수치화
mean_work_time <-c(round(c(a1, a2,a3,a4),2)) #각 군집의 근로시간 평균을 둘째자리까지 나타내어 값을 저장
mean_work_time #값이 잘 들어갔는지 확인
mean_slp_time <-c(round(c(b1, b2, b3, b4),2)) #각 군집의 수면시간 평균을 둘째자리까지 나타내어 값을 저장
mean_slp_time #값이 잘 들어갔는지 확인
mean_df <- data.frame(mean_work_time, mean_slp_time) #위에서 추출한 벡터값을 합쳐 데이터프레임 형성
mean_df #데이터프레임이 잘 만들어졌는지 확인

# 각 군집별 근로시간 및 노동시간의 평균 시각화
par(mfrow=c(1,1)) #위에서 4분할 되었던 PLOT을 다시 합치기
bar.text <- barplot(as.matrix(t(mean_df)), main = "군집별 근로시간과 수면시간", legend= c("근로시간 (시간)","수면시간 (분)"), beside=T,names = c("고근로 수면부족군", "고근로 수면과잉군", "저근로 수면과잉군", "저근로 수면부족군"),col=rainbow(2), args.legend=list(x='topright'), ylim = c(0,600))   #수면시간과 근로시간의 평균을 그래프로 한 번에 시각화
text(bar.text, as.matrix(t(mean_df))+ 38, labels = as.matrix(t(mean_df)), pos= 1.5) #시각화된 그래프에 평균값 라벨 작성

# 각 군집별 수면시간의 표준편차 계산
sd(df_c1$Total_slp) #1번 군집의 수면시간에 대한 표준편차 계산
sd(df_c2$Total_slp) #2번 군집의 수면시간에 대한 표준편차 계산
sd(df_c3$Total_slp) #3번 군집의 수면시간에 대한 표준편차 계산
sd(df_c4$Total_slp) #4번 군집의 수면시간에 대한 표준편차 계산

# 각 군집별 근로시간의 표준편차 계산
sd(df_c1$EC_wht_23) #1번 군집의 근로시간에 대한 표준편차 계산
sd(df_c2$EC_wht_23) #2번 군집의 근로시간에 대한 표준편차 계산
sd(df_c3$EC_wht_23) #3번 군집의 근로시간에 대한 표준편차 계산
sd(df_c4$EC_wht_23) #4번 군집의 근로시간에 대한 표준편차 계산
