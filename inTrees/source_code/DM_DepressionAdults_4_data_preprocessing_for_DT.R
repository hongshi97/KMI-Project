# 관심군인 근로시간이 적은 군집 추출
indexcluster <- clu_best$cluster %in% c(3,4) #관심있는 군집 3, 4번(저근로군)을 의사결정나무를 통해 분류분석을 하기 위해 불러오기
df_Cluster <- df[indexcluster,] #Attribute 중 ID가 의사결정나무의 품질을 떨어뜨리기에 제외
dim(df_Cluster) #불러온 데이터가 잘 불러져 왔는지 (행X열)을 통해 확인
table(df_Cluster$mh_PHQ_S) #불러온 데이터 내 PHQ점수의 빈도 확인

# 관심있는 변수들에 대한 무응답을 NA처리
df_Cluster$HE_fh[df_Cluster$HE_fh == 9] <- NA #모름에 해당하는 값을 NA로 대체
df_Cluster$BE3_81[df_Cluster$BE3_81 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$EC_wht_0[df_Cluster$EC_wht_0 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$BD2_1[df_Cluster$BD2_1 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$LQ4_00[df_Cluster$LQ4_00 == 9] <- NA #모름, 무응답에 해당하는 값을 NA로 대체
df_Cluster$BO1_1[df_Cluster$BO1_1 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$BE8_1[df_Cluster$BE8_1 %in% c(88,99)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$DF2_dg[df_Cluster$DF2_dg == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$DF2_pr[df_Cluster$DF2_pr %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HPfh1[df_Cluster$HE_HPfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HPfh2[df_Cluster$HE_HPfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HPfh3[df_Cluster$HE_HPfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HLfh1[df_Cluster$HE_HLfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HLfh2[df_Cluster$HE_HLfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HLfh3[df_Cluster$HE_HLfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_IHDfh1[df_Cluster$HE_IHDfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_IHDfh2[df_Cluster$HE_IHDfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_IHDfh3[df_Cluster$HE_IHDfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_STRfh1[df_Cluster$HE_STRfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_STRfh2[df_Cluster$HE_STRfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_STRfh3[df_Cluster$HE_STRfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_DMfh1[df_Cluster$HE_DMfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_DMfh2[df_Cluster$HE_DMfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_DMfh3[df_Cluster$HE_DMfh3 %in% c(8,9)] #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_THfh1[df_Cluster$HE_THfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_THfh2[df_Cluster$HE_THfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_THfh3[df_Cluster$HE_THfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HBfh1[df_Cluster$HE_HBfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HBfh2[df_Cluster$HE_HBfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HBfh3[df_Cluster$HE_HBfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$allownc[df_Cluster$allownc == 99] <- NA #모름/무응답으로 설문한 사람들은 NA로 처리
df_Cluster$LF_SAFE[df_Cluster$LF_SAFE == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$EC1_1[df_Cluster$EC1_1 %in% c(8,9)] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$M_2_yr[df_Cluster$M_2_yr %in% c(3,9)] <- NA #의료서비스가 필요 없었던 사람과 모름/무응답으로 설문한 사람을 NA로 처리
df_Cluster$npins[df_Cluster$npins == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$EC_wht_5[df_Cluster$EC_wht_5 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_91[df_Cluster$BE3_91 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_75[df_Cluster$BE3_75 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_85[df_Cluster$BE3_85 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_71[df_Cluster$BE3_71 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$MH1_yr[df_Cluster$MH1_yr == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$MH1_1[df_Cluster$MH1_1 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$MO1_1[df_Cluster$MO1_1 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BD2_31[df_Cluster$BD2_31 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리

# NA 대체를 위해 MICE 사용
df_Cluster <- df_Cluster %>% select(DF2_dg, DF2_pr, HE_HPfh1, HE_HPfh2, HE_HPfh3, HE_HLfh1, HE_HLfh2, HE_HLfh3, HE_IHDfh1, HE_IHDfh2, HE_IHDfh3, HE_STRfh1, HE_STRfh2, HE_STRfh3, HE_DMfh1, HE_DMfh2, HE_DMfh3, HE_THfh1, HE_THfh2, HE_THfh3, HE_HBfh1, HE_HBfh2, HE_HBfh3, allownc, LF_SAFE, EC1_1, HE_obe, HE_BMI, M_2_yr, npins, EC_wht_5, BE3_91, BE3_75, BE3_85, BE3_71, MH1_yr, MH1_1, MO1_1, BD2_31, edu, age, HE_fh, BE3_81, EC_wht_0, BD2_1, LQ4_00, BO1_1, BE8_1, ainc, mh_PHQ_S) #유의미한 변수 추출
df_Cluster <- mice(df_Cluster, m=5, printFlag = F, seed=1000) #mice함수를 이용하여 뽑은 변수들에 대해 결측치를 다중대체법으로 대체한다. 결측치 집합을 5개를 만들고 이를 저장하기 위해 시드값을 표기하였다.
df_Cluster <- complete(df_Cluster, 2) #5개의 집합 중 2번 집합을 가져와 결측치를 대체한다.
table(is.na(df_Cluster)) #결측치가 잘 대체되었는지 확인

# MICE 후 범주화 등의 전처리 및 factor화
# 우울증 의사진단 여부 0: 없음 // 1. 있음
df_Cluster$DF2_dg <- factor(df_Cluster$DF2_dg, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$DF2_dg) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$DF2_dg)) #해당 변수에 결측치가 다 제거되었는지 확인

# 우울증 현재 유병 여부 0:없음, 1:있음
df_Cluster$DF2_pr <- factor(df_Cluster$DF2_pr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$DF2_pr) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$DF2_pr)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고혈압 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_HPfh1 <- factor(df_Cluster$HE_HPfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HPfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HPfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고혈압 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_HPfh2 <- factor(df_Cluster$HE_HPfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HPfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HPfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고혈압 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_HPfh3 <- factor(df_Cluster$HE_HPfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HPfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HPfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고지혈증 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_HLfh1 <- factor(df_Cluster$HE_HLfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HLfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HLfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고지혈증 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_HLfh2 <- factor(df_Cluster$HE_HLfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HLfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HLfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고지혈증 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_HLfh3 <- factor(df_Cluster$HE_HLfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HLfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HLfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 허혈성심장질환 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_IHDfh1 <- factor(df_Cluster$HE_IHDfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_IHDfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_IHDfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 허혈성심장질환 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_IHDfh2 <- factor(df_Cluster$HE_IHDfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_IHDfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_IHDfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 허혈성심장질환 의사진단 여부(형제자매) 0:아니오 // 1:예 
df_Cluster$HE_IHDfh3 <- factor(df_Cluster$HE_IHDfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_IHDfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_IHDfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 뇌졸중 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_STRfh1 <- factor(df_Cluster$HE_STRfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_STRfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_STRfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 뇌졸중 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_STRfh2 <- factor(df_Cluster$HE_STRfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_STRfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_STRfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 뇌졸중 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_STRfh3 <- factor(df_Cluster$HE_STRfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_STRfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_STRfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 당뇨병 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_DMfh1 <- factor(df_Cluster$HE_DMfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_DMfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_DMfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 당뇨병 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_DMfh2 <- factor(df_Cluster$HE_DMfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_DMfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_DMfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 당뇨병 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_DMfh3 <- factor(df_Cluster$HE_DMfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_DMfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_DMfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 갑상선질환 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_THfh1 <- factor(df_Cluster$HE_THfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_THfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_THfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 갑상선질환 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_THfh2 <- factor(df_Cluster$HE_THfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_THfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_THfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 갑상선질환 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_THfh3 <- factor(df_Cluster$HE_THfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_THfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_THfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# B형간염 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_HBfh1 <- factor(df_Cluster$HE_HBfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HBfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HBfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# B형간염 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_HBfh2 <- factor(df_Cluster$HE_HBfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HBfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HBfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# B형간염 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_HBfh3 <- factor(df_Cluster$HE_HBfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HBfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HBfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 기초 생활 수급 여부 0:미수급 // 1:수급
df_Cluster$allownc[df_Cluster$allownc == 10] <- 1 #기초생활수급을 한 경험이 있는 사람은 1로 분류
df_Cluster$allownc[df_Cluster$allownc == 20] <- 0 #기초생활수급을 한 경험이 없는 사람은 0으로 분류
df_Cluster$allownc <- factor(df_Cluster$allownc, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$allownc) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$allownc)) #해당 변수에 결측치가 다 제거되었는지 확인

# 식생활 형편이 좋은 사람:0 // 안 좋은 사람:1
df_Cluster$LF_SAFE[df_Cluster$LF_SAFE %in% c(1,2)] <- 0 #식생활형편이 좋은 사람을 0으로 분류
df_Cluster$LF_SAFE[df_Cluster$LF_SAFE %in% c(3,4)] <- 1 #식생활형편이 좋지 않은 사람을 0으로 분류
df_Cluster$LF_SAFE <- factor(df_Cluster$LF_SAFE, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$LF_SAFE) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$LF_SAFE)) #해당 변수에 결측치가 다 제거되었는지 확인

# 경제활동상태 여부 0: 아니오 // 1: 예 
df_Cluster$EC1_1[df_Cluster$EC1_1 == 2] <- 0 #경제활동을 하지 않는 사람을 0으로 분류
df_Cluster$EC1_1 <- factor(df_Cluster$EC1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$EC1_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$EC1_1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 정상체중여부 0:정상 // 1:체중변화있다  
df_Cluster$HE_obe[df_Cluster$HE_obe %in% c(1,3,4,5,6)] <- 1 #저체중과 비만인 사람을 1로 분류
df_Cluster$HE_obe[df_Cluster$HE_obe == 2] <- 0 #정상체중인 사람은 0으로 분류
df_Cluster$HE_obe <- factor(df_Cluster$HE_obe, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_obe) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_obe)) #해당 변수에 결측치가 다 제거되었는지 확인

# BMI 지수 0:저체중 // 1: 정상 // 2: 비만
df_Cluster$HE_BMI[df_Cluster$HE_BMI < 18.5] <- 0 #BMI가 18.5미만으로 저체중인 사람을 0으로 분류
df_Cluster$HE_BMI[df_Cluster$HE_BMI >= 18.5 & df_Cluster$HE_BMI <23] <- 1 #BMI가 18.5이상 23미만으로 정상체중인 사람을 1로 분류
df_Cluster$HE_BMI[df_Cluster$HE_BMI >= 23] <- 2 #BMI가 23이상으로 비만인사람을 2로 분류
df_Cluster$HE_BMI <- factor(df_Cluster$HE_BMI, levels = c(0,1,2), ordered = TRUE) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_BMI) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_BMI)) #해당 변수에 결측치가 다 제거되었는지 확인

# 필요 의료서비스 미충족 여부 0:충족 // 1:미충족 
df_Cluster$M_2_yr[df_Cluster$M_2_yr == 2] <- 0 #의료서비스를 충족한 사람을 0으로 분류
df_Cluster$M_2_yr <- factor(df_Cluster$M_2_yr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$M_2_yr) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$M_2_yr)) #해당 변수에 결측치가 다 제거되었는지 확인

# 민간의료보험가입여부 0:아니오 // 1:예
df_Cluster$npins[df_Cluster$npins == 2] <- 0 #민간의료보험에 가입하지 않은 사람을 0으로 분류
df_Cluster$npins <- factor(df_Cluster$npins, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$npins) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$npins)) #해당 변수에 결측치가 다 제거되었는지 확인

# 변형근로시간 1:주간근무 // 2:저녁근무 // 3:밤 근무 // 4:주야간 규칙적 교대근무 // 5:24시간 교대근무 // 6:분할근무 // 7:불규칙 교대근무 // 8:기타 
df_Cluster$EC_wht_5 <- factor(df_Cluster$EC_wht_5, levels = c(1,2,3,4,5,6,7,8)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$EC_wht_5) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$EC_wht_5)) #해당 변수에 결측치가 다 제거되었는지 확인

# 장소이동 신체활동 여부 0:아니오 // 1:예
df_Cluster$BE3_91[df_Cluster$BE3_91 == 2] <- 0 #장소이동 신체활동 여부가 없는 사람을 0으로 분류
df_Cluster$BE3_91 <- factor(df_Cluster$BE3_91, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_91) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_91)) #해당 변수에 결측치가 다 제거되었는지 확인

# 여가 고강도 신체활동 여부 0:아니오 // 1:예
df_Cluster$BE3_75[df_Cluster$BE3_75 == 2] <- 0 #여가 고강도 신체활동 여부가 없는 사람을 0으로 분류
df_Cluster$BE3_75 <- factor(df_Cluster$BE3_75, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_75) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_75)) #해당 변수에 결측치가 다 제거되었는지 확인

# 여가 중강도 신체활동 여부 0:아니오 // 1:예
df_Cluster$BE3_85[df_Cluster$BE3_85 == 2] <- 0 #여가 중강도 신체활동 여부가 없는 사람을 0으로 분류
df_Cluster$BE3_85 <- factor(df_Cluster$BE3_85, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_85) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_85)) #해당 변수에 결측치가 다 제거되었는지 확인

# 일 고강도 신체활동 여부 0: 아니오 // 1: 예 
df_Cluster$BE3_71[df_Cluster$BE3_71 == 2] <- 0 #하루에 고강도의 신체활동을 안 하는 사람을 0으로 분류
df_Cluster$BE3_71 <- factor(df_Cluster$BE3_71, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_71) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_71)) #해당 변수에 결측치가 다 제거되었는지 확인

# 1년간 입원이용 여부 0:없음 // 1:있음
df_Cluster$MH1_yr[df_Cluster$MH1_yr == 2] <- 0 #1년간 입원을 하지 않은 사람을 0으로 분류
df_Cluster$MH1_yr <- factor(df_Cluster$MH1_yr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$MH1_yr) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$MH1_yr)) #해당 변수에 결측치가 다 제거되었는지 확인

# 입원이용 횟수 0:5회미만 // 1:5회이상
df_Cluster$MH1_1[df_Cluster$MH1_1 < 5] <- 0 #입원이용횟수가 5회미만인 사람을 0으로 분류
df_Cluster$MH1_1[df_Cluster$MH1_1 >= 5] <- 1 #입원이용 횟수가 5회이상인 사람을 1로 분류
df_Cluster$MH1_1 <- factor(df_Cluster$MH1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$MH1_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$MH1_1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 외래이용 횟수 0:5회미만 // 1:5회이상
df_Cluster$MO1_1[df_Cluster$MO1_1 < 5] <- 0 #외래이용횟수가 5회미만인 사람을 0으로 분류
df_Cluster$MO1_1[df_Cluster$MO1_1 >= 5] <- 1 #외래이용횟수가 5회이상인 사람을 1로 분류
df_Cluster$MO1_1 <- factor(df_Cluster$MO1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$MO1_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$MO1_1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 폭음 빈도 0:월 1회미만 // 1:월 1회 // 2:주 1회 // 3:거의매일
df_Cluster$BD2_31[df_Cluster$BD2_31 %in% c(1,2)] <- 0 #폭음빈도가 월 1회미만인 사람을 0으로 분류
df_Cluster$BD2_31[df_Cluster$BD2_31 == 3] <- 1 #폭음빈도가 월 1회인 사람을 1로 분류
df_Cluster$BD2_31[df_Cluster$BD2_31 == 4] <- 2 #폭음빈도가 주 1회인 사람을 2로 분류
df_Cluster$BD2_31[df_Cluster$BD2_31 == 5] <- 3 #폭음빈도가 거의 매일인 사람을 3으로 분류
df_Cluster$BD2_31 <- factor(df_Cluster$BD2_31, levels = c(0,1,2,3), ordered = T) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BD2_31) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BD2_31)) #해당 변수에 결측치가 다 제거되었는지 확인

# 교육수준 0:대학교 졸업 // 1:고등학교졸업 이하 
df_Cluster$edu[df_Cluster$edu %in% c(1,2,3)] <- 1 #학력이 고등학교 졸업 이하인 사람을 0으로 분류
df_Cluster$edu[df_Cluster$edu == 4] <- 0 #학력이 대학 졸업이상인 사람을 0으로 분류
df_Cluster$edu <- factor(df_Cluster$edu, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$edu) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$edu)) #해당 변수에 결측치가 다 제거되었는지 확인

# 나이대 별 분류
df_Cluster$age<- ifelse(df_Cluster$age >= 60, "60s", ifelse(df_Cluster$age>=50, "50s", ifelse(df_Cluster$age>=40, "40s", ifelse(df_Cluster$age>=30, "30s", ifelse(df_Cluster$age>=20, "20s", ifelse(df_Cluster$age>=10, "10s", "0s")))))) #나이를 구간별로 구분하여 10세미만은 “0s”, 그리고 10대는 “10s”, 20대는 “20s” 등으로 분류하여 60살 이상은 “60s”로 묶어서 분류하였다.
df_Cluster$age <- factor(df_Cluster$age, levels = c("0s","10s", "20s", "30s", "40s", "50s", "60s"), ordered = TRUE) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$age) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$age)) #해당 변수에 결측치가 다 제거되었는지 확인

# 만성질환 의사진단 가족력 여부 0:아니오 // 1:예
table(df_Cluster$HE_fh) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$HE_fh)) #해당 변수에 결측치가 다 제거되었는지 확인
df_Cluster$HE_fh <- as.factor(df_Cluster$HE_fh) #분류한 Value의 타입을 Factor로 변환

# 일 중강도 신체활동 여부 0:아니오 // 1:예 
table(df_Cluster$BE3_81) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$BE3_81)) #해당 변수에 결측치가 다 제거되었는지 확인
df_Cluster$BE3_81[df_Cluster$BE3_81 == 2] <- 0 #Attribute의 value가 2인 값을 0으로 할당
table(df_Cluster$BE3_81) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$BE3_81 <- as.factor(df_Cluster$BE3_81) #분류한 Value의 타입을 Factor로 변환

# 비정규직 여부 0:정규직 // 1:비정규직
table(df_Cluster$EC_wht_0) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$EC_wht_0)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$EC_wht_0[df_Cluster$EC_wht_0 == 1] <- 0 #Attribute value가 1인 값들을 0으로 할당
df_Cluster$EC_wht_0[df_Cluster$EC_wht_0 == 2] <- 1 #Attribue value가 2인 값들을 1로 할당
table(df_Cluster$EC_wht_0) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$EC_wht_0 <- as.factor(df_Cluster$EC_wht_0) #분류한 Value의 타입을 Factor로 변환

# 한 번에 마시는 음주량 0:4잔 이하 // 1:5잔 이상
table(df_Cluster$BD2_1) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$BD2_1)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$BD2_1[df_Cluster$BD2_1 %in% c(1,2)] <- 0 #Attribute value가 1, 2인 값들을 0으로 할당
df_Cluster$BD2_1[df_Cluster$BD2_1 %in% c(3,4,5)] <- 1 #Attribue value가 3, 4, 5인 값들을 1로 할당
table(df_Cluster$BD2_1) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$BD2_1 <- as.factor(df_Cluster$BD2_1) #분류한 Value의 타입을 Factor로 변환

# 활동 제한 여부 0:아니오 // 1:예
table(df_Cluster$LQ4_00) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$LQ4_00)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$LQ4_00[df_Cluster$LQ4_00 == 2] <- 0 #Attribue value가 2인 값들을 0으로 할당
table(df_Cluster$LQ4_00) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$LQ4_00 <- as.factor(df_Cluster$LQ4_00) #분류한 Value의 타입을 Factor로 변환

# 체중변화 0:변화없다 // 1:변화있다.
table(df_Cluster$BO1_1) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$BO1_1)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$BO1_1[df_Cluster$BO1_1 == 1] <- 0 #Attribute value가 1인 값들을 0으로 할당
df_Cluster$BO1_1[df_Cluster$BO1_1 %in% c(2,3)] <- 1 #Attribute value가 2, 3인 값들을 1로 할당
table(df_Cluster$BO1_1) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$BO1_1 <- as.factor(df_Cluster$BO1_1) #분류한 Value의 타입을 Factor로 변환

# 하루에 앉아서 보내는 시간 
table(df_Cluster$BE8_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE8_1)) #해당 변수에 결측치가 다 제거되어있는지 확인

# 소득 
table(df_Cluster$ainc) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$ainc)) #해당 변수에 결측치가 다 제거되어있는지 확인

# 우울증을 정상(PHQ-9 10점 미만) // 우울증 위험군(PHQ-9 10점이상)으로 분류
table(df_Cluster$mh_PHQ_S) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$mh_PHQ_S)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$mh_PHQ_S[df_Cluster$mh_PHQ_S < 10] <- 0 #Attribute value가 10보다 작은 값들을 0으로 할당
df_Cluster$mh_PHQ_S[df_Cluster$mh_PHQ_S >= 10] <- 1 #Attribute value가 10이상인 값들을 1로 할당
df_Cluster$mh_PHQ_S <- as.factor(df_Cluster$mh_PHQ_S) #데이터의 타입을 팩터타입으로 변환

# 보기 편하게 변수명 바꾸기
df_Cluster <- rename(df_Cluster, c(chr_dis_fh = "HE_fh",
                                   day_physic_act = "BE3_81",
                                   perma_posit = "EC_wht_0",
                                   liquor_amount = "BD2_1",   
                                   act_rest = "LQ4_00",        
                                   weight_change = "BO1_1",
                                   sit_time = "BE8_1",
                                   phq_score = "mh_PHQ_S")) #rename함수를 이용하여 변수명을 알기 쉽게 변경

# 변수명 바뀐 것을 확인하기 위해 table() 사용하여 6개의 Data Object
table(c("chr_dis_fh", "day_physic_act", "perma_posit", "liquor_amount", "act_rest", "weight_change","sit_time", "phq_score") %in% colnames(df_Cluster)) #변수명이 잘 변경되었는지 확인
