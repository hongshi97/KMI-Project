# 의사결정나무 만들기
df_DT <- df_Cluster %>% select(chr_dis_fh, day_physic_act, perma_posit, liquor_amount, act_rest, weight_change, sit_time, ainc, phq_score) #의사결정나무에 넣을 변수들 추출

DT_Cluster_row_idx <- createDataPartition(df_DT$phq_score, p=0.75, list=FALSE) #phq_score Attribute를 기준으로 층화추출법을 이용하여  훈련 및 테스트 데이터를 3:1로 분류
DT_Cluster_train_data <- df_DT[DT_Cluster_row_idx, ] #훈련 데이터를 저장
DT_Cluster_test_data <- df_DT[-DT_Cluster_row_idx, ] #테스트 데이터를 저장

DT_Cluster_rpart_result <- rpart(phq_score~., data=DT_Cluster_train_data, control = rpart.control(minsplit=2)) #phq_score를 종속변수로 하여 훈련 데이터를 활용하여 의사결정나무 그리기
rpart.plot(DT_Cluster_rpart_result) #의사결정나무를 시각화

# 의사결정나무 평가
actual <- DT_Cluster_test_data$phq_score #테스트 데이터에 원래 저장되어 있던 phq_score attribute value 저장
expect <- predict(DT_Cluster_rpart_result, DT_Cluster_test_data, type="class") #테스트 데이터를 사용하여 위에서 모델링한 의사결정나무에서 예측 실시 후 예측한 phq_score값을 저장
confusionMatrix(expect, actual, mode="everything") #실제값과 예측값을 confusionMatrix로 만들어 비교

# 의사결정나무 가지치기
printcp(DT_Cluster_rpart_result) #위에서 만든 의사결정나무의 complexity추출 및 가지치기 경우의수 선별
DT_Cluster_rpart_result_prune_tree <- prune(DT_Cluster_rpart_result, cp=0.011792) #가장 최적의 complexity를 이용하여 가치지기 실시
rpart.plot(DT_Cluster_rpart_result_prune_tree) #가지치기한 의사결정나무를 시각화

# 의사결정나무 가지친 모델 평가
DT_Cluster_actual_prune <- DT_Cluster_test_data$phq_score #테스트 데이터에 원래 저장되어 있던 phq_score attribute value 저장
DT_Cluster_expect_prune <- predict(DT_Cluster_rpart_result_prune_tree, DT_Cluster_test_data, type="class") #테스트 데이터를 사용하여 가지치기한 의사결정나무에서 예측 실시 후 예측한 phq_score값을 저장
confusionMatrix(DT_Cluster_expect_prune, DT_Cluster_actual_prune, mode="everything") #실제값과 예측값을 confusionMatrix로 만들어 비교
