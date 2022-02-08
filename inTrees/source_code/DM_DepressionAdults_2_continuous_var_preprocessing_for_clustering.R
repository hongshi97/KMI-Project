# 연속형 변수 전처리

# 주당 평균 근로 시간 전처리
df$EC_wht_23 <- ifelse(df$EC_wht_23 == 888, 0, ifelse(df$EC_wht_23 == 999, NA, df$EC_wht_23))
#비해당(최근 1년 동안 일을 하지 않음)(888)을 0으로, 모름/무응답(999)를 NA로 대체

# 주중 하루 평균 수면 시간(12세 이상) 전처리
df$Total_slp_wk[df$Total_slp_wk %in% c(8888,9999)] <- NA #비해당(소아)(8888), 모름/무응답(9999)를 NA로 대체

# 주말 하루 평균 수면 시간(12세 이상) 전처리
df$Total_slp_wd[df$Total_slp_wd %in% c(8888,9999)] <- NA #비해당(소아)(8888), 모름/무응답(9999)를 NA로 대체

# 하루 평균 수면 시간 (Total_slp)이라는 새로운 Attribute 생성
df$Total_slp <- (df$Total_slp_wd*5 + df$Total_slp_wk*2) / 7 #(주중 하루 수면 시간*5 + 주말 하루 평균 수면 시간*2)/7로 변수 생성