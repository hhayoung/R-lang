### 카이제곱 검정
# 분할표를 이용한 연관성 분석

raw_chisq <- read.csv(file="data/htest05.csv", header = T)

# 분할표
raw_Table <- table(raw_chisq)

head(raw_chisq)
tail(raw_chisq)

### 가설 설정
# 귀무가설 : 흡연 여부와 폐암유무는 연관성이 없다.
# 대립가설 : 흡연 여부와 폐암유무는 연관성이 있다.

### 카이제곱 테스트
chisq.test(raw_Table, correct = FALSE)
# p-value = 0.02686 < 0.05 대립가설 채택

# 셀기대도수 > 5 인 경우 : correct = FALSE
# 셀기대도수 < 5 인 경우 : coreect = TRUE
# 일반적으로 기대도수가 5이상으로 생각하고 검정을 한다.

### 결론
# -> 흡연 여부와 폐암유무는 연관성이 있다.