### ANOVA 검정 : 여러 집단의 평균 차이 검정
# ANOVA 분석을 위해서는 lawstat 패키지 필요함.
install.packages("lawstat")
library(lawstat)

raw_anova <- read.csv(file="data/htest04.csv", header = T )
head(raw_anova)
tail(raw_anova)

groupA4 <- raw_anova[raw_anova$group=='A', 1:2]
groupB4 <- raw_anova[raw_anova$group=='B', 1:2]
groupC4 <- raw_anova[raw_anova$group=='C', 1:2]

mean(groupA4[,2])
mean(groupB4[,2])
mean(groupC4[,2])

### 가설 설정
# 귀무가설 : 세 집단간의 평균 차이가 없다.
# 대립가설 : 세 집단 평균 차이가 있다.(단측검정이 힘들다)

### 정규성 검정
# A집단
shapiro.test(groupA4[,2]) # p-value = 0.8978
qqnorm(groupA4[,2])
qqline(groupA4[,2])

# B 집단
shapiro.test(groupB4[,2]) # p-value = 0.9108
qqnorm(groupB4[,2])
qqline(groupB4[,2])

# C 집단
shapiro.test(groupC4[,2]) # p-value = 0.6313
qqnorm(groupC4[,2])
qqline(groupC4[,2])

### 분산 동질성 검정
# ANOVA 검정에서는 var.test() 대신에 levene, bartlett 함수를 이용한다.
levene.test(raw_anova$height, raw_anova$group)
# 키를 테스트할건데, 그룹별로 하겠다.
# p-value = 0.3298 > 0.05 귀무가설 채택 - 세 집단간의 분산이 동일하다.
bartlett.test(height~group, data = raw_anova)
# p-value = 0.3435 > 0.05 -> 세 집단간의 분산이 동일하다.

### ANOVA 테스트 : aov 함수를 사용한다.
rawAnova <- aov(height ~ group, data = raw_anova)
summary(rawAnova)
# Pr(>F) 값이 p-value 임
# Pr(>F) = 1.14e-05 = 0.0000114 < 0.05 귀무가설 기각. 대립가설 채택
# Mean Sq 는 오차를 뜻하는데
# group 658.4 집단간의 오차
# Residuals 36.2는 집단내의 오차
# value 18.2 관측치 

### 결론
# -> 세 집단 평균 차이가 있다.