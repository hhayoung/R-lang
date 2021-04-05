## 데이터 정제

# 결측치(Missing Value, NA: Not available)
# : 누락된 값, 비어있는 값

df <- data.frame(sex = c("M", "F", NA, "M", "F"), # NA에는 따옴표 ㄴㄴ
                 score = c(5, 4, 3, 4, NA))  
df
# 결측치 확인하기(직접 확인이 어려우니까 함수 사용)
is.na(df) # 결측치가 맞는지(TURE) 아닌지(FALSE)

# 결측치를 눈으로 일일이 확인하기 어려울 때
table(is.na(df))

# 특정 변수의 결측치를 알고 싶을 때
table(is.na(df$sex)) #sex 변수의 결측치 빈도 출력
table(is.na(df$score)) #score 변수의 결측치 빈도 출력

# 결측치 포함된 상태에서 분석했을 경우
mean(df$score)
## [1] NA <- 출력 모습
sum(df$score)
## [1] NA <- 분석 불가능

# 결측치 제거
library(dplyr)

# score가 NA인 데이터만 출력
# 결측치가 TRUE인 것만 남음
df %>% filter(is.na(score))

# score의 결측치를 제거, !은 not
df %>% filter(!is.na(score))

# 원자료 df에는 그대로 결측치가 남아있기 때문에 할당해줘야 함.
# 결측치 제외한 데이터 할당
df_nomiss <- df %>% filter(!is.na(score))

# 결측치 제거 후 분석했을 경우
mean(df_nomiss$score) ## [1] 4
sum(df_nomiss$score)  ## [1] 16 <- 분석 가능

# 여러 변수의 결측치를 동시에 제거하기
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

# na.omit()은 결측치가 하나라도 있으면 제거
# 편리하기는 한데 실제로 잘 사용하지 않는다.(특수한 경우 사용)
# 사용하지 않는 이유? 
# 예를 들면, 내가 분석하고자 하는 데이터가 a,b,c가 있다고 가정했을 때,
# a와 b를 분석하고자 하는데 a,b에는 결측치가 없고 c에 결측치가 있으면 a,b도 같이 지워져 버리기 때문에 데이터 손실이 일어난다. 불편하더라도 filter()를 이용해서 내가 지정해서 결측치를 제거하는게 좋다.
# 특수한 경우: 머신러닝 모형 만들 때(na가 들어있으면 모형을 만들 수 없는 알고리즘이 있음), 데이터가 정말정말정말 많은 경우(데이터가 수억개인데, 결측치가 수백개이면 제거해도 무관)
df_nomiss2 <- na.omit(df)
df_nomiss2

#---------------201013-----------------

# na.rm = T : 결측치 제외하는 기능의 인자
# 모든 함수에서 사용할 수 있는 인자는 아니다.

df
# 결측치 제외 후 평균구하기
mean(df$score, na.rm=T)

# 결측치 제외 후 합계구하기
sum(df$score, na.rm=T)

exam <- read.csv("r_data/csv_exam.csv")
exam

# 5행10행15행에 math변수값을 NA(결측치)로 만들겠다.
exam[c(5,10,15), "math"] <- NA
exam

# 변수 여러개 가능
exam[c(3,6,9), c("english", "math")] <- NA

# 평균값 오류 : NA 출력
exam %>% summarise(mean_math = mean(math))

exam %>% summarise(mean_math = mean(math, na.rm=T))

exam %>% summarise(mean_math = mean(math, na.rm=T),
                   sum_math = sum(math, na.rm=T),
                   median_math = median(math, na.rm=T),
                   max_math = max(math, na.rm=T))

# 결측치 대체(Imputation)하기
# 대표값(평균, 최빈값 등)으로 일괄 대체
# 최근에는 머신러닝 모형을 이용해 다른 변수를 이용하여 추정된 값으로 대체한다.
# 여전히 가장 많이 사용하는 방법으로는 평균 대체법이다.

# 평균을 구해서
mean(exam$math, na.rm=T)

# 결측치 대체
exam$math <- ifelse(is.na(exam$math), 59, exam$math)

table(is.na(exam$math)) #빈도수 확인

# 대체된 값으로 평균 분석 (가능해짐)
mean(exam$math)

# p170. 혼자서 해보기
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212), "hwy"] <- NA
# Q1.
table(is.na(mpg$drv))
table(is.na(mpg$hwy)) #결측치가 5개나 있네?_?
# Q2.
mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

#------------------------------

## 이상치 : 논리적 오류값, 극단적인 값

outlier <- data.frame(sex = c(1,2,1,5,2,1), #sex 1,2일 때
                      score = c(5,4,3,4,2,7)) #score 최대점수 5일 때
outlier

# 이상치 확인
table(outlier$sex) #5라는 이상치 발견
table(outlier$score) #7이라는 이상치 발견

outlier$sex <- ifelse(outlier$sex == 5, NA, outlier$sex)
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
# 최대값이 5라고 정해져있기 때문에 5이상은 결측치로 조건 달아도 된다.

# 결측치 제거 후 성별에 따른 score 분석
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

# 극단적인 값 제거하기
# 논리적으로 판단(몸무게가 200?), 통계적 판단(이 값은 나올 수 없다.), boxplot을 이용하여 제거

mpg <- as.data.frame(ggplot2::mpg)
min(mpg$hwy)
max(mpg$hwy)
boxplot(mpg$hwy)
# 그림 위에 2개의 점 -> 이상치니까 제거
# 윗수염아랫수염끝나는 곳의 값을 알면 그 값범위를 벗어나면 제거해주면 된다.
# boxplot의 전체 통계치
boxplot(mpg$hwy)$stat
#[1,]   12  <- 아랫수염끝나는곳
#[2,]   18  <- 박스밑면
#[3,]   24  <- 박스 중앙 굵은선
#[4,]   27  <- 박스윗면
#[5,]   37  <- 윗수염끝나는곳

mpg$hwy <- ifelse(mpg$hwy<12 | mpg$hwy > 37, NA, mpg$hwy)

table(is.na(mpg$hwy))
# FALSE  TRUE 
#   231     3

mpg

mpg <- as.data.frame(ggplot2::mpg)
mpg %>% 
  select(hwy) %>% 
  filter(hwy>37)

# p178. 혼자서 해보기
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,20,30,40), "drv"] <- "k"
mpg[c(50,60,70,80), "cty"] <- c(3,4,39,42)

# Q1.
# 이상치 확인하기
table(mpg$drv) # k 이상치 확인인

mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA)
#왜 %in%으로 했을까? 이상치가 'k'뿐만 아니라 다른것들도 있을 수 있으니까 일일히 NA로 바꿀 수 없음. 
table(mpg$drv)

# Q2.
boxplot(mpg$cty)
boxplot(mpg$cty)$stat
# 9~26을 벗어나면 NA 처리
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty) 

boxplot(mpg$cty)

# Q3.
# 이상치 제외한 분석
mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))
