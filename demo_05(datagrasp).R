## 데이터 파악하기
# 함수      기능
# head()    데이터의 앞부분 출력
# tail()    데이터의 뒷부분 출력
# View()    원래 데이터를 확인
# dim()     데이터의 차원(행,열(몇차원인지))
# str()     데이터의 속성(변수) 출력
# summary() 요약된 통계치 출력

# 데이터 준비하기
exam <- read.csv("r_data/csv_exam.csv")
head(exam) #이 데이터는 어떤 거지? 아 이런거구나~
head(exam,10) # 앞에서부터 10행 까지 출력
tail(exam)
tail(exam, 7)
# 데이터를 불러오는데 너무 오래 걸리기 때문에 앞뒤부분만 보고 파악하기

View(exam)

# dim(): 몇 행 몇 열로 구성되어있는지 파악악
dim(exam)

# str(): 속성(변수) 파악. structure의 약자
str(exam)

# names() : 변수만 파악
names(exam)

# summary(): 요약된 통계량 산출
summary(exam)
# math 부분:
# Min: 최저점수, 1st Qu.: 하위25퍼, Median: 중간점수
# 3rd Qu.: 상위25퍼, Max: 최고점수, Mean: 평균점수

# mpg 데이터 혼자 파악해보기
head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)

#------------201007----------------

## 데이터 파악하기
# 데이터프레임으로 만들 때 data.frame() 사용

# mpg 데이터는 ggplot2 패키지의 데이터
# R을 설치하는 것만으로 제공되는 데이터도 있다.

library(help=datasets) # 기본적인 데이터프레임들 확인할 수 있음.
?BOD #BOD 데이터셋이 뭔지 궁금할 때
BOD

quakes #지진관련자료
# 총 1000개의 행이 있는데 200개까지만 나오기 때문에 1000개를 다 보고싶을때는
options(max.print = 1000) # 200행 * 5열 = 1000 이렇게 계산되기 때문에
options(max.print = 5000) # 1000행 * 5열 = 5000 해야 1000개의 데이터를 볼 수 있다.
quakes
dim(quakes) #차원의 구조를 파악 (행 열)

library(ggplot2)
mpg
dim(mpg)


data() # 기본 데이터셋과 로드한 패키지안에 있는 데이터셋 확인
# ggplot2를 로드했기 때문에 ggplot2안에 있는 데이터셋도 보여진다.


# ggplot2의 mpg 데이터를 데이터프레임 형태로 불러오는 것.
mpg <- as.data.frame(ggplot2::mpg)
# 더불콜론(::)을 이용하면 특정 패키지에 들어있는 함수나 데이터를 지칭할 수 있다.
# ggplot2::mpg란 ggplot2에 들어있는 mpg데이터를 지칭한다.
mpg

head(mpg)
dim(mpg)
str(mpg)
summary(mpg)

## 데이터 수정하기 - 변수명 바꾸기
library(dplyr)

df_raw <- data.frame(var1 = c(1,2,1), var2 = c(2,3,2))
df_raw

# 뭔가를 수정할때 원본데이터를 그대로 두고 작업하는게 좋음
df_new <- df_raw #복사본 만들기
df_new #복사 잘됐는지 확인

# 변수명 바꾸기
# rename() '새변수=기존변수' 순서로 사용
# var2를 v2로 수정
df_new <- rename(df_new, v2=var2) 
df_new

data.frame(age=numeric(0), gender=character(0), weight=numeric(0))
my_df = data.frame(age=numeric(0), gender=character(0), weight=numeric(0))
my_df <- edit(my_df) #데이터 편집기(쉽게 데이터를 넣을 수 있음)
my_df #데이터가 들어감
# int는 소수점 불가능, numeric은 가능

# p112 혼자서 해보기
mpg <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg
mpg_new <- rename(mpg_new, city=cty, highway=hwy)
head(mpg_new)

## 파생 변수 만들기(분석을 더 하고자 할 때 추가적인 분석 가능)
df <- data.frame(var1=c(4,3,8), var2=c(2,6,1))
df

# $변수명 으로 바로 생성도 가능
df$var_sum <- df$var1 + df$var2
df

df$var_mean <- (df$var1 + df$var2) / 2
df

# mpg 파생변수 생성
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)

mean(mpg$total)

## 조건문 활용한 파생변수 생성
summary(mpg$total)
hist(mpg$total)

# ifelse를 이용한 합격 판정 변수 만들기
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg,20)

# 합격/불합격 자동차 수 확인하기
table(mpg$test) #table(): 빈도수

# 그래프 시각화
qplot(mpg$test)

mpg$grade <- ifelse(mpg$total>=30, "A", ifelse(mpg$total >= 20, "B", "C"))
head(mpg,20)

# 빈도수를 이용한 연비 등급 파악하기
# table(): 빈도표, qplot(): 빈도막대그래프
table(mpg$grade) # 등급 빈도표 생성
qplot(mpg$grade) # 등급 빈도 막대 그래프 생성

# p123. 분석 도전!
data()
library(ggplot2)
library(dplyr)
# Q1. ggplot2의 midwest 데이터를 데이터 프레임 형태로 불러온 다음 데이터 특징을 파악하세요.
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

# Q2. poptotal(전체인구)변수를 total로, popasian(아시아 인구) 변수를 asian으로 수정하세요.
?rename # => rename {dplyr} 패키지확인
midwest_new <- midwest
midwest_new <- rename(midwest_new, total=poptotal, asian=popasian)
head(midwest_new)

# Q3. total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를 만들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요.
midwest_new$ratio <- (midwest_new$asian / midwest_new$total) * 100
hist(midwest_new$ratio)

# Q4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생 변수를 만들어 보세요.
mean(midwest_new$ratio)
midwest_new$group <- ifelse(midwest_new$ratio > mean(midwest_new$ratio), "large", "small")
head(midwest_new)

# Q5. "large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도표와 빈도 막대 그래프를 만들어 확인해 보세요.
table(midwest_new$group)
qplot(midwest_new$group)
