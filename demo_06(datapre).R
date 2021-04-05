## 데이터 가공하기
## 데이터 전처리(분석하고자 하는 형태로(원하는 형태로) 데이터를 가공하는 것)

# 전처리(Preprocessing)시 자주 사용하는 패키지(dplyr)

library(dplyr)
exam <- read.csv("r_data/csv_exam.csv")
exam

# exam에서 class가 1인 데이터 추출하기
# %>% : dplyr패키지에서만 쓰는 오퍼레이터(pipe operator)
# a().b().c()
# exam %>% a() %>% b()
# 함수와 함수를 이어줌.

exam %>% filter(class==1) #1반만
exam %>% filter(class==2) #2반만
exam %>% filter(class!=1) #1반빼고

exam %>% filter(math < 50)

# 여러 조건을 만족하는 행 추출하기(& 연산자 이용)
exam %>% filter(class==1 & math>=50)
exam %>% filter(class==2 & english>=70)

# 여러 조건 중 하나 이상 충족하는 행 추출하기(| 연산자 이용)
exam %>% filter(math>=90 | english>=90)
exam %>% filter(math<90 | science<50)

exam %>% filter(class==1 | class==3 | class==5 )
# 더 간편한 방법
# %in%: 매치 연산자(Matching Operator)
exam %>% filter(class %in% c(1,3,5)) #1,3,5반 해당 데이터 추출

# 추출한 행으로 데이터 만들기
class1 <- exam %>% filter(class==1)
class2 <- exam %>% filter(class==2)
mean(class1$math) #1반 수학 평균 점수
mean(class2$math) #2반 수학 평균 점수


# p133. 혼자서 해보기
# Q1.
mpg <- as.data.frame(ggplot2::mpg)
a <- mpg %>% filter(displ <= 4)
b <- mpg %>% filter(displ >= 5)
mean(a$hwy)
mean(b$hwy)

# Q2. 
audi <- mpg %>% filter(manufacturer=="audi")
toyota <- mpg %>% filter(manufacturer=="toyota")
mean(audi$cty)
mean(toyota$cty)

# Q3.
mf_three <- mpg %>% filter(manufacturer %in% c("chevrolet","ford","honda"))
mean(mf_three$hwy)


#-----------201008-------------
library(dplyr)
exam <- read.csv("r_data/csv_exam.csv")
## 변수(컬럼) 추출
exam %>% select(math)
exam %>% select(english)
exam %>% select(math, english, science) # ,사용해서 여러컬럼 출력

exam %>% select(-math) # math만 빼고 다 가져옴
exam %>% select(-math, -english)

# filter와 select 조합해서 출력하기

# class가 1인 행만 추출한 다음에 english 변수를 추출
exam %>% 
  filter(class==1) %>% 
  select(english)

exam %>% 
  select(id, math) %>% 
  head(10) # head

# p138 혼자서 해보기
library(ggplot2)
# Q1.
mpg <- as.data.frame(ggplot2::mpg)
df <- mpg %>% 
  select(class, cty)
head(df)
# Q2.
df_suv <- df %>% 
  filter(class=="suv")
mean(df_suv$cty)

df_compact <- df %>% 
  filter(class=="compact")
mean(df_compact$cty)

## 정렬하기
exam %>% arrange(math) # 기본값은 오름차순
exam %>% arrange(desc(math)) # 내림차순

# 정렬 기준 변수를 여러개 지정하기
exam %>% arrange(class, math) # 클래스로 정렬 > 클래스대로 수학점수 정렬

# p141. 혼자서 해보기
df_mpg <- as.data.frame(ggplot2::mpg)
df_mpg %>% 
  filter(manufacturer=="audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)
# -> arrange할때는 df_mpg$hwy가 아니라 그냥 hwy해야 함.
# 어짜피 df_mpg %>% 이기 때문에 df_mpg안에 있는 변수들을 그냥 사용하나봄.

## dplyr 패키지의 mutate() 함수를 이용한 파생변수 추가하기
exam %>% 
  mutate(total=math+english+science) %>% 
  head
# 데이터프레임명$변수명 <- 이제 이렇게 쓰지 않아도 된다.

# 여러개의 파생변수를 한번에 추가하기
exam %>% 
  mutate(total = math+english+science, mean=(math+english+science)/3) %>% 
  head

# mutate() 함수에 ifelse() 적용하기
exam %>% 
  mutate(test = ifelse(science>=60, "pass","fail")) %>% 
  head

# 정렬까지
exam %>% 
  mutate(total = math+english+science) %>% 
  arrange(total) %>% 
  head

exam # 파생변수가 추가 안됨.
# mutate()를 안썼을때는 만들고 나서 할당을 해줘야 다른 함수들을 사용할 수 있었음.
# dplyr패키지를 쓰면 할당할 필요없이 정렬도 하고 다 할 수 있음.
# mutate()는 무조건 dataframe형태일 때만 사용가능

# p144. 혼자서 해보기
mpg <- as.data.frame(ggplot2::mpg)
# Q1.
df_mpg <- mpg
df_mpg <- df_mpg %>%
  mutate(mileage=hwy+cty) %>% 
  head
# Q2.
df_mpg <- df_mpg %>% 
  mutate(mean_mileage=mileage/2) %>% 
  head
# Q3.
df_mpg %>% 
  arrange(desc(mean_mileage)) %>% 
  head(3)
# Q4.
mpg %>% 
  mutate(mileage=hwy+cty, mean_mileage=mileage/2) %>% 
  arrange(desc(mean_mileage)) %>% 
  head(3)

## 집단별로 요약 통계치 산출하기

# 집단별로 요약
exam %>% 
  group_by(class) %>% # 각 반에 해당하는 수학평균이 나올 것.
  summarise(mean_math = mean(math)) # mean_math도 파생변수

# 여러 요약 통계량 한 번에 산출하기
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math), 
            sum_math = sum(math),
            median_math = median(math),
            n = n()) # n(): 행의 개수(파라미터X)

# 요거는 그룹이 되지 않음(이렇게 쓰면 X)
exam %>% 
  group_by(class) %>% 
  mutate(mean_math = mean(math))
# group_by와 summarise는 세트!

# 각 집단별로 다시 집단 나누기
mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

# 여러개의 함수를 한번에 사용 가능
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
  mutate(tot=(cty+hwy)/2) %>% 
  summarise(mean=mean(tot)) %>% 
  arrange(desc(mean)) %>% 
  head(5)


# p150. 혼자서 해보기
head(mpg)
mpg <- as.data.frame(ggplot2::mpg)
# Q1.
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty=mean(cty))
# Q2.
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty))
# Q3.
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy=mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)
# Q4.
mpg %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>% 
  summarise(cnt_compact=n()) %>% 
  arrange(desc(cnt_compact))

## 데이터 합치기(가로, 세로)
# 데이터베이스의 join과 비슷

test1 <- data.frame(id=c(1,2,3,4,5), midterm=c(60,90,60,90,85))
test2 <- data.frame(id=c(1,2,3,4,5), final=c(70,60,80,95,80))
test1
test2

# 가로 합치기 (left_join() 기준값이 같아야함)
total <- left_join(test1, test2, by="id") # by="문자열" : 합칠 기준
total

name <- data.frame(class=c(1,2,3,4,5), teacher=c("kim","park","lee","kang","hong"))
name

# class 기준으로 합치기
exam_new <- left_join(exam, name, by="class")
exam_new

# 세로 합치기 (bind_rows() )
group_a <- data.frame(id=c(1,2,3,4,5), test=c(60,90,70,90,85))
group_b <- data.frame(id=c(6,7,8,9,10), test=c(65,96,74,90,85))

group_all <- bind_rows(group_a, group_b)
group_all

# p156. 혼자서 해보기

# stringsAsFactors = F는 
# 문자를 factor타입으로 만들지 말라는 뜻
# 기본적으로 R은 문자열을 factor타입으로 바꾸려는 기본 성질을 갖고 있다.
# factor는 범주형 데이터를 의미(계산할 수 없는 데이터)

# 범주형 데이터   vs    수치형 데이터
# 명목형(범위(성별)) 연속형(데이터가 소수점을 표현할 수 있으면 연속형(키,몸무게))
# 순서형(기준온도)   이산형(연속된 데이터가 아닌 것(사람1명 1.4명X))
# 온도가 있을 때, 10도는 저온, 20도는 평균, 30도는 고온. 10,20,30 숫자지만 수치형X

# 데이터를 분석할 때, 범주형 데이터와 수치형 데이터의 분석 기법은 다르다.

fuel <- data.frame(fl=c("c","d","e","p","r"),
                   price_fl=c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = F)
fuel
# Q1.
head(mpg) # fuel과 동일한 변수 fl
mpg <- as.data.frame(ggplot2::mpg)
mpg <- left_join(mpg, fuel, by="fl")
# Q2.
mpg %>% 
  select(model, fl, price_fl) %>% 
  head(5)
