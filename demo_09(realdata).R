install.packages("foreign")
library(foreign)
# 모든 데이터를 다 사용할 수 없어서 전처리 필요
library(dplyr)
library(ggplot2)

# 데이터프레임 형태로 넣어주겠다.
raw_welfare <- read.spss("data_spss_Koweps2014.sav", to.data.frame=T)

welfare <- raw_welfare
welfare

# 데이터파악
# 이 데이터는 아래 명령으로는 파악하기 어렵다.
# 파악하는 방법은 전처리를 하면서 하나하나 검토하는 방법을 사용한다.
dim(welfare)
str(welfare)
head(welfare)
summary(welfare)
View(welfare)

# 분석하고자 하는 변수의 코드를 알기쉽게 변경
welfare <- rename(welfare, 
                  sex = h0901_4, # 성별
                  birth = h0901_5, # 태어난 연도
                  income = h09_din) # 소득
##################################
# 분석1 : 성별에 따른 소득
# 성별 변수 검토 (성별 전처리)
class(welfare$sex) #속성을 파악, "numeric" 

summary(welfare$sex) #최대 최소 확인, 무응답 9 없음

table(welfare$sex)

# 만일 9 있으면 NA 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

# 결측치 확인
table(is.na(welfare$sex))

# 성별 항목 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")


table(is.na(welfare$income))
table(welfare$sex)

# quick plot(빠르게 시각화도와줌)
qplot(welfare$sex)
# 남성이 여성보다 2배가 많다. (이렇게만 파악하면 X)
# -> 가구주 단위로 되어있기 때문에 남성이 더 많을 수 있기 때문에 이런 특징을 알고 분석을 해야 한다.

# 소득 변수 전처리
# 변수에 대한 검토
class(welfare$income)

# 단위를 알 수 없음, 평균 수입이 낮게 나오는 이유(1인가구, 미성년자, 고령층 포함)
# 데이터의 특성을 파악하고 그것에 맞게 해석하는 것이 중요함.
summary(welfare$income)
# 요약치를 보면서 해야하는 생각.
# 이 수치의 단위는 뭘까. 활용변수파일에 (단위:만원) 확인
# 단위가 가구단위인데 평균이 3336이면 모든 구성원의 소득이면 너무 적은거 아니야? 최소값은 -2억 정도 
# 가처분 소득은 소득에 부채까지 다 포함된 소득이구나~ 하고 분석
# 보통 통계할 때, 세금 내는거 빼고 소득
# 가구중에는 가구수는 많은데 수익이 없는 사람도 있을 수 있어서 평균값이 낮게 나오는 것.(미성년자도 있고, 1인가구도 있고)
# 이것만 보고 대한민국 평균 가구소득이 3천만원이구나 하면 안됨.
qplot(welfare$income)
# 보기불편.. x축 확대해서 보기
qplot(welfare$income) + xlim(0,10000)

table(is.na(welfare$income)) #결측치 없는지 확인

# 성별 소득 평균 분석
# 평균표 만들기
sex_income <- welfare %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income

# ggplot을 이용해서 그래프 만들기
ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_col()
# 실제로 소득이 남여차이가 2배라고만 분석하면 안 됨.
# 여성이 가구주인 경우는 1인경우가 많을 수 있고,
# 여성 혼자 아이를 키우는 경우일 수도 있고,
# 여성의 가구수는 적다라고 해석.
# 일반적으로 남성의 소득이 여성의 소득의 2배가 될 수 없기 때문에
# 가구의 구성원 수를 파악할 필요가 있다.
# 1인가구는 1인가구끼리, 다인가구는 다인가구끼리
# 데이터의 특성을 파악해서 세분화해서 분석해 나가야 함.
# 분석을 해나가면서 아이디어가 나오고, 분석의 질이 달라짐.


# 그래프의 해석: 남성이 여성보다 2배 넘게 소득이 많다. 실제로 남자의 소득이 여성의 소득의 2배라고 보면 안된다. 
# 가구주 남성의 소득평균, 가구주 여성의 소득평균

# 가구주가 여성인 경우에는 1인가구이거나 여성 혼자 아이를 키우는 경우가 많다. 즉, 남성에 비해 상대적으로 여성의 가구가 1인가구이거나 가구수가 적은 경우가 많다.

# 따라서, 가구의 구성원 수를 파악하거나, 1인가구, 다인가구끼리 비교를 해야겠다는 아이디어를 도출할 수 있다. 좀 더 세밀한 분석을 하게 된다.


# ---------------- 201014 --------------------

# 분석 2 : 나이와 소득의 관계

# 이 데이터는 나이변수가 따로 없음.
# 태어난 연도로 나이를 계산
library(dplyr)
library(ggplot2)

class(welfare$birth)

summary(welfare$birth)

# 이 데이터는 일부러 노년층의 특성을 더 반영하기 위해서 노년층의 데이터를 더 많이 표집했음을 알 수 있다.
# 다른 집단에 비해서 과대 표집되어 있는 경향의 자료이다. (다른 집단 = 젊은층)
qplot(welfare$birth)

# 9999가 없기 때문에 이상치가 없다는 것을 알 수 있음.
# 최대값이 9999이면 이상치
summary(welfare$birth)

# 이상치가 있는 경우에는 결측처리
welfare$birth <- ifelse(welfare$birth==9999, NA, welfare$birth)

table(is.na(welfare$birth)) # true없으면 결측치 없다는 뜻

# 지금까지의 결과를 보면 생년 변수에 오류가 없다는 것을 확인했음. 그럼, 나이라는 파생변수를 만들 수 있음.
welfare$age <- 2014 - welfare$birth +1 
summary(welfare$age)
qplot(welfare$age)

# 나이 변수의 전처리가 끝났음.

# 나이별 소득 평균 분석하기
# 평균표 만들기
age_income <- welfare %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

age_income

# 그래프 만들기 - 산점도
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_point()

# 이 그래프의 문제점은 뭘까?
# 예를 들어, 현재 13세는 딱 한명있는데 13세의 평균소득이라고 생각하면 안 됨. 또, 20대가 10명정도 밖에 안되는데 20대의 평균소득이라고 생각하면 안 됨.
# 20-29, 30-39 이런식으로 연령을 나눠주면 좀 더 대표할 수 있는 평균값이라고 볼 수 있다.

# 이 그래프의 오류를 찾아보자
# 이 그래프는 나이별로 그룹핑을 해서 평균을 구한 것임.
# 따라서, 13세가 1명인 경우에는 대표할 수 없는 평균임
# 의미 있는 통계치로 사용하기에는 부적합하다.
# 연령대별로 나눠서 보면 좋겠다는 아이디어를 도출해 낼 수 있다.
# -----------------------------------------------

# 분석 3 : 연령대에 따른 소득

# 연령대 변수 생성
welfare <- welfare %>% 
  mutate(age_gp = ifelse(age<30, "young", ifelse(age <=59,"middle", "old")))

# young은 다른 그룹에 비해서 빈도수가 너무 적다. 
# 따라서, 비교할 수 있는 대표성이 떨어진다. 
table(welfare$age_gp)
# middle    old  young 
#   3004   3979     65 

# young은 분석에서 제외하는 것이 타당하다는 판단을 할 필요가 있다.
qplot(welfare$age_gp)

# 연령대별 소득 평균표 생성(young은 제외)
welfare_income <- welfare %>% 
  filter(age_gp != "young") %>% 
  group_by(age_gp) %>% 
  summarise(mean_income = mean(income))

welfare_income

# 그래프 생성하기
ggplot(data=welfare_income, aes(x=age_gp, y=mean_income)) +
  geom_col()

# -----------------------------------------------
# 분석 4 : 연령대 및 성별에 따른 소득(디테일하게)

sex_income <-  welfare %>% 
  filter(age_gp != "young") %>% 
  group_by(age_gp, sex) %>% 
  summarise(mean_income=mean(income))

sex_income

#그래프
ggplot(data=sex_income, aes(x=age_gp, y=mean_income, fill=sex)) +
  geom_col() # 기본값은 position="stack" 생략 가능

ggplot(data=sex_income, aes(x=age_gp, y=mean_income, fill=sex)) +
  geom_col(position="dodge")
