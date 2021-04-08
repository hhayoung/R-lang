# 변수 설명
# GRE.Score : GRE 점수(영미 대학원에 들어갈 때 필요한 점수)
# TOEFL.Score : 토플 점수
# University.Rating : 학부 대학 레이팅
# SOP : 자기소개서 점수
# LOR : 추천서 점수
# CGPA : 학부 학점
# Research : 연구경험유무
# Chance.of.Admit : 대학원 합격 확률

rawdata1 <- read.csv(file="university.csv", header=TRUE)
str(rawdata1)
# 지도학습으로 할지, 비지도학습으로 할지 => target을 가지고 선택할 수 있음
# Chance.of.Admit <- 이게 target이니까 지도학습!

is.na(rawdata1$GRE.Score)  # 데이터가 많으면 보기 힘들다
sum(is.na(rawdata1$GRE.Score))   # 0값이 나왔을 때는 결측치가 없다. FALSE = 0, TRUE = 1
sum(is.na(rawdata1$TOEFL.Score))
sum(is.na(rawdata1$University.Rating))
sum(is.na(rawdata1$SOP))
sum(is.na(rawdata1$LOR))
sum(is.na(rawdata1$CGPA))
sum(is.na(rawdata1$Research))
sum(is.na(rawdata1$Chance.of.Admit))

# 중복 없는 유일한 관측치 선별하기
unique(rawdata1$GRE.Score)
unique(rawdata1$TOEFL.Score)
unique(rawdata1$University.Rating)
unique(rawdata1$SOP)
unique(rawdata1$LOR)
unique(rawdata1$CGPA)
unique(rawdata1$Research)  # 명목형(=이진형,범주형 0,1만 있기 때문에)
unique(rawdata1$Chance.of.Admit)

research_table <- table(rawdata1$Research)
research_table

# 타겟 변수 최대값/최소값 확인
# 타겟의 범위 확인 0.34~0.97
max(rawdata1$Chance.of.Admit)
min(rawdata1$Chance.of.Admit)
# 일단 숫자
# 예/아니오가 아닌 숫자인데 그 범위가 0에서 1사이
# 로지스틱 회귀분석을 써야겠다!

# 분류, 회귀중에
# 명목형/범주형 데이터 분류를 해왔음.
# 종속변수가 연속형 => Regression (회귀분석을 생각해야 함.)
# 그럼 선형회귀분석할까? ㄴㄴ
# 지금 데이터의 범위가 0과 1사이의 범위에 있음
# 0,1 딱 떨어지는게 아니라 그 사이의 확률값을 가지고 있다
# 로지스틱 회귀분석을 이용


# 변수 별 히스토그램
par(mfrow=c(3,2), mar=c(5,4,4,2))  # mar : 상하좌우 margin 값
hist(rawdata1$GRE.Score, main="GRE score histogram", xlab="GRE score", col="orange")
hist(rawdata1$TOEFL.Score, main="TOEFL score histogram", xlab="TOEFL score", col="green")
hist(rawdata1$SOP, main="SOP score histogram", xlab="SOP core", col="blue")
hist(rawdata1$LOR, main="LOR score histogram", xlab="LOR score", col="yellow")
hist(rawdata1$CGPA, main="CGPA score histogram", xlab="CGPA score", col="darkmagenta")
hist(rawdata1$Research, main="Research score histogram", xlab="Research score", col="yellow")
hist(rawdata1$Chance.of.Admit, main="Acceptance rate histogram", xlab="Acceptance rate", col="red")

# 산점도
plot(rawdata1)
# 대부분 점수가 높으면 합격률이 높다.
# 각각 변수간의 관계도를 한번에 파악할 수 있다.

# 트레이닝 / 테스트 데이터 나누기
set.seed(2020)
newdata <- rawdata1
train_ratio <- 0.7
datatotal <- sort(sample(nrow(newdata), nrow(newdata)*train_ratio))

train <- rawdata1[datatotal,]
test <- rawdata1[-datatotal,]

# 회귀 분석으로 대학원 합격률 예측(caret 패키지 사용)
library(caret)

# 로지스틱 회귀분석
ctrl <- trainControl(method="repeatedcv", repeats=5)
logistic_fit <- train(Chance.of.Admit ~ .,
                      data=train,
                      method="glm",
                      trControl=ctrl,
                      preProcess=c("center","scale"),
                      metric="RMSE")
# 분류로 할 때는 Accuracy, 회귀분석에서는 MSE/RMSE 사용(최소값이 적은게 성능이 좋은거니까)
logistic_fit
logistic_pred <- predict(logistic_fit, newdata=test)
postResample(pred=logistic_pred, obs=test$Chance.of.Admit)
# RMSE : MSE에 루트 씌운 값(작으면 작을수록 좋다)
# MSE : 평균 제곱 오차(이 식은 알고 있는게 좋을 것 같다...)
# MAE : 절대치 오차
# Rsquared : 82프로는 설명할 수 있는 데이터다
# 예를 들면, 우리가 예측한 선형모델이 일자로 쭉 그어졌음.
# 예측한 y의 평균값 선이 있다고 하고
# 실제값<->예측값 = 오차. 에러에 의한 오차는 설명 불가능
# 평균값<->예측값 설명가능
# 전체의 변동분 = 설명불가 + 설명가능
# 설명이 가능한 범위가 많으면 많을수록 좋은 모델
# 설명 불가능한게 많을수록 오차가 많다는 소리니까
# Rsquared(R제곱값) : 전체 변동성 중 설명 가능한 변동성의 비율

logistic_pred <- predict(logistic_fit, newdata=test)
logistic_pred # 각 피쳐에 대한 합격률
# 9번째 줄 엑셀에서는 10번째 행 비교해보면 실제값은 0.5, 예측은 0.55로 비슷하게 예측하고 있음.
# 498번째 줄 엑셀에서는 499번째 행 실제값은 0.93, 예측은 0.95로 예측


# 로지스틱 회귀분석은 분류형도 있고 연속형도 있음
# 전에 했던 로지스틱 회귀분석은 분류형을 했던 거고
# 오늘은 연속형으로 예측모형을 만든 것.

# confusionMatrix(logistic_pred, test$Chance.of.Admit)
# -> 종속변수가 연속형일 때는 이 코드를 쓰면 안되는 것 같음.


# ----------201111 엘라스틱넷-----------

# 엘라스틱넷
ctrl <- trainControl(method="repeatedcv", repeats=5)
logit_penal_fit <- train(Chance.of.Admit ~ ., # ~ . 모든 피쳐에 대해서
                         data=train,
                         method="glmnet",
                         trControl=ctrl,
                         preProcess=c("center","scale"),
                         matric="RMSE")  # matric(모형평가)
logit_penal_fit
# RMSE 가 가장 작은 값일 때의 알파값, 람다값
# 알파가 0.1, 람다가 0.0025481481

plot(logit_penal_fit)

# 예측
logit_penal_pred <- predict(logit_penal_fit, newdata=test)
# RMSE값 가져오면 앞에꺼랑 비교해 볼 수 있음.

# RMSE값을 계산하는 함수
postResample(pred=logit_penal_pred, obs=test$Chance.of.Admit)
#      RMSE   Rsquared        MAE 
#0.05759752 0.81881695 0.04075955 
#     6프로    81프로 설명할수있고, 

#위에 있는 logistic_fit 결과와 거의 비슷하다.