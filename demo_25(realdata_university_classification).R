# 타겟값이 연속형값인데 
# 이제는 합격률을 가지고 합격했는지 안했는지 합격 여부를 판단해보기
# 분류 모델 적용해야 함.

# classification(분류)으로 합격 여부 예측하기

# target값 --> 명목형(범주형) --> 분류(Classification)
# target값 -->   연속형 숫자  --> 회귀(Regression)

# 합격 여부를 판단하려면 기준이 있어야 함.

library(caret)

rawdata3 <- read.csv(file="university.csv", header=TRUE)
str(rawdata3) # target변수는 수치형
# 기준점을 잡는 방법: target 변수 분석
par(mfrow=c(1,2),mar=c(5,4,4,2))  # mar=c(하,좌,상,우): 아래에서 시계방향으로
hist(rawdata3$Chance.of.Admit, main='대학원 합격 확률 히스토그램', xlab='대학원 합격 확률', col="red")
boxplot(rawdata3$Chance.of.Admit, main="대학원 합격 확률 box-plot", col="red")
# 히스토그램에서 보면 0.7정도가 빈도수가 가장 높고,
# 박스플롯에서는 중앙값이 0.7이상

summary(rawdata3)
#Chance.of.Admit 
#Median :0.7200  
#Mean   :0.7217

target_median=median(rawdata3$Chance.of.Admit)
target_median    # 0.72

# 이제 기준을 세웠으니까 0.72 이상은 1, 미만은 0으로
# 타겟의 연속형 값 --> 범주형
# rawdata3[행,열] = "0" or "1"
rawdata3[(rawdata3$Chance.of.Admit < target_median), "Chance.of.Admit"] = "0"
rawdata3[(rawdata3$Chance.of.Admit >= target_median), "Chance.of.Admit"] = "1"

# 범주형으로 변환
rawdata3$Chance.of.Admit <- as.factor(rawdata3$Chance.of.Admit)
str(rawdata3)

# unique : 어떤 값들이 있는지(이상치가 있는지 없는지)
unique(rawdata3$Chance.of.Admit)
rawdata3

# 트레이닝 / 테스트 분류
set.seed(2020)
newdata3 <- rawdata3
train_ratio <- 0.7
datatotal3 <- sort(sample(nrow(newdata3), nrow(newdata3)*train_ratio))
train3 <- rawdata3[datatotal3,]
test3 <- rawdata3[-datatotal3,]

#================KNN================

ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)
# k 범위 설정(k 값은 정하기 나름)
customGrid <- expand.grid(k=1:20) 

# 알고리즘을 이용해 데이터학습을 통한 모델 생성 - train()
knnFit <- train(Chance.of.Admit ~ .,
                data = train3,
                method = "knn",
                trControl = ctrl,
                preProcess = c("center", "scale"),  
                #└학습 중에 전처리(전체 데이터가 수치형일 때는 몰아서 가능)
                tuneGrid = customGrid,
                metric = "Accuracy")
knnFit
plot(knnFit)

knn_pred <- predict(knnFit, newdata = test3)
confusionMatrix(knn_pred, test3$Chance.of.Admit)

# 학습 데이터
# Accuracy 0.8929869

# 실제 데이터
# Accuracy 0.8


#================Boosted Logistic================

ctrl <- trainControl(method="repeatedcv", repeats=5)
logitBoostFit <- train(Chance.of.Admit ~ .,
                  data = train3,
                  method = "LogitBoost",
                  trControl = ctrl,
                  preProcess = c("center", "scale"),
                  metric = "Accuracy")
logitBoostFit
plot(logitBoostFit)

logitBoost_pred <- predict(logitBoostFit, newdata=test3)
confusionMatrix(logitBoost_pred, test3$Chance.of.Admit)

# 학습 데이터
# Accuracy 0.8910009

# 실제 데이터
# Accuracy 0.8

#================Penalized Logistic================
# Penalized Logistic Regression : method = 'plr'
# 정규화(Regularization)를 하는 이유
# : 모델의 복잡성(complexity)조절 => 오버피팅(overfitting) 피하기

ctrl <- trainControl(method="repeatedcv", repeats=5)
logitPenalizeFit <- train(Chance.of.Admit ~ .,
                       data = train3,
                       method = "plr",
                       trControl = ctrl,
                       preProcess = c("center", "scale"),
                       metric = "Accuracy")
logitPenalizeFit
plot(logitPenalizeFit)

logitPenalize_pred <- predict(logitPenalizeFit, newdata=test3)
confusionMatrix(logitPenalize_pred, test3$Chance.of.Admit)

# 학습 데이터
# Accuracy 0.8910009

# 실제 데이터
# Accuracy 0.8333

#================Naive bayse================
# Naive Bayse Classification
# : 대학원 합격 여부를 결정하는 각각의 Feature를 독립사건으로 가정하고 조건부 확률을 적용하는 분류 방법

ctrl <- trainControl(method="repeatedcv", repeats=5)
nbFit <- train(Chance.of.Admit ~ .,
               data = train3,
               method = "naive_bayes",
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric = "Accuracy")
nbFit
plot(nbFit)

# 또 봐줘야 하는 개념
# laplace = 0, usekernel = FALSE and adjust = 1.
# 히스토그램 곡선으로 표현해주는게 kernel
# usekernel : kernel 사용여부 (표현했을 때의 정확도가 달라짐.)
# adjust : 대역폭(bandwidth)을 조절하는 것.(대역폭에 따라서도 정확도가 달라짐)
#          usekernel이 FALSE인 경우에는 adjust가 소용없다.
#         -> 커널을 사용했을 때의 bandwidth를 조절하는 거니까

# 결과 해석 : 
# usekernel - 커널 밀도함수에 대한 사용여부
# adjust - 커널 밀도함수의 bandwidth 값 조절
# laplace - 데이터의 수가 적을 경우(전체 시도횟수가 적을 경우) 0 또는 1과 같은 극단적인 값으로 추정되는 것을 막기 위한 값

nb_pred <- predict(nbFit, newdata=test3)
confusionMatrix(nb_pred, test3$Chance.of.Admit)


# 학습 데이터
# Accuracy 0.8910009

# 실제 데이터
# Accuracy 0.8267

#================Random Forest================

ctrl <- trainControl(method="repeatedcv", repeats=5)
rfFit <- train(Chance.of.Admit ~ .,
               data=train3,
               method="rf",
               trControl=ctrl,
               preProcess=c("center","scale"),
               metric="Accuracy")
rfFit
plot(rfFit)

# 결과 해석:
# mtry - 각 트리에서 랜덤하게 선택되는 분할 피쳐 개수
# tree의 길이(depth)는 작은게 좋다.-> 모델이 복잡하지 않다.
# 예를들어, 배드민턴 치는 유무에서 날씨 테스트를 했는데 바로 구분이 되지 않아서 
# 두번째 테스트로 바람 테스트를 했다. => mtry = 2

rf_pred <- predict(rfFit, newdata=test3)
confusionMatrix(rf_pred, test3$Chance.of.Admit)

# 학습 데이터
# Accuracy 0.8772904

# 실제 데이터
# Accuracy 0.8067

#================서포트 벡터 머신================

ctrl <- trainControl(method="repeatedcv", repeats=5)
svmLinearFit <- train(Chance.of.Admit ~ .,
                        data=train3,
                        method="svmLinear",
                        trControl=ctrl,
                        preProcess=c("center", "scale"),
                        metric="Accuracy")
svmLinearFit
plot(svmLinearFit)

svmLinearFit_pred <- predict(svmLinearFit, newdata=test3)
confusionMatrix(svmLinearFit_pred, test3$Chance.of.Admit)

# 학습 데이터
# Accuracy 0.8895341

# 실제 데이터
# Accuracy 0.8267

#================커널 서포트 벡터 머신(비선형 서포트 벡터 머신)================

ctrl <- trainControl(method="repeatedcv", repeats=5)
svmPolyFit <- train(Chance.of.Admit ~ .,
                      data=train3,
                      method="svmPoly",
                      trControl=ctrl,
                      preProcess=c("center","scale"),
                      metric="Accuracy")
svmPolyFit
plot(svmPolyFit)

svmPoly_pred <- predict(svmPolyFit, newdata=test3)
confusionMatrix(svmPoly_pred, test3$Chance.of.Admit)

# 학습 데이터
# Accuracy 0.8945808

# 실제 데이터
# Accuracy 0.8133


# --------------------------------------------------

# =====================knn=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8929869   <->      0.8

# =====================Boosted Logistic=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8910009   <->      0.8

# =====================Penalized Logistic=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8910009   <->     0.8333

# =====================Naive bayse=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8910009   <->     0.8267

# =====================Random Forest=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8772904   <->     0.8067

# =====================SVM(선형 SVM)=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8895341   <->     0.8267

# =====================커널SVM(비선형 SVM)=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8945808   <->     0.8133
