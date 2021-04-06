library(caret)

rawdata <- read.csv(file="wine.csv", header=TRUE)

# 범주형 데이터 변환
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

# 트레이닝데이터 - 테스트데이터 나누기
analdata <- rawdata
set.seed(2020)
datatotal <- sort(sample(nrow(analdata), nrow(analdata)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]

str(train)

ctrl <- trainControl(method="repeatedcv", repeats=5)
nbFit <- train(Class ~ .,
               data = train,
               method = "naive_bayes",
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric = "Accuracy")
nbFit
# usekernel이 FALSE일 때 정확도 98프로
# 해당 커널이 펄스일때 정확도가 더 높았다. 그래서 얘를 채택했다
# kernel - 부드러운 곡선으로 표현(적용하면)
# usekernel은 사용하고 안하고에 있어서 정확도 차이가 있었다.
# bandwidth를 조절하는 방법 => adjust
# adjust 적용했냐 안했냐 (커널이 적용안됐으면 의미없음.)
# laplace(라플라스) - α값을 더해주기
# 왜 더해주냐면, 만약에 x/N(전체 확률값)이 0이 나왔을 때는
# 다른 값들에게도 영향을 줄 수 있음 (극단값이 나와버릴 수 있다.) 
# 극단값을 피하기 위해서 알파값을 더해준다 => 라플라스
# 라플라스 값을 설정했느냐 안했느냐(여기서는 설정X)

plot(nbFit)

# 예측모델에 테스트데이터 적용
pred_test <- predict(nbFit, newdata=test)
confusionMatrix(pred_test, test$Class)
# 정확도 94프로, knn이랑 거의 비슷한 결과가 나온다.

# 변수의 중요도 확인
importance_nb <- varImp(nbFit, scale=FALSE)
plot(importance_nb)

# 각각의 피쳐에 대해서 ROC 커브값을 적용 -> ? 이해못함

## naive bayes 결과 해석하기

# 커널 밀도추정(kerneal Density Estimation)
# 데이터의 히스토그램을 이용하여 실제 분포를 추정하는 방식
# 히스토그램을 부드러운(Smooting) 곡선으로 표현함.

# usekernel은 kde를 사용했는지를 의미함.

# 커널밀도의 높이(세로)값의 범위를 bandwidth로 보고 
# 그 값이 달라지면 커널밀도추정(kde) 함수의 형태가 달라진다. 

# adjust => bandwidth를 조정한다는 의미

## 라플라스 스무딩(Laplace Smoothing or Additive Smoothing)

# 데이터 수가 적을 경우, 0 또는 1과 같이 극단적인 값으로 추정되는 것을 방지하는 것

# x가 나온 횟수 + 알파
# -----------------------  => 여기서 알파값은 x가 나온 횟수가 0일 경우를 차단하기 위한 값
# 전체 시행 횟수 + 알파
# -> 0이랑 0.1은 엄청 큰 차이가 있다.
