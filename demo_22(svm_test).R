library(caret)

rawdata <- read.csv(file="wine.csv", header=TRUE)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

# 테스트 데이터 분할
analdata <- rawdata
set.seed(2020)
datatotal <- sort(sample(nrow(analdata), nrow(analdata)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
str(train) # 124개 추출

# 선형 서포트 벡터 머신
ctrl <- trainControl(method="repeatedcv", repeats=5)
svm_linear_fit <- train(Class ~ .,
                        data=train,
                        method="svmLinear",
                        trControl=ctrl,
                        preProcess=c("center", "scale"),
                        metric="Accuracy")
svm_linear_fit  # 정확도 97프로

# 예측
pred_test <- predict(svm_linear_fit, newdata=test)
confusionMatrix(pred_test, test$Class)   # 정확도 94프로

# 변수의 중요도
importance_linear <- varImp(svm_linear_fit, scale=FALSE)
plot(importance_linear)

# 비선형 서포트 벡터 머신
ctrl <- trainControl(method="repeatedcv", repeats=5)
svm_poly_fit <- train(Class ~ .,
                      data=train,
                      method="svmPoly",
                      trControl=ctrl,
                      preProcess=c("center","scale"),
                      metric="Accuracy")
svm_poly_fit   # 정확도 99프로
# degree=1, scale=0.01 and c=0.5  수식에 들어가있는 값

# 예측
pred_test <- predict(svm_poly_fit, newdata=test)
confusionMatrix(pred_test, test$Class)  # 정확도 92프로

# 변수의 중요도
importance_poly <- varImp(svm_poly_fit, scale=FALSE)
plot(importance_poly)

# => 학습했을 때의 정확도와 실제 데이터를 넣었을 때의 정확도 차이가 더 심하다.
# 선형 모델보다 비선형 모델이 좀 더 오버핏이 들어갔다.
# 이 알고리즘이 그렇다는게 아니라 현재 데이터로 돌렸을 때 지금 결과가 그렇다.