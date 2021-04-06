# Caret 패키지 : 머신러닝을 쉽게 할 수 있도록 도와주는 패키지
install.packages("caret", dependencies = TRUE)  #의존성 패키지
library(caret)

rawdata <- read.csv(file="wine.csv", header=TRUE)
# as.factor() : 범주형 데이터로 바꾸는 함수
rawdata$Class <- as.factor(rawdata$Class) #범주형으로 변환

# 구조 파악
str(rawdata)  # Class값이 범주형으로 바꼈다.

## 트레이닝 - 테스트 데이터로 분할하기
analdata <- rawdata

set.seed(2020)

# sample(a,b) : 1부터 a까지의 숫자 중에 b개를 추출
# nrow(): 데이터의 행(row)수
# analdata에서 70% 데이터를 추출해오고 그걸 정렬(기본값:오름차순)
datatotal <- sort(sample(nrow(analdata), nrow(analdata)*0.7))
datatotal  # 랜덤한 값

train <- rawdata[datatotal,]
test <- rawdata[-datatotal,] # -붙이면 반대니까 나머지 30%

train_x <- train[,1:13] # class는 빼고
train_y <- train[,14]

test_x <- test[,1:13]
test_y <- test[,14] # 요렇게 4줄이 없어도 돌아가긴 하지만 필요할 때도 있다.

# 파라미터 설정
# 교차검증을 반복적으로 할건지, 교차검증방법으로 어떤 걸 선택할건지, k-fold의 경우 각각의 영역을 나눠줘야 하는데 그 영역을 몇개로 나눌건지 등등
## 데이터 훈련 과정의 파라미터 설정하기 - trainControl()
ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)
# method="repeatedcv" : cross-validation 반복
# number=10 : 훈련 데이터의 fold 갯수
# repeat=5 : cross-validation 반복 횟수
# -> 10-fold cross validation 을 5번 반복하겠다는 소리

# k 범위 설정
customGrid <- expand.grid(k=1:10)

# 알고리즘을 이용해 데이터학습을 통한 모델 생성 - train()
knnFit <- train(
            Class ~ ., # ~ . :feature 모두 사용하겠다.
            data = train,
            method = "knn",  # 사용하려는 알고리즘
            trControl = ctrl,  # 학습 방법 설정
            preProcess = c("center", "scale"),  # 데이터 전처리, 표준화
            tuneGrid = customGrid,   # k 값
            metric = "Accuracy")  # 모형 평가 방식
knnFit
# k를 1부터10까지 정해놓고 정확도를 보니까
# Accuracy 와 Kappa 비슷한 정확도를 나타냄
# 정확도 제일 높은 거 k=5, 97.7%
# 이 모형은 k값이 5일 때 가장 정확한 값을 나타내는 모형

# 수치로 보면 잘 모르기 때문에 꼭 시각화시켜줘야함
plot(knnFit)

## 테스트 데이터로 최종 모형 평가 - predict()
pred_test <- predict(knnFit, newdata = test)
confusionMatrix(pred_test, test$Class) #분할표(예측값, 실제값)
# 분할표 보면 14개 모두 맞춤, 24개 맞추고 3개 실패, 13개 모두 맞춤

# feature들 중 누가 가장 중요한 역할을 했는지
# 변수의 중요도
importance_knn <- varImp(knnFit, scale=FALSE)
plot(importance_knn)
# X1, X2, X3 각각 클래스의 중요한 역할이 된 변수 확인
