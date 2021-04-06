library(caret)

rawdata <- read.csv(file="wine.csv", header=TRUE)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

analdata <- rawdata
set.seed(2020)

datatotal <- sort(sample(nrow(analdata), nrow(analdata)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
str(train)

# Decision Tree 학습
install.packages("tree")
library(tree)
treeRaw <- tree(Class~.,
                data=train)

# 기본트리 확인
plot(treeRaw)
# 어떠한 피쳐를 가지고 있는지
text(treeRaw)
# 왼쪽이 예, 오른쪽이 아니오

# 가장 적합한 사이즈 찾기(Tree Size)
cv_tree <- cv.tree(treeRaw, FUN=prune.misclass)
plot(cv_tree)
# 잘못분류된 오류율
# size -> 트리사이즈 == depth라고 생각하는게 더 이해하기 좋다. 
# 트리사이즈가 1개일때 오류율 겁나 높음.
# 트리사이즈가 4개일때부터는 오류율 변동이 없음
# 그럼 적합한 트리사이즈는 4개 -> 4일 때 가지치기 하면 된다.
# => 4를 찾기위한 과정

# 가지치기(가지가 너무 많으면 성능상의 문제)
# prune.misclass : 가지치기
prune_tree <- prune.misclass(treeRaw, best=4) # best 값은 cv에서 측정된 값을 적용
plot(prune_tree)
text(prune_tree, pretty=0)

# 예측
pred <- predict(prune_tree, test, type='class') 
# 확률값 예측할 때는 prob, 분류 나타낼 때는 class
confusionMatrix(pred, test$Class)  # 정확도 85프로

# 의사결정트리는 오버핏이 많이 발생
# 오버핏 발생을 줄이기 위해 랜덤포레스트(앙상블 방법)를 사용한다. => 내일
# 수학적으로 계산하는 것도 부담이 없고 사람들이 많이 사용함.

# 학습했을때의 정확도와 테스트데이터로 했을 때의 정확도의의 차이가 많이 난다는 것은 오버핏 발생 확률이 높다고 보면 됨.

# ----------201110 Random Forest 추가-----------

# Random Forest 학습
ctrl <- trainControl(method="repeatedcv", repeats=5)
rfFit <- train(Class ~ .,
               data=train,
               method="rf",
               trControl=ctrl,
               preProcess=c("center","scale"), # 숫자니까 가능하지 factor형이 있으면 이렇게 처리 못함.
               metric="Accuracy")
rfFit
#   mtry  Accuracy   Kappa    
#    2    0.9916017  0.9873198
#    7    0.9856677  0.9783898
#   13    0.9481069  0.9209296
# mtry가 2인 경우 정확도가 가장 높다. 
# 피쳐후보개수가 2개일 때(피쳐개수가 많을수록 나무 길이가 길어진다)
plot(rfFit)

# 예측
pred_test <- predict(rfFit, newdata=test)
confusionMatrix(pred_test, test$Class)
# => 실제 데이터를 넣었을 때 정확도 98프로

# 의사결정트리로 했을 때의 정확도 85프로, 랜덤포레스트를 했더니 98프로

# 변수의 중요도
importance_rf <- varImp(rfFit, scale=FALSE)
plot(importance_rf)
