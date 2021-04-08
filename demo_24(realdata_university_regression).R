# 랜덤포레스트, SVM(선형,비선형) 2개 알고리즘 가지고 테스트해보기.
# 학습 데이터에 나왔던 결과와 테스트데이터로 나오는 결과와 비교하기

library(caret)

rawdata2 <- read.csv(file="university.csv", header=TRUE)

set.seed(2020)
newdata <- rawdata2
datatotal <- sort(sample(nrow(newdata), nrow(newdata)*.7))

train <- rawdata2[datatotal,]
test <- rawdata2[-datatotal,]

#-------------random forest---------------

ctrl <- trainControl(method="repeatedcv", repeats=5)
rfFit <- train(Chance.of.Admit ~ .,
               data=train,
               method="rf",
               trControl=ctrl,
               preProcess=c("center","scale"),
               metric="RMSE")
rfFit

# 예측
rfFit_pred <- predict(rfFit, newdata=test)
# RMSE값을 계산하는 함수
postResample(pred=rfFit_pred, obs=test$Chance.of.Admit)


#-------------svm(선형)---------------

ctrl <- trainControl(method="repeatedcv", repeats=5)
svmLinearFit <- train(Chance.of.Admit ~ .,
                      data=train,
                      method="svmLinear",
                      trControl=ctrl,
                      preProcess=c("center", "scale"),
                      metric="RMSE")
svmLinearFit

# 예측
svmLinearFit_pred <- predict(svmLinearFit, newdata=test)
# RMSE값을 계산하는 함수
postResample(pred=svmLinearFit_pred, obs=test$Chance.of.Admit)

#-------------svm(비선형(=kernal, 커널)---------------

ctrl <- trainControl(method="repeatedcv", repeats=5)
svmPolyFit <- train(Chance.of.Admit ~ .,
                    data=train,
                    method="svmPoly",
                    trControl=ctrl,
                    preProcess=c("center","scale"),
                    metric="RMSE")
svmPolyFit

# 예측
svmPolyFit_pred <- predict(svmPolyFit, newdata=test)
# RMSE값을 계산하는 함수
postResample(pred=svmPolyFit_pred, obs=test$Chance.of.Admit)

#-------------로짓회귀---------------

ctrl <- trainControl(method="repeatedcv", repeats=5)
logistic_fit <- train(Chance.of.Admit ~ .,
                      data=train,
                      method="glm",
                      trControl=ctrl,
                      preProcess=c("center","scale"),
                      metric="RMSE")
logistic_fit

# 예측
logistic_pred <- predict(logistic_fit, newdata=test)
# RMSE값을 계산하는 함수
postResample(pred=logistic_pred, obs=test$Chance.of.Admit)

#-------------엘라스틱넷---------------

ctrl <- trainControl(method="repeatedcv", repeats=5)
logit_penal_fit <- train(Chance.of.Admit ~ .,
                         data=train,
                         method="glmnet",
                         trControl=ctrl,
                         preProcess=c("center","scale"),
                         matric="RMSE")
logit_penal_fit

# 예측
logit_penal_pred <- predict(logit_penal_fit, newdata=test)
# RMSE값을 계산하는 함수
postResample(pred=logit_penal_pred, obs=test$Chance.of.Admit)

# --------------------------------------------------

# 테스트 데이터를 넣어서 해봤을 때의 결과

# =====================random forest=====
# ---- 예측(학습 데이터)
#mtry        RMSE   Rsquared         MAE       
#2     0.06314975  0.8112020  0.04475650

# ---- 테스트데이터 넣었을 때
#      RMSE   Rsquared        MAE 
#0.05783150 0.81633086 0.04130344

# =====================svm(선형)=====
# ---- 예측(학습 데이터)
#      RMSE   Rsquared         MAE       
#0.06175957  0.8235632  0.04294417

# ---- 테스트데이터 넣었을 때
#      RMSE   Rsquared        MAE 
#0.05871683 0.82091845 0.04052400

# =====================svm(비선형=커널)=====
# ---- 예측(학습 데이터)
#degree  scale  C     RMSE        Rsquared   MAE
#2       0.100  0.50  0.06050000  0.8247851  0.04204156

# ---- 테스트데이터 넣었을 때
#      RMSE   Rsquared        MAE 
#0.05836868 0.82317817 0.04074317

# =====================로짓회귀=====
# ---- 예측(학습 데이터)
#     RMSE   Rsquared         MAE       
#0.0612547  0.8210424  0.04443425

# ---- 테스트데이터 넣었을 때
#      RMSE   Rsquared        MAE 
#0.05797190 0.81681161 0.04105176

# =====================엘라스틱넷=====
# ---- 예측(학습 데이터)
#alpha  lambda        RMSE        Rsquared   MAE
#0.10   0.0025481481  0.06104364  0.8214952  0.04433596

# ---- 테스트데이터 넣었을 때
#      RMSE   Rsquared        MAE 
#0.05759752 0.81881695 0.04075955
