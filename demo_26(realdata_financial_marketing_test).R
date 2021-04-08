# financial_marketing.csv 데이터 분석하기
# 일단 target이 있기 때문에 => 지도학습
# 피쳐의 개수는 몇개인지
# 피쳐의 값은 데이터형태가 어떻게 되는지(범주형인지 수치형인지)
# 수치형이 아니기 때문에 통계를 낼 수 없음. 문자를 어떻게 처리할지
# 결측치가 있는지, 이상치가 있는지
# 문자형 데이터에 결측치가 나왔을 때는 최빈값으로 처리
# 표준화 작업 (그동안은 숫자였지만, 지금은 문자니까 각각 따로 scale)

library(caret)
rawdata <- read.csv(file="financial_marketing.csv", header=TRUE)
str(rawdata)

# 결측치 확인(있으면 na.omit() 사용해서 제거해버리기)
sum(is.na(rawdata$age))
sum(is.na(rawdata$job))
sum(is.na(rawdata$marital))
sum(is.na(rawdata$education))
sum(is.na(rawdata$default))
sum(is.na(rawdata$housing))
sum(is.na(rawdata$loan))
sum(is.na(rawdata$contact))
sum(is.na(rawdata$month))
sum(is.na(rawdata$day_of_week))
sum(is.na(rawdata$duration))
sum(is.na(rawdata$campaign))
sum(is.na(rawdata$previous))
sum(is.na(rawdata$poutcome))
sum(is.na(rawdata$emp.var.rate))
sum(is.na(rawdata$cons.price.idx))
sum(is.na(rawdata$cons.conf.idx))
sum(is.na(rawdata$euribor3m))
sum(is.na(rawdata$nr.employed))
sum(is.na(rawdata$target))

# 이상치 확인
unique(rawdata$age)
unique(rawdata$job)   # "unknown" -> 결측치(비어있는게 아니라 문자로 써져있는 이상한 것들 잡아내야 함.)
unique(rawdata$marital)   # "unknown"
unique(rawdata$education)   # "unknown"
unique(rawdata$default)   # "unknown"
unique(rawdata$housing)   # "unknown"
unique(rawdata$loan)   # "unknown"
unique(rawdata$contact)
unique(rawdata$month)
unique(rawdata$day_of_week)
unique(rawdata$duration)
unique(rawdata$campaign)
unique(rawdata$previous)
unique(rawdata$poutcome)   # "nonexistent" 이런것도 한번은 더 생각해보고
unique(rawdata$emp.var.rate)
unique(rawdata$cons.price.idx)
unique(rawdata$cons.conf.idx)
unique(rawdata$euribor3m)
unique(rawdata$nr.employed)
unique(rawdata$target)

# unknown -> NA
rawdata[rawdata=="unknown"] <- NA

# NA total
sum(is.na(rawdata))
str(rawdata)  # 600개

# NA omit
rawdata <- na.omit(rawdata)
str(rawdata)  # 456개

# 수치형 변수별 histogram(통계량)
par(mfrow=c(3,3), mar=c(5,4,4,2))
hist(rawdata$age, main="age histogram", xlab="age", col="red")
hist(rawdata$duration, main="duration histogram", xlab="duration(time)", col="orange")
hist(rawdata$campaign, main="campaign histogram", xlab="campaign", col="yellow")
hist(rawdata$previous, main="previous histogram", xlab="previous", col="green")
hist(rawdata$emp.var.rate, main="emp.var.rate histogram", xlab="emp.var.rate", col="blue")
hist(rawdata$cons.price.idx, main="cons.price.idx histogram", xlab="cons.price.idx", col="navy")
hist(rawdata$cons.conf.idx, main="cons.conf.idx histogram", xlab="cons.conf.idx", col="purple")
hist(rawdata$euribor3m, main="euribor3m histogram", xlab="euribor3m", col="gray")
hist(rawdata$nr.employed, main="nr.employed histogram", xlab="nr.employed", col="khaki")
# -> 단위가 다 다르니까 표준화 필요하다는걸 느껴야 함.

# Scale(표준화)
numVar <- c("age", "duration", "campaign", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")
rawdata[,numVar] = lapply(rawdata[,numVar], scale)
str(rawdata)

# histogram after scale
par(mfrow=c(3,3), mar=c(5,4,4,2))
hist(rawdata$age, main="age histogram", xlab="age", col="red")
hist(rawdata$duration, main="duration histogram", xlab="duration(time)", col="orange")
hist(rawdata$campaign, main="campaign histogram", xlab="campaign", col="yellow")
hist(rawdata$previous, main="previous histogram", xlab="previous", col="green")
hist(rawdata$emp.var.rate, main="emp.var.rate histogram", xlab="emp.var.rate", col="blue")
hist(rawdata$cons.price.idx, main="cons.price.idx histogram", xlab="cons.price.idx", col="navy")
hist(rawdata$cons.conf.idx, main="cons.conf.idx histogram", xlab="cons.conf.idx", col="purple")
hist(rawdata$euribor3m, main="euribor3m histogram", xlab="euribor3m", col="gray")
hist(rawdata$nr.employed, main="nr.employed histogram", xlab="nr.employed", col="khaki")
# -> 1 단위로 바뀜.

## Categorical(Factor) Data
# barchart
par(mfrow=c(4,3), mar=c(0.5,0.5,0.5,0.5))
prop.table(table(rawdata$job)) # 직업에 대한 비율
barplot(prop.table(table(rawdata$job)), main="job proportion")
barplot(prop.table(table(rawdata$marital)), main="marital proportion")
barplot(prop.table(table(rawdata$education)), main="education proportion")
barplot(prop.table(table(rawdata$default)), main="default proportion")
barplot(prop.table(table(rawdata$housing)), main="housing proportion")
barplot(prop.table(table(rawdata$loan)), main="loan proportion")
barplot(prop.table(table(rawdata$contact)), main="contact proportion")
barplot(prop.table(table(rawdata$month)), main="month proportion")
barplot(prop.table(table(rawdata$day_of_week)), main="day_of_week proportion")
barplot(prop.table(table(rawdata$poutcome)), main="poutcome proportion")

# piechart
par(mfrow=c(4,3), mar=c(0.5,0.5,0.5,0.5))
pie(prop.table(table(rawdata$job)), main="job proportion")
pie(prop.table(table(rawdata$marital)), main="marital proportion")
pie(prop.table(table(rawdata$education)), main="education proportion")
pie(prop.table(table(rawdata$default)), main="default proportion")
pie(prop.table(table(rawdata$housing)), main="housing proportion")
pie(prop.table(table(rawdata$loan)), main="loan proportion")
pie(prop.table(table(rawdata$contact)), main="contact proportion")
pie(prop.table(table(rawdata$month)), main="month proportion")
pie(prop.table(table(rawdata$day_of_week)), main="day_of_week proportion")
pie(prop.table(table(rawdata$poutcome)), main="poutcome proportion")

# 데이터 전체를 한번에 파악할 수 있는 방법은 없을까? 
# => 차원 축소 - 주성분(PCA) 분석 : 데이터의 주요한 부분 표현
# 차원축소를 하면 데이터를 100퍼센트 표현하진 못하지만 우리는 보면서 그 데이터 특성(주성분)은 파악할 수 있다.
# 우리가 그릴 수 있는 최대 차원은 3차원까지
# 퍼진 정도가 가장 많은(분산값이 가장 높은) 순서대로 주성분을 구할 수 있다.

# target 변수 factor형으로 변환
rawdata$target <- as.factor(rawdata$target)
str(rawdata)
# 범주형 독립변수는 범주화 시켜줘야 함.
factVar <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome")
rawdata[,factVar] = lapply(rawdata[,factVar], factor)
str(rawdata)


# 트레이닝 / 테스트 데이터 나누기
set.seed(2020)
newdata <- rawdata

# default 열 삭제하기
# newdata <- subset(newdata, select=-default)

train_ratio <- 0.7
datatotal <- sort(sample(nrow(newdata), nrow(newdata)*train_ratio))

train <- newdata[datatotal,]
test <- newdata[-datatotal,]

#================KNN(knn)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
# k 범위 설정(k 값은 정하기 나름)
customGrid <- expand.grid(k=1:20) 

# 알고리즘을 이용해 데이터학습을 통한 모델 생성 - train()
knn_fit <- train(target ~ .,
                 data = train,
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid = customGrid,
                 metric = "Accuracy")
knn_fit

knn_pred <- predict(knn_fit, newdata = test)
confusionMatrix(knn_pred, test$target)

# 학습 데이터
# Accuracy : 0.8527413

# 실제 데이터
# Accuracy : 0.7883

#================로지스틱 회귀(glm)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
logit_fit <- train(target ~ .,
                   data = train,
                   method = "glm",
                   trControl = ctrl,
                   metric = "Accuracy")
logit_fit

logit_pred <- predict(logit_fit, newdata = test)
confusionMatrix(logit_pred, test$target)

# 학습 데이터
# Accuracy : 0.8406555

# 실제 데이터
# Accuracy : 0.8029

#================Boosted 로지스틱(LogitBoost)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
logit_boost_fit <- train(target ~ .,
                         data = train,
                         method = "LogitBoost",
                         trControl = ctrl,
                         metric = "Accuracy")
logit_boost_fit
plot(logit_boost_fit)

logit_boost_pred <- predict(logit_boost_fit, newdata=test)
confusionMatrix(logit_boost_pred, test$target)

# 학습 데이터
# Accuracy : 0.8703635

# 실제 데이터
# Accuracy : 0.8029

#================로지스틱 모형 트리(LMT)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
logit_lmt_fit <- train(target ~ .,
                       data = train,
                       method = "LMT",
                       trControl = ctrl,
                       metric = "Accuracy")
logit_lmt_fit

logit_lmt_pred <- predict(logit_lmt_fit, newdata=test)
confusionMatrix(logit_lmt_pred, test$target)

# 학습 데이터
# Accuracy : 0.8770400

# 실제 데이터
# Accuracy : 0.8248

#================Penalized 로지스틱(plr)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
logit_plr_fit <- train(target ~ .,
                       data = train,
                       method = "plr",
                       trControl = ctrl,
                       metric = "Accuracy")
logit_plr_fit

logit_plr_pred <- predict(logit_plr_fit, newdata=test)
confusionMatrix(logit_plr_pred, test$target)

# 학습 데이터
# Accuracy : 0.8684745

# 실제 데이터
# Accuracy : 0.8321

#================Regularized 로지스틱(regLogistic)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
logit_reg_fit <- train(target ~ .,
                       data = train,
                       method = "regLogistic",
                       trControl = ctrl,
                       metric = "Accuracy")
logit_reg_fit

logit_reg_pred <- predict(logit_reg_fit, newdata=test)
confusionMatrix(logit_reg_pred, test$target)

# 학습 데이터
# Accuracy : 0.8802108

# 실제 데이터
# Accuracy : 0.8394

#================Naive bayse(naive_bayes)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
nb_fit <- train(target ~ .,
                data = train,
                method = "naive_bayes",
                trControl = ctrl,
                metric = "Accuracy")
nb_fit

nb_pred <- predict(nb_fit, newdata=test)
confusionMatrix(nb_pred, test$target)

# 학습 데이터
# Accuracy : 0.7741257

# 실제 데이터
# Accuracy : 0.708

#================Random Forest(rf)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
rf_fit <- train(target ~ .,
                data = train,
                method = "rf", 
                trControl = ctrl,
                metric = "Accuracy")
rf_fit

rf_pred <- predict(rf_fit, newdata=test)
confusionMatrix(rf_pred, test$target)

# 학습 데이터
# Accuracy : 0.8581696

# 실제 데이터
# Accuracy : 0.8613

#================서포트벡터머신(svmLinear)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
svm_linear_fit <- train(target ~ .,
                        data = train,
                        method = "svmLinear",
                        trControl = ctrl,
                        metric = "Accuracy")
svm_linear_fit

svm_linear_pred <- predict(svm_linear_fit, newdata=test)
confusionMatrix(svm_linear_pred, test$target)

# 학습 데이터
# Accuracy : 0.8558095

# 실제 데이터
# Accuracy : 0.8467

#================커널 서포트벡터머신(svmPoly)================
ctrl <- trainControl(method="repeatedcv", repeats=5)
svm_poly_fit <- train(target ~ .,
                      data = train,
                      method = "svmPoly",
                      trControl = ctrl,
                      metric = "Accuracy")
svm_poly_fit

svm_poly_pred <- predict(svm_poly_fit, newdata=test)
confusionMatrix(svm_linear_pred, test$target)

# 학습 데이터
# Accuracy : 0.8796004

# 실제 데이터
# Accuracy : 0.8467

# --------------------------------------------------

# =====================knn=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8527413   <->      0.7883

# =====================Logistic=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8406555   <->      0.8029

# =====================Boosted Logistic=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8703635   <->      0.8029

# =====================LMT=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8770400   <->     0.8248

# =====================Penalized Logistic=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8684745   <->     0.8321

# =====================Regularized Logistic=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8802108   <->     0.8394

# =====================Naive bayse=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.7741257   <->     0.708

# =====================Random Forest=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8581696   <->     0.8613

# =====================SVM(선형 SVM)=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8558095   <->     0.8467

# =====================커널SVM(비선형 SVM)=====
#             학습 데이터  <->  테스트 데이터
# Accuracy :   0.8796004   <->     0.8467