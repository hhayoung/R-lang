library(caret)

rawdata <- read.csv(file="heart.csv", header=TRUE)
str(rawdata)

#target: 심장병 유무
#cp : 통증 타입
#trestbps : 혈압
#chol : 콜레스테롤
#fbs : 공복혈당 > 120
#restecg : 심전도 결과
#thalach : 심박수
#exang : 협심증
#oldpeak : 운동에 의해 저하되는 ST
#slope : ST 분절 기울기
#ca : 형광투시법에 의해 착색되는 주요 혈관 수
#thal : 결함 유무

# 타겟(정답, class, label) 변수의 범주화
rawdata$target <- as.factor(rawdata$target)
str(rawdata)

# 연속형 독립변수에 대해서는 표준화를 한다. 
#rawdata$age <- scale(rawdata$age)
#rawdata$trestbps <- scale(rawdata$trestbps)
#rawdata$chol <- scale(rawdata$chol)
#rawdata$thalach <- scale(rawdata$thalach)
#rawdata$oldpeak <- scale(rawdata$oldpeak)
#rawdata$slope <- scale(rawdata$slope)
numVar <- c("age", "trestbps", "chol", "thalach", "oldpeak", "slope")
rawdata[,numVar] = lapply(newdata[,numVar], scale)
str(rawdata)

# 범주형 독립변수는 범주화 시켜줘야 함.
newdata <- rawdata
factVar <- c("sex", "cp", "fbs", "restecg", "exang", "ca", "thal")

# factVar를 한번에 lapply함수를 이용하여 바꿔주기
# lapply(x, mean) : x(리스트, 데이터프레임)에 mean함수를 적용한 결과를 리턴한다. 
newdata[,factVar] = lapply(newdata[,factVar], factor)


# 트레이닝 - 테스트 데이터 분할
set.seed(2020)
datatotal <- sort(sample(nrow(newdata), nrow(newdata)*0.7))
train <- newdata[datatotal,]
test <- newdata[-datatotal,]

# 학습모델 생성

# 로지스틱 회귀분석 알고리즘들
# "LogitBoost", "LMT", "plr", "regLogistic"
# -> method값을 바꿔서 테스트 해볼 때 선택은 1로
# 4개의 분석방법이 비슷한 결과가 나왔다.
ctrl <- trainControl(method="repeatedcv", repeats=5)
logitFit <- train(target ~ .,
                  data = train,
                  method = "LogitBoost",
                  trControl = ctrl,
                  #preProcess = c("center", "scale"),
                  metric = "Accuracy")
logitFit
# 기본으로 fold 10번 정해지고 반복 5번
# 보통 Accuracy, Kappa 많이 사용됨.
# nIter : 반복횟수
# 21번 반복했을 때가 가장 정확했다. (82퍼센트)
plot(logitFit)

# 모델을 이용한 예측
pred_test <- predict(logitFit, newdata = test)
confusionMatrix(pred_test, test$target)
# 분할표에서 맞춘 정답은 대각선(위에서 아래로)
# 정확도는 75퍼센트

# 트레이닝 데이터로는 82퍼였는데, 테스트데이터로 해보니까 75퍼센트
# 데이터가 많을수록 정확도가 올라가게 된다.

# 변수의 중요도
# : 심장병을 예측하는데 있어서 어떤 변수가 큰 영향을 미쳤을까
importance_logit <- varImp(logitFit, scale=FALSE)#scale=TRUE로 해도 큰 영향 미치지 않는다고 알려져있음.
plot(importance_logit)
# => 어떤 통증타입(cp)이냐에 따라서 심장병을 판단하는데 있어서 중요한 기준이 된다. 그 다음으로는 thalach(심박수)
# fbs : 공복혈당과 심장병은 아무런 관련이 없다고 판단해도 된다.


