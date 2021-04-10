# financial_marketing 파일을 2차원으로 차원축소하기

rawdata <- read.csv('financial_marketing.csv', header=TRUE)
str(rawdata)

# unknown -> NA
rawdata[rawdata=="unknown"] <- NA
# NA total
sum(is.na(rawdata))
str(rawdata)  # 600개
# NA omit
rawdata <- na.omit(rawdata)
str(rawdata)  # 456개

# Scale(표준화)
# numVar <- c("age", "duration", "campaign", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")
# rawdata[,numVar] = lapply(rawdata[,numVar], scale)
# str(rawdata)


# factor_var <- c("job","marital","education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome", "target")
# rawdata[,factor_var] = lapply(rawdata[,factor_var], factor)



# 주성분 분석
numVar <- c("age", "duration", "campaign", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")

tar <- rawdata[,"target"]
num_data <- rawdata[,numVar]
# pca_num <- prcomp(num_data) # scale을 위에서 해 준 경우
pca_num <- prcomp(num_data, center=T, scale=T) # scale을 따로 안 해준 경우
# center = T는 중앙을 0으로, scale=T 는 분산을 1로
plot(pca_num, type="l", main="Principle Component Analysis")

summary(pca_num)

# 수정 전 코드
# pca_matrix <- pca_num$rotation
# pca_data <- as.matrix(num_data) %*% pca_matrix  # as.matrix() : 매트릭스로 변환
# dim(pca_data)
# library(ggfortify)
# autoplot(pca_num, data=rawdata, colour="target")
"
--> 저 수정 전 코드는 잘못된 코드였었음.
일단은 주성분 값(pca_data)를 구하려는 거였는데, 그냥 pca_num$x 가 바로 주성분 값이어서 그냥 써먹으면 되는 거라서 굳이 계산할 필요도 없었음. 
       pca_num$x = as.matrix(num_data) %*% pca_num$rotation
근데 일단 계산해보려고 했었는데 바꾼 데이터의 값을 가져오는게 아니라 원래의 데이터의 값을 뿌려주는 코드였다. (잘못된 변수를 넣었었다고 함. 아마 autoplot(data=rawdata) 여기 부분인 것 같음.)
그래서 제대로 된 값을 가져오려면 할 때, 
ggfortify를 이용하게 되면 좀 더 설정해줘야 하는게 있는데, 그게 복잡해서 교수님이 에러나서 ggplot으로 그냥 한 것.

여기서는 주성분의 값을 구하기 위해 곱셈을 하려고 했던 거고,
이미지 실습코드에서는 
주성분의 값과 아이겐벡터의 값을 곱하는 식이 원래 이미지로 다시 만들 수 있는 식이기 때문에 이 때는 꼭 행렬곱을 해줘야 한다. 
    	compressed.img <- j$x %*% t(j$rotation)
"

# 수정 후 코드
reduced_data <- data.frame(cbind(pca_num$x[,1:3], tar))
# -> PC1, PC2, PC3를 가져올지, PC1, PC2를 가져올지는 분석목표에 따라 설정
reduced_data$tar <- as.factor(reduced_data$tar)
str(reduced_data)

library(ggplot2)
ggplot(data = reduced_data, aes(x=PC1, y=PC2)) +
  geom_point(aes(color = tar, shape = tar)) +
  xlab("PC1")+ylab("PC2") +
  ggtitle("PCA DATA")
  
# 3차원으로 표현
#install.packages("scatterplot3d")
library(scatterplot3d)
shapes = c(11,16)
# tar값을 수치형으로 변환
shapes <- shapes[as.numeric(reduced_data$tar)]
colors <- c('#999999', '#E69F00')
colors <- colors[as.numeric(reduced_data$tar)]
scatterplot3d(reduced_data[,1:2], color=colors, pch=shapes, angle=45)
# -> reduced_data[,1:3]은 데이터일 뿐 축을 설정하고 그런 옵션은 아니다.
# -> 축은 데이터에 따라서 scatterplot3d 함수가 알아서 설정하는 듯

# scatterplot3d(reduced_data[,1:3], color=reduced_data[,"tar"], pch=shapes, angle=45) # angle: 각도를 45도로