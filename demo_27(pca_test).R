iris  # 꽃에 대한 기본 데이터셋
# PCA로 차원 축소 해보기

# 데이터 파악하기
head(iris)  # sepal: 꽃받침, petal: 꽃잎
str(iris)  # 전체 데이터 150개

# 결측치 확인
colSums(is.na(iris))   # 각 변수에 해당되는 na값이 나온다.

summary(iris)  # 통계량
boxplot(iris[,1:4])  # 큰 이상치는 없음

# 주성분 분석하기
# prcomp 함수 적용하기(평균:0, 분산:1) - 표준화도 같이 해줌
iris.pca <- prcomp(iris[1:4], center = T, scale = T)
# -> 실행했을 때의 오른쪽 Environment 탭에서 자세히 표현된 부분 보기

summary(iris.pca)  # 이걸로 보는게 나음
"
Importance of components:
                          PC1    PC2     PC3     PC4
Standard deviation     1.7084 0.9560 0.38309 0.14393
└ 표준편차.제곱하면 분산  └ 제곱한 값 = 주성분1의 eigenValue
Proportion of Variance 0.7296 0.2285 0.03669 0.00518
└ 분산비율                └ 주성분1의 분산비율(전체 분산의 72프로 차지)
Cumulative Proportion  0.7296 0.9581 0.99482 1.00000
└ 분산의 누적비율
"
# 분석, 결과치 확인 및 해석
# Standard deviation(표준편차)의 제곱 = 분산 = eigenvalue
# Proportion of Variance : 각 주성분이 차지하는 분산 비율
# Cumulative Proportion : 분산의 누적 비율


iris.pca$rotation  # eigenVector

"
                    PC1         PC2        PC3        PC4
Sepal.Length  0.5210659 -0.37741762  0.7195664  0.2612863
Sepal.Width  -0.2693474 -0.92329566 -0.2443818 -0.1235096
Petal.Length  0.5804131 -0.02449161 -0.1421264 -0.8014492
Petal.Width   0.5648565 -0.06694199 -0.6342727  0.5235971
"
## 각 변수에 해당되는 벡터값. pc1에서 가장 큰 수 -> petal.length 
# 첫번째 주성분에서 가장 중요한 변수가 뭐냐? 
# -> petal.length가 가장 중요한 변수다라고 말할 수 있다.
# 축을 만들 때 가장 큰 영향을 미친 것? 벡터 중에 가장 큰값?
# pc1 주성분에서 4개의 변수 중에 가장 중요한건 petal.length다. 
# petal.length로 나머지 변수들이 투영된거니까
# rotation : eigenVector

# eigenvector(고유벡터) : 어떤 변수들이 중요한지를 eigenvector을 통해서 알 수 있다. 

# 새로운 축에서 그 데이터들이 갖는 값(각 주성분들의 값)
head(iris.pca$x, 10)
# 새로운 축이 만들어지면 각각 벡터값들이 하나의 축으로 투영되는데
# 축으로 몰렸을 때의 값(차원축소 했었을 때의 각각의 값)

# 차원축소해서 시각화해보기(보통 2차원, 3차원 중 하나)
# scree plot 확인(주성분의 개수를 선택하기 위해 scree plot을 그린다)
# 주성분 중 몇가지를 쓸 지는 plot과 summary를 통해 알아볼 수 있다.
# 주성분 개수를 선택하는 기준
# 1. 시각화를 위해 2,3 중 하나
# 2. eigenvalue > 1를 선택
# 3. elbow 포인트 선택
plot(iris.pca, type="l", main="Scree Plot")
# x축 주성분개수, y축 분산 = eigenvalue
# elbow(변화폭이 큰 부분) 여기 그래프에서 3? 근데 분산값이 너무 작음.
# 2가 그나마 1보다 크진 않지만 선택할 수 있는 방법이 2차원
# 주성분 개수는 2개로 -> summary까지 확인해보는게 좋다. 
summary(iris.pca)  # PC2까지해서 95%를 설명할 수 있기 때문에 주성분으로 PC1과 PC2 선택

# 2개의 차원으로 축소 x:실제주성분값
head(iris.pca$x[,1:2], 10)

# 2차원 시각화
install.packages('ggfortify')
library(ggfortify)
autoplot(iris.pca, data=iris, colour="Species")
# 각 품종별로 몰려있는게 보인다. 데이터의 전체적인 구조를 차원 축소를 통해 대충 파악하고 있는 것. 
