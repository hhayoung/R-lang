df <- read.csv('customers_data.csv', stringsAsFactors = F, header=T)

library(dplyr)  # %>% 이거 쓰려면 필요

# 도매시장에서 고객들이 연간 물건을 얼마나 구매했는지를 정리한 데이터
head(df) 
# Channel: 유통채널, Region: 지역, Fresh: 신선식품, Milk: 유제품, Grocery: 채소류, Frozen: 냉동식품, Detergents_Paper: 세제·제지, Delicassen: 제품사이름

# 데이터 분석
str(df)  # 데이터 440개
dim(df)

# Channel, Region 변수 범주형 데이터 변환
df$Channel <- df$Channel %>% as.factor()
df$Region <- as.factor(df$Region)
str(df)

# 결측치 확인
colSums(is.na(df))  # 결측치 없음
sum(is.na(df))

# 이상치 확인
unique(df$Channel)
unique(df$Region)
unique(df$Fresh)
unique(df$Milk)
unique(df$Grocery)
unique(df$Frozen)
unique(df$Detergents_Paper)
unique(df$Delicassen)
# 여기서는 굳이 안해도 됐었음.

# 기술 통계량 확인
summary(df)
# Median(중앙값)과 Mean(평균값)의 차이가 크다.
# -> 평균값의 단점: 이상치의 영향을 많이 받음.
# 중앙값은 이상치의 영향을 덜 받음.

# boxplot 이상치 확인할 때 사용
options(scipen=999)
boxplot(df[,3:ncol(df)]) # ncol() : 컬럼의 개수
# scipen: 정수 또는 과학적 표기법으로 숫자값을 표현할 때 사용
# 양수: 정수표현, 음수: 과학적 표기(기본값이 음수)
# options(scipen = 999) -> 정수 표기
# options(scipen = -999) -> 과학적 표기

# 이상치 제거
tmp <- NULL
for(i in 3:ncol(df)) {
  tmp <- rbind(tmp, df[order(df[,i], decreasing=T),] %>% slice(1:5))
}
# df를 한 열 씩 내림차순 해서 큰 값부터 값이 나오게 하고
# 그런 다음 1행부터 5행까지 큰 값 5개 제거 

tmp %>% arrange(Fresh) %>% head() # 중복된 값들 발견

# 중복 제거
tmp <- distinct(tmp)
# anti_join(df,tmp)은 df에서 tmp를 제거
# 같은 것들을 찾아서 제거해주는 함수
rm_outlier_df <- anti_join(df, tmp)

# 이상치 제거 후 박스플롯 확인해보기
par(mfrow=c(1,2))
boxplot(df[,3:ncol(df)])
boxplot(rm_outlier_df[,3:ncol(df)])
# 박스를 벗어난 모든 데이터들을 삭제하는 건 안된다. 
# 너무 간격이 큰 값들만 제거

# 있는 거에서 꺼내고 있는 거에서 꺼내고 하니까 중복이 될 수 밖에 없음.

# Plots 창에 있는 모든 Plots들을 삭제(clear, delete)하고 싶을 때
dev.off()

# 전처리는 됐고, 이제 군집으로 나눠주면 된다.
# 고객 분류 개수를 지정해야 하는데 K값을 선택해야 함. 
# 엘보우 메소드, 실루엣 메소드 중 이용

# 클러스터의 갯수(k 개수 선택)
# 군집 분석시 사용되는 패키지 factoextra
# install.packages('factoextra')
library(factoextra)

#첫번째, 두번째 방법을 둘 다 해서 둘 중에 좋은걸 선택
# 첫번째 방법> Elbow Method
set.seed(2020)
fviz_nbclust(rm_outlier_df[,3:ncol(rm_outlier_df)], kmeans, method="wss", k.max=15) + theme_classic() + ggtitle("Elbow Chart")
# elbow point 변동이 없는 지점 -> 여기서는 5 -> k=5(5개의 군집)

# 두번째 방법> Silhouette Method
fviz_nbclust(rm_outlier_df[,3:ncol(rm_outlier_df)], kmeans, method="silhouette", k.max=15) + theme_minimal() + ggtitle("Silhouette Chart")
# silhouette 최대값일 때 k=3

# elbow방식은 5가 나왔고, slihouette방식은 3이 나왔다. 
# 둘 중에 뭘 해야 할지 고민을 해봐야 한다. 
# 더 세분화하게 분류하겠다 하면 5를 선택하면 됨.

# 클러스터를 5개로 나누기

# kmeans 모델 생성 : kmeans() 함수 사용
df_kmeans <- kmeans(rm_outlier_df[,3:ncol(rm_outlier_df)], centers = 5, iter.max = 1000)
# centers:k개수 지정, iter.max: 반복을 몇번할건지

# 결과 해석
df_kmeans
"##############
K-means clustering with 5 clusters of sizes 179, 42, 72, 110, 18
-> 각 클러스트의 사이즈 
  총 5개로 군집이 나눠졌고, 각각 클러스트에
  179, 42, 72, 110, 18개의 데이터들이 들어간 상태

Cluster means:
      Fresh      Milk   Grocery   Frozen Detergents_Paper Delicassen
1  4267.933  3751.480  4672.950 2211.313        1550.4469   1036.006
2 25332.000  5603.548  7160.024 4144.667        1449.2381   2053.333
3  5152.250 12536.694 19616.472 1644.014        8794.1389   1696.653
4 14527.509  2606.064  3503.873 3202.073         804.8091   1037.882
5 40558.056  3113.444  3814.333 2974.833         684.2778   1271.333
-> 각 클러스트의 변수별 평균값
   행이 각 클러스터(1~5번 그룹)

Clustering vector:
  [1] 4 1 1 4 2 1 4 1 1 3 1 4 2 2 2 4 1 1 2 1 4 1 2 2 4 4 4 3 5 2 1 4 2 1 1
 [36] 2 3 3 2 4 3 3 1 3 3 4 3 1 1 5 3 2 1 3 3 4 1 1 1 3 1 1 2 1 1 4 1 2 1 4
-> 각 데이터(벡터)들이 몇번째 군집에 할당되었는지
##############"

# 시각화
barplot(df_kmeans$centers, beside=T, col=1:6) # 모델에 해당되는 centers값 -> 평균값

barplot(t(df_kmeans$centers), beside=T, col=1:6)
legend("topright", colnames(df[3:8]), fill=1:6, cex=0.5)
# 각 군집에서 어떤 물건을 얼마나 샀는지 (이게 더 보기 편안)
# 이런 결과를 보고 마케팅 전략을 짜는 것.

# 이것도 시각화. -> 해석하기에는 barplot이 더 좋다. 
fviz_cluster(df_kmeans, data=rm_outlier_df[,3:ncol(rm_outlier_df)])

# 산점도로 표현
df_clustered <- data.frame(rm_outlier_df, cluster=factor(df_kmeans$cluster))
library(ggplot2)
ggplot(df_clustered, aes(x=Fresh ,y=Grocery, color=cluster)) +geom_point()
