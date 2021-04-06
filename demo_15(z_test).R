### Z 검정

rawN30 <- read.csv(file="data/htest03.csv", header = T)
head(rawN30)
tail(rawN30)
dim(rawN30) # 60 2

groupA3 <- rawN30[rawN30$group == 'A', 1:2]
groupB3 <- rawN30[rawN30$group == 'B', 1:2]

mean(groupA3[,2])
mean(groupB3[,2])

### 가설 설정
# 귀무가설 : 그룹A, 그룹B간 평균 차이가 없다.
# 대립가설 : 그룹B의 평균 키가 그룹A의 평균 키보다 크다.

# Z 검정은 데이터가 30개 이상이므로 정규분포를 따르기 때문에 데이터 정규성 검정이랑 분산 동질성 검정을 할 필요가 없음.

### Z-test

# R에서는 Z 검정 함수가 없다. 함수를 정의해서 사용해보기

# R에서 함수 만드는 방식
# z.test <- funtion(x1, x2) {
# 
# }

z.test <- function(x1, x2) {
  # 가져온거 복붙
  n_x1 = length(x1)
  n_x2 = length(x2)
  mean_x1 = mean(x1)
  mean_x2 = mean(x2)
  
  cat("\n")
  cat("\tTwo Sample z-test\n")
  cat("\n")
  cat("mean of x1:", mean_x1, "\n")
  cat("mean of x2:", mean_x2, "\n")
  var_x1 = var(x1)
  var_x2 = var(x2)
  z = (mean_x1 - mean_x2)/sqrt((var_x1/n_x1)+(var_x2/n_x2))
  abs_z = abs(z)
  cat("z =", abs_z, "\n")
  p_value = 1-pnorm(abs_z)
  cat("p-value =", p_value)
}

z.test(groupA3[,2], groupB3[,2])
# p-value = 0.04866272 < 0.05 귀무가설 기각. 대립가설 채택

### 결론
# 대립가설
# -> 그룹B의 평균 키가 그룹A의 평균 키보다 크다.

# 소표본일 때는 10cm 차이가 있음에도 귀무가설이 채택됐었는데
# 지금과 같은 경우에는 3cm 차이임에도 불구하고 대립가설이 채택됐다.
# => 평균만 봐서는 알 수가 없다.반드시 검정을 통해 결론을 내야 한다.

## 이 데이터를 t-test 했으면 결과가 어땠을까
# 데이터가 30개 이상이니까 데이터 정규성 검정은 할 필요 없고
# 분산 동질성 검정
var.test(groupA3[,2], groupB3[,2])
# p-value = 0.09465 > 0.05 : 분산이 동일하다

t.test(groupA3[,2], groupB3[,2], alternative = "less", var.equal = T, conf.level = 0.95)
# p-value = 0.05136 > 0.05 귀무가설 채택
# conf.level = 0.95 : 신뢰구간(confidence interval)을 95%로 하겠다

# z-test와 t-test 의 결과가 다르게 나온다.
# 검정방법에는 여러가지가 있기 때문에
# 데이터를 보고 어떤 검정방법을 사용해야 하는지는 내가 결정해야 한다.
# 기본적으로 검정방법 알고리즘대로 선택하는게 맞는것이고
# 지금의 t-test는 잘못된 분석으로 나온 결과라고 보면 된다.

# 막상 데이터 분석을 하려고 하면 
# 이걸 어떤 분석으로 해야하지? 고민을 많이 해야 함.
# 그 데이터가 어떤 특성을 가지고 있는지 천천히 하나하나 분석하고나서 
# 분석방법 알고리즘을 따라가면서 검정방법 중 하나를 골라서 검정을 하는 것.