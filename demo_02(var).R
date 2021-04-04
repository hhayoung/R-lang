## 변수
# 패키지 로드(library)
library(ggplot2) # <- mpg 데이터가 들어있는 곳

#mpg 데이터의 변수를 다루기
head(mpg)

mean(mpg$hwy) #mean:평균, hwy값의 평균
max(mpg$hwy)
min(mpg$hwy)
hist(mpg$hwy) # histogram 그림으로 표현

# 변수 만들기
a <- 100 # 할당연산자
a

c <- 300
c

c <- 3.5
c

# 변수로 연산(사칙연산)하기
a+c

# 연속값이 있는 변수 만들기
# c()함수 이용
dd <- c(1,2,3,4,5)
dd

ee <- c(1:10)
ee
ee <- c(1:5)
ee

ff <- seq(1,4)
ff
gg <- seq(1,10,by=2)
gg
# 연속값 변수로 연산하기
dd+10
dd  #dd자체는 변하지 않음
ee
dd+ee
# 문자 변수 만들기
a2 <- "a"
a2
b2 <- "world"
b2
c2 <- "Hello World!"
c2
# 연속된 문자 변수를 만들기
d2 <- c("a","c","e")
d2
e2 <- c("Hello!!","World","is","Good") 
e2
# 문자변수의 연산?
b2+10 # Error in b2 + 10 : 이항연산자에 수치가 아닌 인수입니다

a2+b2 # Error in b2 + 10 : 이항연산자에 수치가 아닌 인수입니다
