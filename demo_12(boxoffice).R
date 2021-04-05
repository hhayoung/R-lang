## 관객수 기준 역대 박스오피스
# http://www.kobis.or.kr

library(readxl)
movies <- read_excel("movies.xlsx")
movies

## 데이터 파악하기
head(movies)
tail(movies)
names(movies) # 변수가 뭐뭐 있는지
dim(movies) # 총 관측치는 200개 변수는 7개
summary(movies)

max(movies$매출액)
max(movies$관객수)
max(movies$상영횟수)

# 최대관객수의 영화이름을 알고 싶을 때
which.max(movies$관객수) # 64. <-행의 번호
movies[64, 1]
movies[64, ]

# 스크린수가 가장 적은 영화가 뭔지
which.min(movies$스크린수)
movies[60, ]

which.max(movies$매출액)
movies[64, ]

# histogram
movies$관객수
hist(movies$관객수)

## histogram 말고 다른 그래프
library(ggplot2)
ggplot(data=movies, aes(관객수)) + geom_histogram()

# 관객수와 상영횟수 관계
ggplot(data=movies, aes(x=관객수, y=상영횟수)) + geom_point()

# 국가별 비교를 위해 색 추가
gp <- ggplot(data=movies, aes(x=관객수, y=상영횟수, col=대표국적)) + geom_point()

# 인터랙티브 그래프
# : 마우스 움직임에 반응해 실시간으로 형태가 변함
install.packages("plotly")
library(plotly)
ggplotly(gp)

# 상영횟수당 관객수가 가장 많은 영화 찾기
# 관객수 / 상영횟수 = 값이 최고인 영화 찾기
which.max(movies$관객수/movies$상영횟수)
movies[21, ]

which.max(movies$상영횟수)
movies[126, ]

# 상영횟수당 관객수가 가장 많은 영화 top10 뽑아내기
top_10 <- movies %>% 
  arrange(desc(관객수/상영횟수)) %>% 
  head(10)
top_10

sort(movies$관객수/ movies$상영횟수, decreasing=T) #decreasing 기본값 F. decreasing=T(내림차순), F(오름차순)
library(dplyr)
movies %>% 
  mutate(상영대비관객수 = 관객수/상영횟수) %>% 
  arrange(desc(상영대비관객수)) %>% 
  head(10)
