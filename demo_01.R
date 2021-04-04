#주석처리

# 패키지 설치
install.packages("dplyr")
install.packages("ggplot2") #시각화

# 패키지 로드
library(dplyr) #로드를 해야 사용 가능
library(ggplot2)

# 데이터 파악(앞부분 일부 출력해보기)
head(mpg) #mpg라는 데이터셋의 맨앞부분 출력(테이블 모양)
tail(mpg) # A tibble: 6 x 11(속성,필드 개수)

# 데이터의 크기 확인
dim(mpg) # 234행 11열

# 어떤 열(변수, 속성, 필드(db))들이 있는지 확인
str(mpg)

# 통계 수치를 요약해서 출력하기
summary(mpg)
# min, max, median
# Min 1Q(쿼터) Median 3Q(쿼터) Max 
# 문자형은 구분할 수 없기 때문에 수치값만 4등분으로 나눌 수 있다.

# Raw(원본) 데이터를 살펴보고자 할 때(데이터가 많을 때)
View(mpg) # V가 대문자, 대소문자 구분함
# 데이터가 정말 복잡할 땐 봐도 복잡해서 많이 사용되지는 않음.

# 지금까지의 과정 -> 분석한 것!
# 분석하면서 아이디어를 얻어내야 한다. 
# 다양한 아이디어를 통해서 시각화를 통한 결과 예측

# 위의 데이터를 파악함으로써 통계 분석을 할 수 있음
# 통계적 데이터분석

# 1. 회사별 평균 연비(정렬)
mpg %>%
  group_by(manufacturer) %>% #제조사별로
  summarise(mean.hwy=mean(hwy)) %>% #고속도로 연비 평균
  arrange(desc(mean.hwy)) # mean = 평균

# 2. 포드사의 연비 확인
mpg %>%
  filter(manufacturer=="ford") %>%
  group_by(model) %>%
  arrange(desc(hwy))

# 3. 배기량이 연비에 미치는 영향(회귀분석)
lm.mpg <- lm(data=mpg, hwy~displ) # lm함수로 회귀분석 바로 됨.
# data는 mpg를 쓸거고, 거기에서 hwy랑 displ과의 관계를 회귀분석해서 집어 넣겠다.

# 회귀분석
summary(lm.mpg)

# 시각화 해보기
qplot(data=mpg, x=displ, y=hwy)
# 선이 나온다는건 선형모델인거
# 선을 가지고 다른 데이터들이 들어왔을 때,
# 다른 데이터간의 차이가 오차율인데 오차율이 적을수록 좋음.