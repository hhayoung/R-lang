## 그래프

## 산점도 : 변수간의 관계 표현
library(ggplot2)

#ggplot2 의 문법은 레이어 구조.
# 1. 배경을 만들고 2. 그 위에 그래프 형태 그리고 3. 설정을 추가

# 배경 레이어 생성(첫번째 레이어)
ggplot(data=mpg, aes(x=displ, y=hwy)) # aes: 축

# 그래프 모양 추가(두번째 레이어)
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point() # 점으로 찍겠다.

# 그래프의 옵션을 추가(세번째 레이어)
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point() + xlim(3, 6) 
# xlim(limit): x축의 범위 지정

ggplot(data=mpg, aes(x=displ, y=hwy)) + 
  geom_point() + # 산점도 그래프로 
  xlim(3, 6) +   # x축 범위를 3~6으로
  ylim(10, 30)   # y축 범위를 10~30으로

# p188 혼자서 해보기
# Q1.
ggplot(data=mpg, aes(x=cty, y=hwy)) + geom_point()
# Q2.
midwest <- as.data.frame(ggplot2::midwest)

ggplot(data=midwest, aes(x=poptotal, y=popasian)) + geom_point()
# x,y축은 범위지정 해주지 않으면 딱 하나의 데이터 때문에 분석할 그래프의 크기가 너무 작아진다. x,y축 범위를 지정하면 좀 더 자세한 데이터분포를 알 수 있다.

ggplot(data=midwest, aes(x=poptotal, y=popasian)) + 
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)

#-------------------------------------------------

## 막대 그래프 - 집단간의 차이를 비교
library(dplyr)

# 평균 막대 그래프( geom_col() )
# 평균표 만들기(데이터 프레임 필요)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

# 그래프 생성
ggplot(data=df_mpg, aes(x=drv, y=mean_hwy)) +
  geom_col() # 막대그래프

# 정렬하고 싶을때는 reorder(x축 변수, 정렬기준 변수) 사용
ggplot(data=df_mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) +
  geom_col() # -붙으면 내림차순, 안붙으면 오름차순

# 빈도 막대 그래프( geom_bar() ) : 원(raw)자료를 바로 사용(y축 지정할 필요 없음. 원본을 가지고 개수를 세서 y축을 지정해줌)
# 값의 개수(빈도)로 막대의 길이를 표현
ggplot(data=mpg, aes(x=drv)) + geom_bar()

# x축의 변수 : 연속 변수
ggplot(data=mpg, aes(x=hwy)) + geom_bar()

# 평균막대그래프(geom_col()) vs 빈도막대그래프(geom_bar())
# 평균막대그래프 - 데이터프레임형식의 평균표를 먼저 만든 뒤, 만든 df를 data로 이용
#                - x,y축 지정
# 빈도막대그래프 - 바로 원자료그대로 data로 이용
#                - x축만 지정(x축 변수는 시간의 흐름을 가진 변수로)


# p193. 혼자서 해보기
# Q1.
mpg_suv <- mpg %>% 
  filter(class=="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

mpg_suv

ggplot(data=mpg_suv, aes(x=reorder(manufacturer, -mean_cty), y=mean_cty)) +
  geom_col()

# Q2.
ggplot(data=mpg, aes(x=class)) + geom_bar()

#-------------------------------------------------

## 선 그래프(시계열(Time Series: 시간의 흐름) 데이터를 표현하는데 주로 사용)

# x축에는 시간의 흐름이 담겨있는 변수로 할 것.
ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line()

# 혼자서 해보기
ggplot(data=economics, aes(x=date, y=psavert)) + geom_line()

#-------------------------------------------------

## boxplot(상자 그래프) : 집단의 다양한 데이터 특성들을 알아 볼 수 있는 그래프

ggplot(data=mpg, aes(x=drv, y=hwy)) + geom_boxplot()

# 먼저 볼 것 : 중앙값 위치 -> 박스크기
# 가장 굵은 선의 높이 - 중앙값 위치
# 박스 크기 - 1사분위수 ~ 3사분위수(50% 데이터가 가장 많은 구간)
#           - 크기가 크다 -> 데이터가 잘 퍼져있다.
#           - 크기가 작다 -> 데이터가 몰려있다.(그래서 평균값도 높음)
# 박스 안의 중앙값 위치 - 평균값이 어디에 치우쳐 있는지
#                       - 굵은선이 아래쪽에 있으면 상위25퍼가 더 많은 거고,
#                       - 굵은선이 윗쪽에 있으면 하위25퍼가 더 많은 거고
# 아랫수염 - 하위25퍼, 윗수염 - 상위25퍼
# 점 - 극단치(범위를 훌쩍 뛰어넘은 데이터)
#    - 박스 세로길이(IQR) * 1.5보다 크거나 작으면 점으로 표시
#    - 위의 점 : 3사분위수 + 1.5IQR 보다 크면 
#    - 아래 점 : 1사분위수 - 1.5IQR 보다 작으면

# p198. 혼자서 해보기
mpg <- as.data.frame(ggplot2::mpg)
mpg_class <- mpg %>% 
  filter(class %in% c("compact", "subcompact", "suv"))

ggplot(data=mpg_class, aes(x=class, y=cty)) + geom_boxplot()
# 그래프를 보면서 분석
# 중앙값은 compact가 가장 크네. 
# compact와 suv를 정규분포도 그래프로 보면 뾰족한 형태겠구나
# subcompact는 넓은 분포를 가진 완만한 형태겠구나
