## 지도 시각화

# 단계 구분도(지역별 통계치)
install.packages("mapproj")
install.packages("ggiraphExtra")

library(ggiraphExtra)

# 패키지안에 있는 데이터셋 목록
data() #USArrests : 주별 강력 범죄율 정보

# 구조 확인
str(USArrests) #UrbanPop(도시 인구)
head(USArrests)

library(dplyr)

library(tibble)
# 행의 이름을 state 변수로 바꿔서 새로운 데이터 프레임을 만듦
crime <- rownames_to_column(USArrests, var="state")
crime$state <- tolower(crime$state)
crime

# 미국의 위/경도 정보가 있는 지도 데이터터
install.packages("maps")
library(ggplot2)

# 데이터 프레임으로 불러옴
states_map <- map_data("state")
str(states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T)
