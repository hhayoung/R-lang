## 함수
aa <- c(1,2,3)
aa

mean(aa)
max(aa)
min(aa)

bb <- c("a","a","b","c")
bb
qplot(bb) #빈도 그래프 만들기

# 문자 처리 함수
cc <- c("Hello","World","good","nice")
cc
paste(cc, collapse=" ") #빈칸 구분자로 문자를 붙이기

cc_paste <- paste(cc, collapse=" ")
cc_paste #cc자체를 바꾸지 않고 다른 변수에 할당

cc2_paste <- paste(cc, collapse=",")
cc2_paste

## 패키지(packages) = 함수 + 데이터 꾸러미
# 함수를 사용하려면 반드시!!
# 패키지를 설치(install) & 로드(library)
# RStudio 실행할 때마다 한번 설치된 패키지는 로드해줘야 한다.
# 단, 내장함수는 설치 & 로드가 필요없음(min, max, mean, ...)
# 필요한 기능에 맞게 어플을 설치하듯이 패키지를 설치하면 된다. 패키지들은 수시로 업데이트 된다.

# e.g) ggplot2 : 시각화 패키지(qplot, geom_line, ... 포함)

# 이러한 패키지들은 CRAN(공식사이트) 등록되어 있음.
# 15,000여개의 패키지들이 있고, 누구나 만들어 등록할 수 있다.

bb
a #현재 파일에 없는 변수a도 오른쪽 Environment에 올라가 있기 때문에 사용할 수 있음.
qplot(bb)
head(mpg) #ggplot2 패키지 안에 있는 데이터 mpg

# 함수를 사용하려면 파라미터를 아는 것이 중요!
# 함수 파라미터(parameter) 지정하기
qplot(data=mpg, x=hwy)
qplot(data=mpg, x=cty)
qplot(data=mpg, y=hwy, x=drv, geom="point") # geom: 표시할 모양
qplot(data=mpg, y=hwy, x=drv, geom="boxplot", colour=drv) # colour: 색

# help에 있는 메뉴얼 참조하기
?qplot # 옆에 Help에 메뉴얼 창이 뜸.

# example에 있는 코드를 복사해서 실행해보고 예측해보기기
qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)
