## 데이터프레임 만들기

# 역사 점수 생성
history <- c(90,80,60,70)
history
# 수학 점수 생성
math <- c(50,60,100,20)
math

# 변수를 합쳐서 데이터프레임 만들기(2차원형태)
df_midterm <- data.frame(history, math)
# 데이터프레임 만들 때는 data.frame() 이용
df_midterm

# 변수 추가하기
class <- c(1,1,2,2)
class

df_midterm <- data.frame(history, math, class)
df_midterm

# 역사 평균, 수학 평균
mean(df_midterm$history)
mean(df_midterm$math)


## 외부 데이터 불러오기
# 엑셀 파일 : readxl 패키지 설치
install.packages("readxl")

# readxl 패키지 로드
library(readxl)

# 엑셀 파일 불러오기
df_finalexam = read_excel("r_data/finalexam.xlsx", sheet=1, col_names=T)
# sheet가 제각각일 때는 sheet이름을 적을 수도 있다. 
# col_name: 컬럼의 이름을 포함할지 안할지 T/True or F/False
df_finalexam

mean(df_finalexam$math)
mean(df_finalexam$history)
mean(df_finalexam$english)

df_finalexam = read_excel("r_data/finalexam.xlsx", sheet=1, col_names=F) #컬럼이름이 아예 행에 포함됨. 행 개수가 +1 됨.
df_finalexam

# 엑셀파일에 첫행에 컬럼이름이 없을 때, 
# F로 하면 그대로 행 개수 20개,
# T로 하면 id=1인 첫행이 컬럼이름이 되어 버려서 행 개수 19개
# 제목이 있으면 T로 하는 걸로

# 엑셀파일 -> csv파일로 만들어 저장할 수 있음.
write.csv(df_finalexam, file="r_data/output_newdata.csv")
