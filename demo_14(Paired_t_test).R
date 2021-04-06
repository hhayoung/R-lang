### 대응표본 T 검정

# 데이터는 마케팅 전후의 판매액이라고 가정
raw_d <- read.csv(file="data/htest02d.csv", header = T)
head(raw_d)
dim(raw_d) # 10  2 -> 데이터 총 10개

groupA_d <- raw_d[,1]
groupB_d <- raw_d[,2]

mean(groupA_d) # 10500
mean(groupB_d) # 23800


### 가설 설정 
# 귀무가설 : 마케팅을 통해 판매액의 변화(차이)가 없음.
# 대립가설 : 마케팅을 통해 판매액이 증가함.

### 데이터 정규성 검정

# 차이를 구할 때는 before에서 after을 빼던지, after에서 before을 빼던지 상관 X
d = groupA_d - groupB_d

shapiro.test(d) # p-value = 0.1621 > 0.05 귀무가설 채택 -> 정규분포를 따른다.
# 시각화로 확인
qqnorm(d) # 데이터분포를 확인할 수 있고,
qqline(d) # 선을 그어서 더 명확히 확인.
# 선을 중심으로 데이터들이 큰 차이가 없다 -> 정규분포를 따른다.


### 분산 동질성 검정
# A와 B집단이 있을 때 둘의 분산이 같은지 다른지 판단하는 건데
# 여기는 차이라는 파생변수 하나만 있기 때문에 (두 집단이 없기 때문에)
# 대응표본 t 검정에서는 분산 동질성 검정을 하지 않는다.

# 분산 동질성 검정은 안한다.
# 하나의 집단(차이)이기 때문에..

### T-test
t.test(groupA_d, groupB_d, alternative = "less", paired = T)
# paired = T : 대응표본이라고 지정하는 속성
# p-value = 0.006745 < 0.05 -> 귀무가설 기각. 대립가설 채택

### 결론 
# => 마케팅을 통해 판매액이 증가함.
