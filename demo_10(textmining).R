## 텍스트 마이닝
# : 문자로 된 데이터에서 가치 있는 정보를 얻어 내는 분석 기법

install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP") #우리나라 형태소 분석기(한글 자연어 분석 패키지)
# -> R버전 4.0에서는 설치 안됨. 
# 설치하려면 

koinstall = c('rJava', 'stringr', 'hash', 'tau', 'Sejong', 'RSOLite', 'devtools', 'vctrs')
# 변수에 담아두고 변수를 install

install.packages(koinstall)
# 이 상태로도 아마 안 됨.
remotes::install_github('haven-jeon/KoNLP', upgrade="never", INSTALL_opts = c("--no-multiarch"))
# 이제는 됐음.

library(rJava)
library(stringr)
library(KoNLP)

# 사전 설정하기
# 보통은 세종딕셔너리 많이 사용(책은 NIADic())
useSejongDic() # 1번 눌러서 All 설치

extractNoun("롯데마트가 판매하는")
# KoNLP : 한글 어원의 형태소를 분석해주는 분석기

# 파일 읽어오기 readLines()
txt <- readLines("hiphop.txt")
head(txt)

# stringr 패키지 안의 str_replace_all()
txt <- str_replace_all(txt, "\\W", " ") #특수기호 없애기

nouns <- extractNoun(txt) #명사 뽑아오기
nouns

# extractNoun()의 출력 형태 = 리스트
unlist(nouns) #리스트가 아닌 형태

table(unlist(nouns)) #각 단어의 빈도수 출력

# 단어별 빈도표 생성
wordCount <- table(unlist(nouns))

wordCount

df_word <- as.data.frame(wordCount, stringsAsFactors = F)
# stringsAsFactors 속성
# data.frame()으로 데이터프레임을 생성할 때, 변수에 문자가 있을 경우 자동으로 factor타입으로 변환된다. 하지만 factor 변수는 연산이 되지 않으므로 stringsAsFactors() 함수를 써서 factor타입으로 변환되지 않게 한다.


df_word

# 전처리 작업 %>%
library(dplyr)

# 변수명 바꾸기
df_word <- rename(df_word, word=Var1, freq=Freq)

# 두 글자 이상 단어 추출
df_word <- filter(df_word, nchar(word)>=2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

## 워드 클라우드 만들기
# : 단어의 빈도를 구름 모양으로 표현한 그래프
install.packages("wordcloud")

library(wordcloud)
library(RColorBrewer) #글자 색을 표현하는 데 사용되는 패키지

set.seed(100) # 난수 고정 => 항상 동일한 모양으로 생성되도록

# brewer.pal() 색상 코드 목록 만들기
pal <- brewer.pal(9, "Set1") #팔레트(색상)
word <- c("인천광역시", "서울시", "대구시")
freq <- c(700, 80, 30)
wordcloud(word, freq, colors=pal)

# 힙합노래가사 워드 클라우드
wordcloud(words=df_word$word,
          freq=df_word$freq,
          min.freq=2,
          max.words = 200,
          random.order=F,
          rot.per=.1,
          scale=c(4,0.3),
          colors=pal)

# --------------- 201015 -----------------
library(KoNLP)
library(RColorBrewer)
library(wordcloud)

useSejongDic()

pal <- brewer.pal(8, "Dark2")

text <- readLines(file.choose()) #파일탐색기 뜸

text #한글깨짐

text <- readLines(file.choose(), encoding="UTF-8")
text

# 각 행렬로 명사를 추출 
# sapply() : 결과를 벡터나 행렬로 리턴됨.

noun <- sapply(text, extractNoun, USE.NAMES=F)
#USE.NAMES = T -> 행이 보이고, 명사 추출
#USE.NAMES = F -> 행이 보이지 않고, 명사 추출
noun

noun2 <- unlist(noun)
noun2

# 단어 빈도수
word_count <- table(noun2)
word_count

df <- as.data.frame(word_count, stringsAsFactors = F)
df
library(dplyr)
df <- rename(df, word=noun2, freq = Freq)

df <- filter(df,nchar(word) > 1 )
top_20 <- df %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

wordcloud(word = df$word,
          freq = df$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .5,
          scale = c(10,1),
          colors = pal)

library(ggplot2)

# 빈도수로 정렬된 order 변수 생성
order <- arrange(top_20, freq)$word

ggplot(data=top_20, aes(x=word, y=freq)) + 
  ylim(0, 40) +
  geom_col() +
  # 막대그래프를 빈도수대로 출력
  scale_x_discrete(limits = order) +
  # v = vertical -5를 주면 라벨이 확 올라감.
  geom_text(aes(label=freq), hjust=-0.5) +
  coord_flip() # 90도 회전 -> vjust를 hjust로
  