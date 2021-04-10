#install.packages("jpeg")
library(jpeg)

cat <- readJPEG('cat.jpg')

class(cat) # "array"
# 1차원: 벡터, 2차원:매트릭스, 3차원부터는 텐서 or array
# 이미지는 픽셀로 표현되는데 1픽셀마다 rgb로 이루어져 있다.

dim(cat) # 3 : rgb값(깊이, 3차원)

# rgb로 각각 분리
r <- cat[,,1]
g <- cat[,,2]
b <- cat[,,3]

# r매트릭스, g매트릭스, b매트릭스
# 각각 r,g,b를 차원축소해서 다시 합칠 것 -> 차원축소된 이미지가 결과로 나옴

# 차원축소함수 prcomp
# 차원축소를 할 때 scale을 하면 다른걸 할 수가 없어서 차원축소할 떄 scale은 하지 않는걸로
cat.r.pca <- prcomp(r, center=F)
cat.g.pca <- prcomp(g, center=F)
cat.b.pca <- prcomp(b, center=F)

rgb.pca <- list(cat.r.pca, cat.g.pca, cat.b.pca) # 리스트로 하나로 만들기

# 이미지는 따로 데이터 분석을 할 필요가 없음.
pc <- c(2,10,50,100,300)  # 차원 수 지정


for(i in pc) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i]) # 행렬곱필요(그래서 뒤의 행렬을 t함수를 써서 전치해준 것.)
    # i가 2차원일때는 1-2, 10차원일떄는 1~10
    # rotation : 각각의 eigenvertor
  }, simplify = 'array')  # 다 계산(함수적용)하고 array로 만들겠다.
  writeJPEG(pca.img, paste('cat_pca_',i,'.jpg', sep=''))
}
# sapply함수는 rgb.pca 리스트 각각의 값에 function()함수를 적용한 뒤 array로 만들어줘라는 의미

# %*%는 행렬곱(내적과 비슷한 연산)을 의미하는 연산자

# t는 transpose 
# t()함수는 수학적으로 전치를 의미함. 행렬의 행과 열을 전치(행을 열로 전환)

# 꿈이로 다시 해보기
ggumi <- readJPEG('GGumi.jpg')
class(ggumi)
dim(ggumi)

# rgb 분리
rr <- ggumi[,,1]
gg <- ggumi[,,2]
bb <- ggumi[,,3]

# 각각 rgb 차원축소
ggumi.r.pca <- prcomp(rr, center=F)
ggumi.g.pca <- prcomp(gg, center=F)
ggumi.b.pca <- prcomp(bb, center=F)

# 차원축소한 값들 일단 리스트로 한곳으로 모아둠
rgb.pca2 <- list(ggumi.r.pca, ggumi.g.pca, ggumi.b.pca)

# 차원 수 지정
pc <- c(2,10,50,100,300)

# 차원축소한 rgb를 하나로 합쳐서 이미지 만들기
for(i in pc) {
  # 차원축소값 리스트에 함수적용하겠다.
  pca.img2 <- sapply(rgb.pca2, function(j) {
    # 주성분값과 eigenvector의 행렬곱을 해서 array로 만들겠다.
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
    # 주성분 값이랑 아이젠벡터를 내적해야지만 원래대로 되돌아간다.
  }, simplify = 'array')
  # 파일로 저장
  writeJPEG(pca.img2, paste('ggumi_pca_',i,'.jpg',sep=''))
}
