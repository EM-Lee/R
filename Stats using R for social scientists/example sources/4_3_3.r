# 4부. 통계분석
# 3장. 다변량 추리통계분석
# 3. 상관관계 분석(correlation analysis)

# 반복되는 부분에 대해서는 설명을 생략한다.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# 두 변수를 체크해서 결측값으로 처리할 부분이 있는지를 살펴본다. 
# 독서량(q2)의 경우는 98(잘 모름), 99(응답거부)를 결측값으로 처리하였다. 
# 교육수준의 경우 8(잘 모름), 9(응답거부)를 결측값으로 처리하였다.  
table(pew$q2, useNA = 'always')
table(pew$educ, useNA = 'always')
pew$q2r <- pew$q2
pew$educr <- pew$educ
pew$q2r[pew$q2 == 98 | pew$q2 == 99] <- NA
pew$educr[pew$educ == 8 | pew$educ == 9] <- NA
# 우선 변수들의 히스토그램을 통해 분포를 확인해 본다. 
hist(pew$q2r,breaks = 15) 
hist(pew$educr,breaks = 7)

# q2r의 경우 몇몇 사람들의 독서량은 매우 높지만, 대부분의 사람들이 거의 책을 읽지 않는 것을 보인다. 
# 이렇게 치우친 분포(skewed distribution)의 경우 로그전환을 하면 정규분포에 가까운 형태을 얻을 수 있다.  
# 여기서는 밑(base)가 10인 상용로그를 취하였다. 0권을 읽을 사람을 처리하기 위해 상수 1을 더하였다. 
pew$log.q2r <- log((pew$q2r+1), 10)
# plot()을 이용, 두 변수 사이의 산점도를 그려본다.  type = 'p'는 점을 찍는다는 의미다. 
plot(pew$educr, pew$log.q2r, type = 'p')

# 위의 플롯은 교육수준과 독서량의 관계를 효과적으로 보여주지 못하는 단점이 있다. 표본이 클 경우 
# 산점도가 효과적이지 않은 경우가 많은데, 이런 경우 점들이 밀집된 정도를 명암으로 처리한 
# 밀도산점도(density scatterplot)이 효과적일 수 있다. 밀도산점도의 경우 hexbin이라는 라이브러리
# 에서 얻을 수 있다. 설치가 되어 있지 않으면, install.packages('hexbin')을 통해 설치를 시키자.  
library('hexbin')
# 교육수준을 X축, 로그치환한 독서량을 Y축에 배치한다. xbins의 경우 X축에 배치될 육각형모양 영역의 
# 개수를 의미한다 (6을 설정하면 7개의 영역이 나온다). 이렇게 변환한 후 hex.plot이라는 오브젝트로 저장한다 
hex.plot <- hexbin(pew$educr, pew$log.q2r, xbins = 6) 
# hex.plot 오브젝트의 그림을 그린다. xlab과 ylab은 각각 X축과 Y축의 이름을, 그리고 main은 그래프의 이름을
# 설정하는 옵션이다.  
plot(hex.plot, xlab = 'educational achievement', ylab = 'book-reading (log-transformed)',
  main = "Relationship between educational acheivement and book reading")
  
# 밀도산점도에서 볼 수 있듯이 둘 사이의 관계는 단선형적 관계에 가까운 듯 하다. 
# cor.test()는 두 변수의 상관관계의 통계적 유의도를 테스트한다. type을 통해 피어슨 r임을 명확하게 하였다.  
cor.test(pew$educr, pew$log.q2r, method = 'pearson') 
# 피어슨상관계수는 다음과 같이 계산할 수도 있다. 우선 두 변수에서 결측값이 없는 사례들을 골라낸다. 
# 피어슨 r의 공식을 프로그래밍하면 cor.test()에서의 상관계수와 동일한 값을 얻을 수 있다.  
pew.subset <- pew[complete.cases(pew$educr,pew$log.q2r),] 
cov(pew.subset$educr,pew.subset$log.q2r)/(sd(pew.subset$educr)*sd(pew.subset$log.q2r)) 

# cor.test()에서 type을 통해 spearman으로 바꾸면 rho를 구할 수 있다.  
cor.test(pew$educr, pew$log.q2r, method = 'spearman') 
# cor.test()에서 type을 통해 kendall으로 바꾸면 tau를 구할 수 있다.  
cor.test(pew$educr, pew$log.q2r, method = 'spearman') 

# r의 경우 .358, rho의 경우 .363으로 계수의 값이 조금 다르다는 것을 알 수 있다. 
# 이에 비해 tau는 .297로 상대적으로 낮다. 
# 만약  독서량을 로그전환시키지 않으면 r은 계수값에 변화가 있는 반면, rho와 tau는 변화없다. 
cor.test(pew$educr, pew$q2r, method = 'pearson') 
cor.test(pew$educr, pew$q2r, method = 'spearman') 
cor.test(pew$educr, pew$q2r, method = 'kendall') 

# 독서량을 선형변환(linear transformation)시키는 한, rho와 tau는 달라지지 않는다.  
cor.test(pew$educr, pew$q2r^2, method = 'pearson') 
cor.test(pew$educr, pew$q2r^2, method = 'spearman') 
cor.test(pew$educr, pew$q2r^2, method = 'kendall') 

cor.test(pew$educr, sqrt(pew$q2r), method = 'pearson') 
cor.test(pew$educr, sqrt(pew$q2r), method = 'spearman') 
cor.test(pew$educr, sqrt(pew$q2r), method = 'kendall') 

# 반복되는 부분에 대해서는 설명을 생략한다.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# 히스패닉이 아닌 백인(1)과 백인이 아닌 나머지 인종(소수인종, 0)으로 구분한다.
# 잘모름(8)과 응답거부(9)는 결측값으로 처리했다. 
table(pew$race, pew$hisp, useNA = 'always')
pew$white <- 0
pew$white[pew$race == 1] <- 1
pew$white[pew$race == 8 | pew$race == 9] <- NA
# 백인이면서 히스패닉이라고 응답한 사람은 소수인종으로 코딩하였다.  
pew$white[pew$hisp == 1] <- 0
# 히스패닉여부도 잘모름(8)과 응답거부(9)는 결측값으로 처리했다
pew$white[pew$hisp == 8 | pew$hisp == 9] <- NA
table(pew$white, useNA = 'always') 
# 인터넷 이용여부에서 잘모름(8)과 응답거부(9)는 결측값으로 처리했다. 
# 비이용자는 0, 이용자는 1로 코딩하였다.   
table(pew$intuse, useNA = 'always') 
pew$intaccess <- pew$intuse
pew$intaccess[pew$intuse == 8 | pew$intuse == 9] <- NA
pew$intaccess[pew$intuse == 2] <- 0
# 인종과 인터넷 이용여부의 교차표를 살펴본 후 white.intaccess 오브젝트를 생성한다.  
white.intaccess <- table(pew$white, pew$intaccess)
# phi를 구하기 위해서는 외부 라이브러리로 psych가 필요하다. 설치가 되어 있지 않을 경우 라이브러리 
# 구동이전에 install.packages('psych')로 라이브러리를 설치하여야 한다.  
library('psych') 
# phi()는 table()로 얻은 2X2 교차표의 크레머의 파이값을 구해준다. 
phi(white.intaccess)

# 연속형 변수 혹은 서열형 변수 경우 이용하는 상관관계와 비교하면 다음과 같다. 
cor.test(pew$white, pew$intaccess, method = 'pearson')
cor.test(pew$white, pew$intaccess, method = 'spearman')
cor.test(pew$white, pew$intaccess, method = 'kendall')

