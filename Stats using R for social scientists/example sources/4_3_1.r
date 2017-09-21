# 4부. 통계분석
# 3장. 다변량 추리통계분석
# 1. t-검증 

# TESS 데이터를 불러들이는 것은 앞에서 이미 설명하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# 분석의 대상이 되는 XIDEO 변수의 빈도표를 살펴보았다. 13개의 사례는 무응답으로 결측값 처리되어 있다. 
table(tess$XIDEO, useNA = 'always')
# t.test() 는 오브젝트를 t-검증하는 명령문이다. mu 는 단일표본 t-검증의 모수평균을 의미한다. 
t.test(tess$XIDEO, mu = 4)

# 앞에서 반복된 부분의 경우 설명을 생략하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# 언급한 두 변수의 빈도표를 살펴보면 무응답, 즉 -1에 해당되는 사례들이 각각 26, 32만큼 존재한다. 
# 이들을 결측값으로 처리하였다.  
table(tess$Q4, useNA = 'always')
table(tess$Q8, useNA = 'always')
tess$Q4r <- tess$Q4 
tess$Q8r <- tess$Q8
tess$Q4r[tess$Q4 == -1 ] <- NA
tess$Q8r[tess$Q8 == -1 ] <- NA
# 각 변수들의 평균과 표준편차를 구체적으로 살펴보면 아래와 같다. 
mean(tess$Q4r, na.rm = TRUE); sd(tess$Q4r, na.rm = TRUE)
mean(tess$Q8r, na.rm = TRUE); sd(tess$Q8r, na.rm = TRUE)
# 결측값으로 처리된 Q4r과 Q8r 을 대응표본 t-검증으로 평균값의 차이를 테스트하였다.  
# paired 옵션의 경우 해당 t-검증이 대응표본 t-검증임을 밝혀주는 역할을 한다. 
# 만약 paired = FALSE일 경우 아래에 설명될 독립표본 t-검증이 실행된다. 
t.test(tess$Q4r, tess$Q8r, paired = TRUE)

# 차이점수(difference score)를 이용해서 단일표본 t-검증으로 테스트할 수도 있다. 
# 여기서 모수평균은 0으로 설정되어야 한다. 그 이유는 귀무가설에 따르면 차이점수가 없어야 하기 때문이다. 
tess$diff.Q4r.Q8r <- tess$Q4r - tess$Q8r
t.test(tess$diff.Q4r.Q8r, mu = 0)

# 앞에서 반복된 부분의 경우 설명을 생략하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# 언급하였듯 Q8에는 무응답이 있으며, 이를 결측값으로 처리해야 한다.  
table(tess$Q8, useNA = 'always')
tess$Q8r <- tess$Q8
tess$Q8r[tess$Q8 == -1 ] <- NA
# 예측변수를 여성이미지가 삽입된 경우(13∼24)와 남성이미지가 제시된  경우(1∼12)로 나누어 
# 남성이미지의 경우는 0, 여성이미지의 경우는 1로 코딩하였다.  
table(tess$XTESS089, useNA = 'always')
tess$other.female <- NA
tess$other.female[tess$XTESS089 >= 13] <- 1 
tess$other.female[tess$XTESS089 <= 12] <- 0
# 우선 두 집단간 평균(표준편차)의 기술통계치를 살펴보았다. aggregate()는 앞에서 설명한 바 있다. 
aggregate(Q8r ∼ other.female, data=tess, FUN = mean)
aggregate(Q8r ∼ other.female, data=tess, FUN = var)
# bartlett.test()는 두 집단별로 결과변수인 Q8r의 분산동질성을 검증한다. '결과변수 ∼ 예측변수'의 형태이며, data에는 변수가 속한 데이터셋의 이름을 넣어주면 된다.  
bartlett.test(Q8r ∼ other.female, data=tess)
# 비모수통계기법(nonparametric technique)으로써 fligner.test(Q8r ∼ other.female, data=tess)를 
# 이용할 수도 있다. 모수통계기법과 비모수통계기법의 차이는 해당 전공서적을 참조하라.  
# t.test()를 이용해서 독립표본 t-검증을 실시하였다.  분산동질성 검증 결과 두 집단의 분산이 통계적으로 
# 유의미하게 다르기 때문에, var.equal을 FALSE로 설정해야 한다.  
t.test(Q8r ∼ other.female, var.equal = FALSE, data=tess)
# 만약 분산동질성을 이론적으로 가정할 경우 var.equal을 TRUE로 설정하면 된다. 앞의 결과와 비교하면 
# 분산동질성을 가정할 때 자유도가 더 크게 나타나는 것을 확인할 수 있다.  
t.test(Q8r ∼ other.female, var.equal = TRUE, data=tess)

################################---- 각주 
# 샤피로-윌크의 정규성 검증 
shapiro.test(tess$XIDEO)

# car 라이브러리를 실행한다. 만약 'car'가 설치되어 있지 않으면 
# install.packages('car')를 통해 설치한 후 라이브러리를 구동시킨다.  
library('car')
# 르빈의 분산동질성 검증은 leveneTest()로 가능하다. 
# 명령문의 구조는 bartlett.test()와 유사하다. 주의할 점은 
# 르빈의 검증은 조금 후에 소개될 분산분석에 기반하기 때문에 예측변수를 요인으로 
# 설정하여야 한다는 점이다. as.factor() 에 대해서는 앞에서 설명한 바 있다.  
leveneTest(Q8r ∼ as.factor(other.female), data=tess)

# set.seed() 명령문은 무작위로 추출되는 사례의 시작점을 고정시키는 역할을 한다. 
# 만약 해당명령문이 없으면 여기서 나온 결과와 다른 결과를 얻을 수도 있다.  
set.seed(2013)
# 평균이 4이며 표준편차가 1인 집단0와 평균이 5이며 표준편차가 4인 집단1을 
# 각각 10개씩 추출하도록 시뮬레이션하였다.  
sim.group0 <- rnorm(10, mean = 4, sd = 1)
sim.group1 <- rnorm(10, mean = 5, sd = 4)
# 분산동질성을 가정한 경우와 가정하지 않은 경우 독립표본 t-검증을 실시하였다.  
t.test(sim.group0, sim.group1, paired = FALSE, var.equal = TRUE)
t.test(sim.group0, sim.group1, paired = FALSE, var.equal = FALSE)
 