

# ------------------ 3.3.3.7.1. 크론바흐의 알파(Cronbach’s )
# 여러문항간의 내적일치도를 계산하는 크론바흐의 알파(Cronbach's alpha)를 편하게 계산할 수
# 있는 라이브러리로 psych이 있다. 해당 라이브러리가 설치되어 있지 않으면
# install.packages()로 먼저 psych 설치하자.
library('psych')
# tess 데이터를 불러들인 후 고통을 느끼는 상황 네가지별로 자신이 느끼는 고통과 타인이 느끼는
# 고통의 경우를 각각 코론바흐 알파를 이용해서 계산해보자.
# 앞에서 반복된 부분의 경우 설명을 생략하였다.
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav",
use.value.labels = FALSE, to.data.frame = TRUE)
# 우선 자신이 느끼게 될 고통은 Q1~Q4까지이다. 타인이 느끼게 될 고통의 경우 Q5~Q8이다.
# 해당 상황에 대해서는 데이터와 함께 첨부된 설문지를 참조하자. -1의 경우 결측값처리하였다.
table(tess$Q1, useNA = 'always')
table(tess$Q2, useNA = 'always')
table(tess$Q3, useNA = 'always')
table(tess$Q4, useNA = 'always')
table(tess$Q5, useNA = 'always')
table(tess$Q6, useNA = 'always')
table(tess$Q7, useNA = 'always')
table(tess$Q8, useNA = 'always')
missingvalue.Q1Q8 <- c(NA,0:10)
tess$q1r <- missingvalue.Q1Q8[tess$Q1+2]
tess$q2r <- missingvalue.Q1Q8[tess$Q2+2]
tess$q3r <- missingvalue.Q1Q8[tess$Q3+2]
tess$q4r <- missingvalue.Q1Q8[tess$Q4+2]
tess$q5r <- missingvalue.Q1Q8[tess$Q5+2]
tess$q6r <- missingvalue.Q1Q8[tess$Q6+2]
tess$q7r <- missingvalue.Q1Q8[tess$Q7+2]
tess$q8r <- missingvalue.Q1Q8[tess$Q8+2]
# 크론바흐 알파값을 구하기 위해서는 응답자(R)X항목(C)로 된 데이터셋을 구축할 필요가 있다.
# 인덱싱을 이용하면 상당히 편하다.
# 우선 자신이 느끼게될 고통의 경우는 아래와 같다. 우선 상관계수를 살펴보고 크론바흐 알파를 구했다
alpha(tess[,c('q1r','q2r','q3r','q4r')], na.rm = TRUE)
# 타인이 느끼게 될 고통의 경우는 다음과 같다.
alpha(tess[,c('q5r','q6r','q7r','q8r')], na.rm = TRUE)
# 그렇다면 자신과 타인이 느끼게 될 고통과 관련된 8개 항목 전체를 해본면 어떨까?
alpha(tess[,c('q1r','q2r','q3r','q4r','q5r','q6r','q7r','q8r')], na.rm = TRUE)


#------ 3.3.3.7.2. 코더간 신뢰도: 코헨의 카파와 크리펜도르프의 알파
# 코더간 신뢰도 계산을 위해서는 irr 라이브러리가 유용하다.
# irr 라이브러리가 설치되어 있지 않으면 install.packages('irr')을 실행하면 된다.
library('irr')
# 아래는 Krippendorff (1980)에 실려있는 사례이다. 총 12개를 대상으로 4명의 코더가
# 내용분석을 실시했다. NA는 해당코더의 내용분석결과가 결측값으로 된 결과이다.
# 크리펜도르프 알파의 경우 코더가 가로줄에, 사례가 세로줄에 배치되어야 한다. 
content <- matrix(c(1,1,NA,1,2,2,3,2,3,3,3,3,3,3,3,3,2,2,2,2,1,2,3,4,4,4,4,4,
1,1,2,1,2,2,2,2,NA,5,5,5,NA,NA,1,1,NA,NA,3,NA), nrow=4)
colnames(content) <- paste('obs',1:12,sep='')
rownames(content) <- paste('coder',1:4,sep='')
content
# 각 코더별로 어떻게 내용분석을 했는지를 살펴보면 아래와 같다.
table(content[1,], useNA = 'always')
table(content[2,], useNA = 'always')
table(content[3,], useNA = 'always')
table(content[4,], useNA = 'always')
# 만약 내용분석의 결과를 명목형 변수라고 가정하면 다음과 같다.
kripp.alpha(content, method = 'nominal')
# 서열형 변수라고 가정하면 아래와 같다.
kripp.alpha(content, method = "ordinal")
# 등간형 변수로 가정하면 아래와 같다.
kripp.alpha(content, method = "interval")
# 연속형 변수라고 가정하면 아래와 같다.
kripp.alpha(content, method = "ratio")
# 4명중 3명이 판단을 내리지 못한(즉 결측값인) 12번째의 현상은 계산에 포함되지 못한다.
# 아래의 경우 12번째 현상을 제외했다. 결과는 위와 동일하다.
kripp.alpha(content[,1:11], method = 'nominal')
kripp.alpha(content[,1:11], method = "ordinal")
kripp.alpha(content[,1:11], method = "interval")
kripp.alpha(content[,1:11], method = "ratio")
# 그러나 4명중 2명이 판단을 내리지 못하더라도 다른 2명이 판단을 내린 11번째의 현상은 계산이 포함된다.
# 아래의 경우 12번째, 그리고 11번째 현상을 제외했다. 결과가 달라지는 것을 확인하라.
kripp.alpha(content[,1:10], method = 'nominal')
kripp.alpha(content[,1:10], method = "ordinal")
kripp.alpha(content[,1:10], method = "interval")
kripp.alpha(content[,1:10], method = "ratio")  
# 개인적으로 권하지는 않지만 코헨의 카파, 라이트의 카파, 플라이스의 카파는 다음과 같이 구할 수 있다. 
# 우선 언급하였듯 코헨의 카파는 오직 2명의 코더간 신뢰도를 구할 수 있다. 
# 크리펜도르프 알파와 데이터 형식이 다르다. 카파의 경우 사례가 가로줄에, 코더가 세로줄에 배치된다. 
t.content <- t(content)
t.content
kappa2(t.content[,c(1,2)])  
kappa2(t.content[,c(1,3)])  
kappa2(t.content[,c(1,4)])  
kappa2(t.content[,c(2,3)])  
kappa2(t.content[,c(2,4)])  
kappa2(t.content[,c(3,4)])  
# 아까 언급했지만, 라이트의 카파와 플라이스의 카파의 경우 결측값을 리스트 제거 방식으로 제거한다. 
# 라이트와 플라이스의 카파를 이용하여 코더간 신뢰도를 계산할 때 포함되는 사례수는 8이라는 점에 주목하라. 
kappam.light(t.content)
kappam.fleiss(t.content)
# 결측값을 제거한 후 코딩결과가 명목형 변수라고 가정한 후 크리펜도르프 알파를 구하면 
# 그 값이 매우 유사하게  나오는 것을 확인할 수 있다.  
kripp.alpha(content[,2:9],method='nominal')
