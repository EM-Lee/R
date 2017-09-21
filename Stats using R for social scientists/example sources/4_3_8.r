
# 주성분 분석
mydata <- read.csv(file="C:/data/example/example_EFA.csv",header=TRUE)
# 아래의 외부 라이브러리가 필요하다. 
library('psych')
library('GPArotation')
# 상관계수행렬을 구해 변수간 관계를 확인해본다
cor(mydata)

# 적절한 주성분의 개수를 스크리도표를 이용해서 구해본다.  
mypc <- princomp(mydata[,-16], cor=TRUE)
summary(mypc)
# 주성분의 분산은 주성분의 표준편차를 제곱하면 된다. 
summary(mypc)$sdev
# 주성분의 분산의 합은 주성분 분석에 포함된 변수의 수에 표준화된 분산 1을 곱한 값과 같다. 
sum(summary(mypc)$sdev^2)

plot(mypc,type='lines',ylim=c(0,5),main='Screeplot for PCA (Principal Component Analysis)')
abline(h=1)

mypc$loading[,1:3]


# 추출된 주성분의 회전방법에 따른 주성분분석의 차이 비교
mypc.no <- principal(mydata[,-16], nfactors = 3, rotate = 'norotate')
mypc.vm <- principal(mydata[,-16], nfactors = 3, rotate = 'varimax')
mypc.pm <- principal(mydata[,-16], nfactors = 3, rotate = 'promax')
mypc.om <- principal(mydata[,-16], nfactors = 3, rotate = 'oblimin')
# 회전없음 
mypc.no

# 직각회전(Varimax rotation)일 경우
mypc.vm

# 사각회전(oblimin rotation)
mypc.om

# 프로맥스(Promax) 회전
mypc.pm

# 회전방법에 따른 주성분 분석 결과 정리후 저장 
mypcs <- cbind(mypc.vm$scores,mypc.om$scores,mypc.pm$scores)
colnames(mypcs) <- c('CompB.V','CompC.V','CompA.V',
      'CompB.O','CompC.O','CompA.O','CompB.P','CompC.P','CompA.P')
round(cor(mypcs),2)

# 회전방법과 주성분분석으로 추출한 주성분과 탐색적 인자분석으로 추출한 인자를 이용한 회귀모형 비교는 
# 탐색적 인자분석 쪽에서 다루기로 함. 

# 탐색적 인자분석(인자의 개수는 연구자의 이론적 예측에 근거하여 결정함) 
# 주성분 분석과 달리 탐색적 인자분석은 인자의 수를 연구자가 설정해야 한다. 
# scores = ‘regression’은 회귀분석 모형을 이용해서 인자값을 추출했다는 의미이다. 
mymlefa.vm <- factanal(mydata[,-16], factors = 3, rotation = 'varimax', scores = 'regression')
mymlefa.om <- factanal(mydata[,-16], factors = 3, rotation = 'oblimin', scores = 'regression')
mymlefa.pm <- factanal(mydata[,-16], factors = 3, rotation = 'promax', scores = 'regression')
# 직각회전(Varimax rotation)
print(mymlefa.vm,cutoff=.00)

# 사각회전(Oblimin rotation)
print(mymlefa.om,cutoff=.00)

# 프로맥스 회전(Promax rotation)
print(mymlefa.pm,cutoff=.00)

# 회전방법에 따른 탐색적 인자분석 결과 정리후 저장 
myefas <- cbind(mymlefa.vm$scores,mymlefa.pm$scores,mymlefa.om$scores)
colnames(myefas) <- c('FB.V','FA.V','FC.V',
        'FB.O','FC.O','FA.O','FB.P','FA.P','FC.P')
round(cor(myefas[,c('FB.V','FA.V','FC.V',
         'FB.O','FC.O','FA.O','FB.P','FA.P','FC.P')]),2)

# 도출한 탐색적 인자분석 결과와 앞에서 언급한 주성분분석을 mydata 파일에 추가하였다. 
mydata <- data.frame(mydata,mypcs,myefas)
# 아래의 명령문은 추출된 주성분이나 인자를 사용하지 않고, 항목의 평균값을 이용하는 것을 보여준다. 
mydata$meanA <- apply(mydata[,1:5],1,mean) 
mydata$meanB <- apply(mydata[,6:10],1,mean)
mydata$meanC <- apply(mydata[,11:15],1,mean)

# 추출된 주성분, 인자, 항목의 평균값을 이용하여 OLS 회귀분석을 실시할 경우를 비교하였다. 
# 해당 주성분/인자의 효과가 서로 사뭋다른 것을 확인할 수 있다. 
model.mean.value <- lm(outcome ~ meanA + meanB + meanC, mydata)
model.PC.varimax <- lm(outcome ~ CompA.V + CompB.V + CompC.V, mydata)
model.PC.oblimin <- lm(outcome ~ CompA.O + CompB.O + CompC.O, mydata)
model.PC.promax <- lm(outcome ~ CompA.P + CompB.P + CompC.P, mydata)
model.EFA.varimax <- lm(outcome ~ FA.V + FB.V + FC.V, mydata)
model.EFA.oblimin <- lm(outcome ~ FA.O + FB.O + FC.O, mydata)
model.EFA.promax <- lm(outcome ~ FA.P + FB.P + FC.P, mydata)
# 항목의 평균값을 이용하여 outcome을 예측하는 경우
summary(model.mean.value)

# 직각회전한 주성분을 예측변수로 이용하여 outcome을 예측하는 경우
summary(model.PC.varimax)

# 사각회전한 주성분을 예측변수로 이용하여 outcome을 예측하는 경우
summary(model.PC.oblimin)

# 프로맥스 방법으로 회전한 주성분을 예측변수로 이용하여 outcome을 예측하는 경우
summary(model.PC.promax)

# 직각회전한 인자를 예측변수로 이용하여 outcome을 예측하는 경우
summary(model.EFA.varimax)

# 사각회전한 인자를 예측변수로 이용하여 outcome을 예측하는 경우
summary(model.EFA.oblimin)

# 프로맥스 방법으로 회전한 인자를 예측변수로 이용하여 outcome을 예측하는 경우
summary(model.EFA.promax)

