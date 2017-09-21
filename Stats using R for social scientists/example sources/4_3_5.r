# 4부. 통계분석
# 3장. 다변량 추리통계분석
# 5. 다변량 일반회귀분석

# 다변량 회귀분석 (multivariate odrindary least square regression)
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# 결과변수로 주관적인 삶의 질 평가, 그리고 예측변수로는 응답자의 인구통계학적 변수들(성별(여성),
# 인종(소수인종), 연령, 교육수준, 소득수준, SNS 이용여부를 이용하였다.
# 우선 삶의 질의 경우 8과 9는 각각 Don't know와 Refusal로 결측값처리하였다. 숫자가 클수록
# 삶에 대해 만족하는 응답이 나오도록 역코딩도 하였다.
table(pew$q1, useNA = 'always')
pew$life.quality <- pew$q1
pew$life.quality[pew$q1 ==8 | pew$q1 == 9] <- NA
pew$life.quality <- 6 - pew$life.quality
# 성별의 경우 이분변수로 만들고, 여성이면 1, 남성이면 0으로 코딩하였다.
table(pew$sex, useNA = 'always')
pew$female <- 0
pew$female[pew$sex == 2] <- 1
# 인종의 경우 히스패닉이 아닌 백인, 히스패닉이 아닌 흑인, 히스패닉, 히스패닉이 아닌 기타집단으로
# 4개 집단을 생성하였다. 준거집단은 '히스패닉이 아닌 백인'으로 설정하였다.  
# 8과 9는 각각 Don't know와 Refusal로 결측값처리하였다.
table(pew$race, pew$hisp, useNA = 'always')
pew$race4 <- 4
pew$race4[pew$race==1 & pew$hisp==2] <- 1
pew$race4[pew$race==2 & pew$hisp==2] <- 2
pew$race4[pew$hisp==1] <- 3
pew$race4[pew$hisp==8|pew$hisp==9|pew$race==8|pew$race==9] <- NA
# R의 경우 factor로 정의된 변수를 회귀분석에 투입하면, 
# 첫번째 집단을 준거집단으로 하는 이분변수군으로 모형에 투입된다.  
pew$race4 <- factor(pew$race4,labels=c('white','black','hispanic','other races'))
table(pew$race4,useNA='always')
# 그러나 여기서는 연습을 위해 black, hispanic, other.race의 세 이분변수를 생성하기로 한다. 
pew$black <- 1
pew$hispanic <- 1
pew$other.race <- 1
pew$black[pew$race4 != 'black'] <- 0
pew$hispanic[pew$race4 != 'hispanic'] <- 0
pew$other.race[pew$race4 != 'other races'] <- 0
pew$black[is.na(pew$race4)] <- pew$hispanic[is.na(pew$race4)] <- pew$other.race[is.na(pew$race4)] <- NA
table(pew$black, pew$race4, useNA='always')
table(pew$hispanic, pew$race4, useNA='always')
table(pew$other.race, pew$race4, useNA='always')
# 연령의 경우, 연속형 변수로 이용하지만, 98과 99의 경우 Don't know와 Refusal로 결측값처리하였다
table(pew$age, useNA = 'always')
pew$age.m <- pew$age
pew$age.m[pew$age >= 98] <- NA
# 아래는 중심화변환(centering)을 실시한 것이다.
pew$age.m.c <- pew$age.m - mean(pew$age.m,na.rm=T)
# 교육수준의 경우 서열형 변수이지만 연속형 변수로 가정하였다. 8과 9는 각각 Don't know와 Refusal로
# 결측값처리하였다.
table(pew$educ, useNA = 'always')
pew$educ.m <- pew$educ
pew$educ.m[pew$educ >= 8] <- NA
pew$educ.m.c <- pew$educ.m - mean(pew$educ.m,na.rm=T)
# 소득수준의 경우도 서열형 변수이지만 연속형 변수로 가정하였다. 98과 99는 각각 Don't know와 Refusal로
# 결측값처리하였다.
table(pew$inc, useNA = 'always')
pew$inc.m <- pew$inc
pew$inc.m[pew$inc >= 98] <- NA
pew$inc.m.c <- pew$inc.m - mean(pew$inc.m,na.rm=T)
# SNS이용여부는 이용자이면 1, 비이용자이면 0으로 코딩하였다. 98과 99는 각각 Don't know와 Refusal로
# 결측값처리하였다.
table(pew$q17m1, useNA = 'always')
pew$sns.user <- 0
pew$sns.user[pew$q17m1 >= 98] <- NA
pew$sns.user[pew$q17m1 <= 12] <- 1
# 분석에 이용되는 변수들을 리스트와이즈(listwise) 방식으로 결측값을 갖는 사례들을 제거한다.
pew.listwise <- pew[complete.cases(pew[,c('female','race4','age.m.c','educ.m.c','inc.m.c','sns.user','life.quality')]),]
# 보통의 통계분석 서적에서는 산점도를 이용하여 예측변수와 결과변수의 관계를 살펴볼 것을 권장한다.
# 아쉽게도 순위형 자료에 가까운 사회과학자료의 특성으로 산점도를 통해 관계를 살펴보는 것이 쉽지 않은 경우가 많다.
# 그러나 jitter() 함수를 이용하면 어느정도 시각적 데이터 이해가 가능하다. 지터는 데이터에 오차를 넣어
# 여러데이터를 산포시키는 기능을 한다. jitter() 대신 앞에서 소개한 hexbin()을 이용하는 것도 한가지 방법이다.
# 중심화변환은 단지 평균값만을 이동하는 것이기에 해석의 편이성을 위해 여기서는 중심화변환 이전값을 이용했다.
par(mfrow=c(1,3))
plot(jitter(pew.listwise$age.m,factor=2),jitter(pew.listwise$life.quality,factor=2),
    xlab = 'age, years', ylab = 'Subjectively felt life quality', main = 'Life quality & Age, years')
plot(jitter(pew.listwise$educ.m,factor=2),jitter(pew.listwise$life.quality,factor=2),
    xlab = 'Education level', ylab = 'Subjectively felt life quality', main = 'Life quality & Education level')
plot(jitter(pew.listwise$inc.m,factor=2),jitter(pew.listwise$life.quality,factor=2),
    xlab = 'Income level', ylab = 'Subjectively felt life quality', main = 'Life quality & Income level')
# 예측변수간 상관관계를 구해본다.
round(cor(pew.listwise[,c('life.quality','female','black','hispanic','other.race','age.m.c',
  'educ.m.c','inc.m.c','sns.user')]),2)
# 다음으로 결과변수의 분포를 히스토그램을 이용하여 시각적으로 확인해 보았다.   
hist(pew.listwise$life.quality, breaks=5, ylab='Frequency', 
  xlab='Subjectively felt life quality',main='Distribution of outcome variable')
# 선형회귀분석 모형을 실행하여 각각 오브젝트화한다.
obj.ols.1 <- lm(life.quality ~ female + black + hispanic + other.race + age.m.c + I(age.m.c^2) 
    + educ.m.c + sns.user, data = pew.listwise)
summary(obj.ols.1)
# 상호작용을 테스트하는 것도 가능하다. 특별히 상호작용항을 추가로 만들필요가 없다.
# 다음은 SNS 이용여부에 따라 인구통계학적 속성과  life.quality의 관계가 달라지는지 여부를 테스트한 것이다.
obj.ols.2 <- lm(life.quality ~ (female + black + hispanic + other.race + age.m.c + I(age.m.c^2) 
    + educ.m.c + inc.m.c)*sns.user, data = pew.listwise)
summary(obj.ols.2)
# 모형의 적합도를 테스트할 수도 있다. 분산분석에서 사용했던 anova()를 이용하면 된다.
# 언급하였듯, GLM의 관점에서 ANOVA와 REGRESSION은 수학적으로 동등한 모형이다. 
anova(obj.ols.1, obj.ols.2)

# 예측변수가 결과변수에 미치는 효과크기는 표준화 회귀계수를 이용하여 비교할 수 있다.  
library('QuantPsyc')
round(lm.beta(obj.ols.1),3)
round(lm.beta(obj.ols.2),3)

# 아래와 같은 방식으로 회귀계수관련 정보를 종합하여 살펴볼 수 있다.
ols.table <- round(cbind(summary(obj.ols.1)$coef, c(NA,lm.beta(obj.ols.1))),2)
colnames(ols.table) <- c('Unstd.Coef','S.E.','T.value','P.value','Std.Coef')
ols.table

# 비표준화 및 표준화 계수관련 정보는 write.csv() 함수를 이용하여 별개화일로 배출할 수 있다
write.csv(ols.table, file = "C:/data/example/OLS_R_introduction_book.csv", row.names=FALSE) 

# 연령의 효과를 도표로 그리기
# 우선 최저연령과 최고연령을 표현할 수 있는 테이터를 가상으로 하나 복제한다. 
pred.pew.age <- pew.listwise[18:97,]
pred.pew.age$age.m.c <- 18:97 - mean(pew.listwise$age.m)
pred.pew.age$age.m <- 18:97
# 성별과 인종, 교육수준, 소득, SNS이용여부를 다음과 같이 표본의 평균값으로 대체하여 통제한다.  
# 중심화 변환을 실시하였던 연속형 변수는 0으로 대치하면 되지만, 아래와 같이 표본의 평균값인 0으로 대치하였다. 
pred.pew.age$female <- mean(pew.listwise$female)
pred.pew.age$black <- mean(pew.listwise$black)
pred.pew.age$hispanic <- mean(pew.listwise$hispanic)
pred.pew.age$other.race <- mean(pew.listwise$other.race)
pred.pew.age$educ.m.c <- mean(pew.listwise$educ.m.c)
pred.pew.age$inc.m.c <- mean(pew.listwise$inc.m.c)
pred.pew.age$sns.user <- mean(pew.listwise$sns.user)
# 연령이 17부터 18까지 움직인다고 할 때, 앞에서 도출한 첫번째 OLS 회귀방정식에 따른
# 예측값은 아래와 같이 확인할 수 있다.
pred.life.quality <- predict(obj.ols.1, pred.pew.age, interval = 'confidence')
pred.life.quality
pred.pew.age <- data.frame(pred.pew.age,pred.life.quality)
# 연령에 따라 주관적 삶의 만족도가 어떻게 변화하는지는 다음과 같은 플롯을 이용해서 구할 수 있다.  
plot(pred.pew.age$age.m,pred.pew.age$fit, ylim = c(3,4.5),
    type = 'l', lwd = 3, lty = 1,  col = 'blue',
    xlab = "Respondents' age, years", ylab = "Perceived life quality (OLS regression prediction)",
    main = "Quadratic relationship between perceived life quality and respondents' age")
# 예측선의 95% 신뢰구간을 표시하면 다음과 같다.  
points(pred.pew.age$age.m,pred.pew.age$lwr,type = 'l', lty = 2, col = 'blue')
points(pred.pew.age$age.m,pred.pew.age$upr,type = 'l', lty = 2, col = 'blue')

# 상호작용 효과 패턴을 도표로 그리기
pred.pew.educ <- pew.listwise[1:4,]
pred.pew.educ$female <- mean(pew.listwise$female)
pred.pew.educ$black <- mean(pew.listwise$black)
pred.pew.educ$hispanic <- mean(pew.listwise$hispanic)
pred.pew.educ$other.race <- mean(pew.listwise$other.race)
# 사실 중심화 변환을 실시하면 아래 두줄의 명령문, 대신 0을 투입한 것과 동일한 효과가 있다.  
pred.pew.educ$inc.m.c <- mean(pew.listwise$inc.m.c)
pred.pew.educ$age.m.c <- mean(pew.listwise$age.m.c)
pred.pew.educ$sns.user <- c(0,0,1,1)
pred.pew.educ$educ.m.c <- c(1,7,1,7) - mean(pew.listwise$educ.m)
# 교육수준이 최소-최대로 변화할 때, 그리고 SNS 이용상태에 따라 앞에서 도출한 OLS 방정식에 따른
# 예측값은 아래와 같이 확인할 수 있다.
pred.life.quality <- predict(obj.ols.2, pred.pew.educ, interval = 'confidence')
pred.life.quality
pred.pew.educ <- data.frame(pred.pew.educ,pred.life.quality)
# 우선 SNS를 이용하지 않는 경우, 교육수준(최소-최대)과 회귀분석모형예측값의 선형관계
plot(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==0],
    pred.pew.educ$fit[pred.pew.educ$sns.user==0], ylim = c(2.5,3.5),
    type = 'l', lwd = 3, lty = 1,  col = 'red',
    xlab = "Education level (centered)", ylab = "Perceived life quality (OLS regression prediction)",
    main = 'Interaction effect between education and SNS use status\non perceived life quality')
# 우선 SNS를 이용하는 경우, 교육수준(최소-최대)과 회귀분석모형예측값의 선형관계
points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==1],
    pred.pew.educ$fit[pred.pew.educ$sns.user==1],
    type = 'l', lwd = 3, lty = 1, col = 'blue')
legend('bottomright',legend=c('Non SNS-users','SNS users'),col=c('red','blue'),lty=1, lwd=3)
# 아래의 명령문을 이용하면 예측선의 95% 신뢰구간을 표시할 수 있다.
# 그러나 집단이 여럿일 경우 그래프가 복잡해져서 정보전달 능력이 떨어진다고 생각하기에 그리지는 않았다.
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==0],
#     pred.pew.educ$lwr[pred.pew.educ$sns.user==0],type = 'l', lty = 2, col = 'red')
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==0],
#     pred.pew.educ$upr[pred.pew.educ$sns.user==0],type = 'l', lty = 2, col = 'red')
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==1],
#     pred.pew.educ$lwr[pred.pew.educ$sns.user==1], type = 'l', lty = 2, col = 'blue')
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==1],
#     pred.pew.educ$upr[pred.pew.educ$sns.user==1], type = 'l', lty = 2, col = 'blue')


# 모형진단
# 이상치 검증 (상호작용 효과항을 고려한 경우만 예로 소개한다)
cooksd <- cooks.distance(obj.ols.2)
cook.threshold <- 4/(dim(pew.listwise)[1] - length(obj.ols.2$coef))
plot(cooksd)
abline(h = cook.threshold)
outlier <- rep(0,length(cooksd))
# 아래와 같이 이상치에 해당되는 사례의 숫자를 계산할 수 있다. 총 99개(약 6%)가 이상치에 해당되는 것으로 나온다  
outlier[cooksd > cook.threshold] <- 1
table(outlier)

# 정규성 진단
# 'car' 라이브러리가 있으면 매우 유용하다 
library('car')
par(mfrow=c(1,2))
qqPlot(obj.ols.1)
qqPlot(obj.ols.2)

# 유의도 검증 방식을 이용한 이분산성 진단
library('lmtest')
bptest(obj.ols.1)
bptest(obj.ols.2)

# 이분산성(heteroskedascity) 진단(모형 1만 고려함)
par(mfrow=c(2,2))
plot(fitted.values(obj.ols.1), resid(obj.ols.1))
plot(pew.listwise$age.m, resid(obj.ols.1))
plot(pew.listwise$educ.m, resid(obj.ols.1))
plot(pew.listwise$inc.m, resid(obj.ols.1))

# 독립성 검증: 다중공선성(multi-collinearity)
# 분산팽창지수를 구하는 방식은 다음과 같다.
round(vif(obj.ols.1),2)
round(vif(obj.ols.2),2)
# 흔히 용인도(tolerance)로 알려진 값도 구할 수 있다.
round(1/vif(obj.ols.1),2)
round(vif(obj.ols.2),2)
