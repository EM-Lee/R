
#------------------------------3.3.3.6. 로지스틱 회귀분석
# 앞부분에서 나온 것은 반복하지 않았다.
# 결과변수를 SNS이용여부로 선택하고 인구통계학적 요인들과 인터넷 이용여부를
# 예측변수로 하였다. 주지하다시피 SNS이용여부는 이분변수이다.
# glm()은 Generalized Linear Model의 약자이며, 이를통해 로지스틱 회귀분석은 물론
# 포아송 회귀분석이나 음이항 회귀분석도 가능하다. 물론 OLS를 glm()으로 하는 것도
# 가능하다. family 옵션은 결과변수의 분포가 이항분포(binomial distribution) 임을
# 보여주며, 링크함수로 logit함수를 사용함을 의미한다. 'probit'이라고 하는 경우 프로빗
# 함수를 이용한다.
obj.logit.1 <- glm(sns.user ~ female + black + hispanic + other.race + age.m.c,
    data = pew.listwise, family = binomial(link='logit'))
summary(obj.logit.1)
obj.logit.2 <- glm(sns.user ~ female + black + hispanic + other.race + age.m.c 
    + educ.m.c + inc.m.c,data = pew.listwise, family = binomial(link='logit'))
summary(obj.logit.2)
# 두번째 모형이 첫번째 모형의 설명력을 개선했는지는 아래와 같이 테스트할 수 있다.  
anova(obj.logit.1,obj.logit.2)
1 - pchisq(anova(obj.logit.1,obj.logit.2)$Deviance[2],anova(obj.logit.1,obj.logit.2)$Df[2])

# 로짓분석의 회귀계수 해석은 회귀계수의 지수함수값을 이용하기 때문에 해석의 용이성을
# 위해서 약간의 프로그래밍을 가미했다. 앞에서의 OLS와 본질적으로 같다.
logit.table.1 <- round(cbind(summary(obj.logit.1)$coef, exp(summary(obj.logit.1)$coef[,1])),3)
colnames(logit.table.1) <- c('Coef','S.E.','Z.value','P.value','Odds.Ratio')
logit.table.1
logit.table.2 <- round(cbind(summary(obj.logit.2)$coef, exp(summary(obj.logit.2)$coef[,1])),3)
colnames(logit.table.2) <- c('Coef','S.E.','Z.value','P.value','Odds.Ratio')
logit.table.2

# 결과를 도표로 그리기
# 앞에서 설명한 것과 동일하다. 단 로지스틱 모형은 선형이 아니기 때문에 연령의 최소-최대값을
# 넣지 않고, 연령의 실제 범위인 18~97을 1년 단위로 이용하였다.
pred.pew.age <- pew.listwise[18:97,]
pred.pew.age$age.m <- 18:97
pred.pew.age$female <- mean(pew.listwise$female)
pred.pew.age$black <- mean(pew.listwise$black)
pred.pew.age$hispanic <- mean(pew.listwise$hispanic)
pred.pew.age$other.race <- mean(pew.listwise$other.race)
pred.pew.age$inc.m.c <- mean(pew.listwise$inc.m.c)
pred.pew.age$educ.m.c <- mean(pew.listwise$educ.m.c)
pred.pew.age$age.m.c <- pred.pew.age$age.m - mean(pew.listwise$age.m)
# type = 'response'는 예측값을 결과변수의 형태, 즉 확률의 형태로 보고하라는 의미다.
# se.fit은 표준오차를 같이 보고하라는 의미이다.
logit.pew.age <- predict.glm(obj.logit.2, pred.pew.age, type = 'response', se.fit = TRUE)
# 예측점을 별도의 오브젝트화한다.
pred.pew.age$fit <- logit.pew.age$fit
# 예측점의 95% 신뢰구간의 상한과 하한을 각각 오브젝트화한다.
pred.pew.age$lwr <- logit.pew.age$fit - 1.96*logit.pew.age$se.fit
pred.pew.age$upr  <- logit.pew.age$fit + 1.96*logit.pew.age$se.fit
# 앞서의 방식으로 그림을 그렸다.
plot(pred.pew.age$age.m, pred.pew.age$fit, ylim = c(0, 1),
    type = 'l', lwd = 3, col = 'blue',
    ylab = "predicted probability to use SNS", xlab = "Age, year",
    main = "The effect of age years on respondents' probability to use SNS ")
points(pred.pew.age$age.m, pred.pew.age$lwr, type = 'l', lty = 2, col = 'blue')
points(pred.pew.age$age.m, pred.pew.age$upr, type = 'l', lty = 2, col = 'blue')


#-------------------------------각주
x <- -600:600/100
y <- 1/(1+exp(-x))
plot(x, y, type='l', col = 'blue', 
    xlab='x', ylab='F(x)', main = 'Logistic function')

mylogit <- log(y/(1-y))
plot(x, mylogit, type='l', col = 'blue', 
    xlab='x', ylab='Logit', main = 'Transformed by Logit function')

#-------------------------------각주
# 로지스틱 회귀분석의 임의R2 계산 함수
LogisticPseudoR2 <- function(logistic_object1, logistic_object2, mydigit) {
    obj1.model <- -2*logLik(logistic_object1)
    obj1.null <- summary(logistic_object1)$null.deviance
    obj2.model <- -2*logLik(logistic_object2)
    obj2.null <- summary(logistic_object2)$null.deviance
    obj1.size <- summary(logistic_object1)$df.null + 1
    obj2.size <- summary(logistic_object1)$df.null + 1
    obj1.df <- summary(logistic_object1)$df[2]
    obj2.df <- summary(logistic_object2)$df[2]
    obj1.MCFR2 <- 1 - (obj1.model/obj1.null)
    obj2.MCFR2 <- 1 - (obj2.model/obj2.null)
    obj1.CSR2 <- 1 - exp((obj1.model-obj1.null)/obj1.size)
    obj2.CSR2 <- 1 - exp((obj2.model-obj2.null)/obj2.size)
    obj1.NGKR2 <- obj1.CSR2/(1-exp(-1*obj1.null/obj1.size))
    obj2.NGKR2 <- obj2.CSR2/(1-exp(-1*obj2.null/obj2.size))
    delta.chi <- obj1.model - obj2.model
    delta.df <- obj1.df - obj2.df
    mypval <- 1 - pchisq(delta.chi, delta.df)
    finalreport <- round(rbind(
    c(obj1.MCFR2,obj2.MCFR2,(obj2.MCFR2-obj1.MCFR2)),
    c(obj1.CSR2,obj2.CSR2,(obj2.CSR2-obj1.CSR2)),
    c(obj1.NGKR2,obj2.NGKR2,(obj2.NGKR2-obj1.NGKR2))),mydigit)
    teststat <- paste('Chi2 = ',round(delta.chi,mydigit),", df = ", delta.df, ",", " p = ", round(mypval, mydigit), sep='')
    colnames(finalreport) <- c('Model1','Model2','Model.difference')
    rownames(finalreport) <- c('McFadden R2','Cox&Snell R2','Negelkerke R2')
    myfinalreport <- list(finalreport,teststat)
    myfinalreport
}
LogisticPseudoR2(obj.logit.1, obj.logit.2, 3)

