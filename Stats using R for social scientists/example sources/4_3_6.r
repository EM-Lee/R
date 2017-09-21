
#------------------------------3.3.3.6. ������ƽ ȸ�ͺм�
# �պκп��� ���� ���� �ݺ����� �ʾҴ�.
# ��������� SNS�̿뿩�η� �����ϰ� �α�������� ���ε�� ���ͳ� �̿뿩�θ�
# ���������� �Ͽ���. �����ϴٽ��� SNS�̿뿩�δ� �̺к����̴�.
# glm()�� Generalized Linear Model�� �����̸�, �̸����� ������ƽ ȸ�ͺм��� ����
# ���Ƽ� ȸ�ͺм��̳� ������ ȸ�ͺм��� �����ϴ�. ���� OLS�� glm()���� �ϴ� �͵�
# �����ϴ�. family �ɼ��� ��������� ������ ���׺���(binomial distribution) ����
# �����ָ�, ��ũ�Լ��� logit�Լ��� ������� �ǹ��Ѵ�. 'probit'�̶�� �ϴ� ��� ���κ�
# �Լ��� �̿��Ѵ�.
obj.logit.1 <- glm(sns.user ~ female + black + hispanic + other.race + age.m.c,
    data = pew.listwise, family = binomial(link='logit'))
summary(obj.logit.1)
obj.logit.2 <- glm(sns.user ~ female + black + hispanic + other.race + age.m.c 
    + educ.m.c + inc.m.c,data = pew.listwise, family = binomial(link='logit'))
summary(obj.logit.2)
# �ι�° ������ ù��° ������ ������� �����ߴ����� �Ʒ��� ���� �׽�Ʈ�� �� �ִ�.  
anova(obj.logit.1,obj.logit.2)
1 - pchisq(anova(obj.logit.1,obj.logit.2)$Deviance[2],anova(obj.logit.1,obj.logit.2)$Df[2])

# �����м��� ȸ�Ͱ�� �ؼ��� ȸ�Ͱ���� �����Լ����� �̿��ϱ� ������ �ؼ��� ���̼���
# ���ؼ� �ణ�� ���α׷����� �����ߴ�. �տ����� OLS�� ���������� ����.
logit.table.1 <- round(cbind(summary(obj.logit.1)$coef, exp(summary(obj.logit.1)$coef[,1])),3)
colnames(logit.table.1) <- c('Coef','S.E.','Z.value','P.value','Odds.Ratio')
logit.table.1
logit.table.2 <- round(cbind(summary(obj.logit.2)$coef, exp(summary(obj.logit.2)$coef[,1])),3)
colnames(logit.table.2) <- c('Coef','S.E.','Z.value','P.value','Odds.Ratio')
logit.table.2

# ����� ��ǥ�� �׸���
# �տ��� ������ �Ͱ� �����ϴ�. �� ������ƽ ������ ������ �ƴϱ� ������ ������ �ּ�-�ִ밪��
# ���� �ʰ�, ������ ���� ������ 18~97�� 1�� ������ �̿��Ͽ���.
pred.pew.age <- pew.listwise[18:97,]
pred.pew.age$age.m <- 18:97
pred.pew.age$female <- mean(pew.listwise$female)
pred.pew.age$black <- mean(pew.listwise$black)
pred.pew.age$hispanic <- mean(pew.listwise$hispanic)
pred.pew.age$other.race <- mean(pew.listwise$other.race)
pred.pew.age$inc.m.c <- mean(pew.listwise$inc.m.c)
pred.pew.age$educ.m.c <- mean(pew.listwise$educ.m.c)
pred.pew.age$age.m.c <- pred.pew.age$age.m - mean(pew.listwise$age.m)
# type = 'response'�� �������� ��������� ����, �� Ȯ���� ���·� �����϶�� �ǹ̴�.
# se.fit�� ǥ�ؿ����� ���� �����϶�� �ǹ��̴�.
logit.pew.age <- predict.glm(obj.logit.2, pred.pew.age, type = 'response', se.fit = TRUE)
# �������� ������ ������Ʈȭ�Ѵ�.
pred.pew.age$fit <- logit.pew.age$fit
# �������� 95% �ŷڱ����� ���Ѱ� ������ ���� ������Ʈȭ�Ѵ�.
pred.pew.age$lwr <- logit.pew.age$fit - 1.96*logit.pew.age$se.fit
pred.pew.age$upr  <- logit.pew.age$fit + 1.96*logit.pew.age$se.fit
# �ռ��� ������� �׸��� �׷ȴ�.
plot(pred.pew.age$age.m, pred.pew.age$fit, ylim = c(0, 1),
    type = 'l', lwd = 3, col = 'blue',
    ylab = "predicted probability to use SNS", xlab = "Age, year",
    main = "The effect of age years on respondents' probability to use SNS ")
points(pred.pew.age$age.m, pred.pew.age$lwr, type = 'l', lty = 2, col = 'blue')
points(pred.pew.age$age.m, pred.pew.age$upr, type = 'l', lty = 2, col = 'blue')


#-------------------------------����
x <- -600:600/100
y <- 1/(1+exp(-x))
plot(x, y, type='l', col = 'blue', 
    xlab='x', ylab='F(x)', main = 'Logistic function')

mylogit <- log(y/(1-y))
plot(x, mylogit, type='l', col = 'blue', 
    xlab='x', ylab='Logit', main = 'Transformed by Logit function')

#-------------------------------����
# ������ƽ ȸ�ͺм��� ����R2 ��� �Լ�
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

