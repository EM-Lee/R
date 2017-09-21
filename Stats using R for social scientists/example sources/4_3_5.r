# 4��. ���м�
# 3��. �ٺ��� �߸����м�
# 5. �ٺ��� �Ϲ�ȸ�ͺм�

# �ٺ��� ȸ�ͺм� (multivariate odrindary least square regression)
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# ��������� �ְ����� ���� �� ��, �׸��� ���������δ� �������� �α�������� ������(����(����),
# ����(�Ҽ�����), ����, ��������, �ҵ����, SNS �̿뿩�θ� �̿��Ͽ���.
# �켱 ���� ���� ��� 8�� 9�� ���� Don't know�� Refusal�� ������ó���Ͽ���. ���ڰ� Ŭ����
# � ���� �����ϴ� ������ �������� ���ڵ��� �Ͽ���.
table(pew$q1, useNA = 'always')
pew$life.quality <- pew$q1
pew$life.quality[pew$q1 ==8 | pew$q1 == 9] <- NA
pew$life.quality <- 6 - pew$life.quality
# ������ ��� �̺к����� �����, �����̸� 1, �����̸� 0���� �ڵ��Ͽ���.
table(pew$sex, useNA = 'always')
pew$female <- 0
pew$female[pew$sex == 2] <- 1
# ������ ��� �����д��� �ƴ� ����, �����д��� �ƴ� ����, �����д�, �����д��� �ƴ� ��Ÿ��������
# 4�� ������ �����Ͽ���. �ذ������� '�����д��� �ƴ� ����'���� �����Ͽ���.  
# 8�� 9�� ���� Don't know�� Refusal�� ������ó���Ͽ���.
table(pew$race, pew$hisp, useNA = 'always')
pew$race4 <- 4
pew$race4[pew$race==1 & pew$hisp==2] <- 1
pew$race4[pew$race==2 & pew$hisp==2] <- 2
pew$race4[pew$hisp==1] <- 3
pew$race4[pew$hisp==8|pew$hisp==9|pew$race==8|pew$race==9] <- NA
# R�� ��� factor�� ���ǵ� ������ ȸ�ͺм��� �����ϸ�, 
# ù��° ������ �ذ��������� �ϴ� �̺к��������� ������ ���Եȴ�.  
pew$race4 <- factor(pew$race4,labels=c('white','black','hispanic','other races'))
table(pew$race4,useNA='always')
# �׷��� ���⼭�� ������ ���� black, hispanic, other.race�� �� �̺к����� �����ϱ�� �Ѵ�. 
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
# ������ ���, ������ ������ �̿�������, 98�� 99�� ��� Don't know�� Refusal�� ������ó���Ͽ���
table(pew$age, useNA = 'always')
pew$age.m <- pew$age
pew$age.m[pew$age >= 98] <- NA
# �Ʒ��� �߽�ȭ��ȯ(centering)�� �ǽ��� ���̴�.
pew$age.m.c <- pew$age.m - mean(pew$age.m,na.rm=T)
# ���������� ��� ������ ���������� ������ ������ �����Ͽ���. 8�� 9�� ���� Don't know�� Refusal��
# ������ó���Ͽ���.
table(pew$educ, useNA = 'always')
pew$educ.m <- pew$educ
pew$educ.m[pew$educ >= 8] <- NA
pew$educ.m.c <- pew$educ.m - mean(pew$educ.m,na.rm=T)
# �ҵ������ ��쵵 ������ ���������� ������ ������ �����Ͽ���. 98�� 99�� ���� Don't know�� Refusal��
# ������ó���Ͽ���.
table(pew$inc, useNA = 'always')
pew$inc.m <- pew$inc
pew$inc.m[pew$inc >= 98] <- NA
pew$inc.m.c <- pew$inc.m - mean(pew$inc.m,na.rm=T)
# SNS�̿뿩�δ� �̿����̸� 1, ���̿����̸� 0���� �ڵ��Ͽ���. 98�� 99�� ���� Don't know�� Refusal��
# ������ó���Ͽ���.
table(pew$q17m1, useNA = 'always')
pew$sns.user <- 0
pew$sns.user[pew$q17m1 >= 98] <- NA
pew$sns.user[pew$q17m1 <= 12] <- 1
# �м��� �̿�Ǵ� �������� ����Ʈ������(listwise) ������� �������� ���� ��ʵ��� �����Ѵ�.
pew.listwise <- pew[complete.cases(pew[,c('female','race4','age.m.c','educ.m.c','inc.m.c','sns.user','life.quality')]),]
# ������ ���м� ���������� �������� �̿��Ͽ� ���������� ��������� ���踦 ���캼 ���� �����Ѵ�.
# �ƽ��Ե� ������ �ڷῡ ����� ��ȸ�����ڷ��� Ư������ �������� ���� ���踦 ���캸�� ���� ���� ���� ��찡 ����.
# �׷��� jitter() �Լ��� �̿��ϸ� ������� �ð��� ������ ���ذ� �����ϴ�. ���ʹ� �����Ϳ� ������ �־�
# ���������͸� ������Ű�� ����� �Ѵ�. jitter() ��� �տ��� �Ұ��� hexbin()�� �̿��ϴ� �͵� �Ѱ��� ����̴�.
# �߽�ȭ��ȯ�� ���� ��հ����� �̵��ϴ� ���̱⿡ �ؼ��� ���̼��� ���� ���⼭�� �߽�ȭ��ȯ �������� �̿��ߴ�.
par(mfrow=c(1,3))
plot(jitter(pew.listwise$age.m,factor=2),jitter(pew.listwise$life.quality,factor=2),
    xlab = 'age, years', ylab = 'Subjectively felt life quality', main = 'Life quality & Age, years')
plot(jitter(pew.listwise$educ.m,factor=2),jitter(pew.listwise$life.quality,factor=2),
    xlab = 'Education level', ylab = 'Subjectively felt life quality', main = 'Life quality & Education level')
plot(jitter(pew.listwise$inc.m,factor=2),jitter(pew.listwise$life.quality,factor=2),
    xlab = 'Income level', ylab = 'Subjectively felt life quality', main = 'Life quality & Income level')
# ���������� ������踦 ���غ���.
round(cor(pew.listwise[,c('life.quality','female','black','hispanic','other.race','age.m.c',
  'educ.m.c','inc.m.c','sns.user')]),2)
# �������� ��������� ������ ������׷��� �̿��Ͽ� �ð������� Ȯ���� ���Ҵ�.   
hist(pew.listwise$life.quality, breaks=5, ylab='Frequency', 
  xlab='Subjectively felt life quality',main='Distribution of outcome variable')
# ����ȸ�ͺм� ������ �����Ͽ� ���� ������Ʈȭ�Ѵ�.
obj.ols.1 <- lm(life.quality ~ female + black + hispanic + other.race + age.m.c + I(age.m.c^2) 
    + educ.m.c + sns.user, data = pew.listwise)
summary(obj.ols.1)
# ��ȣ�ۿ��� �׽�Ʈ�ϴ� �͵� �����ϴ�. Ư���� ��ȣ�ۿ����� �߰��� �����ʿ䰡 ����.
# ������ SNS �̿뿩�ο� ���� �α�������� �Ӽ���  life.quality�� ���谡 �޶������� ���θ� �׽�Ʈ�� ���̴�.
obj.ols.2 <- lm(life.quality ~ (female + black + hispanic + other.race + age.m.c + I(age.m.c^2) 
    + educ.m.c + inc.m.c)*sns.user, data = pew.listwise)
summary(obj.ols.2)
# ������ ���յ��� �׽�Ʈ�� ���� �ִ�. �л�м����� ����ߴ� anova()�� �̿��ϸ� �ȴ�.
# ����Ͽ���, GLM�� �������� ANOVA�� REGRESSION�� ���������� ������ �����̴�. 
anova(obj.ols.1, obj.ols.2)

# ���������� ��������� ��ġ�� ȿ��ũ��� ǥ��ȭ ȸ�Ͱ���� �̿��Ͽ� ���� �� �ִ�.  
library('QuantPsyc')
round(lm.beta(obj.ols.1),3)
round(lm.beta(obj.ols.2),3)

# �Ʒ��� ���� ������� ȸ�Ͱ������ ������ �����Ͽ� ���캼 �� �ִ�.
ols.table <- round(cbind(summary(obj.ols.1)$coef, c(NA,lm.beta(obj.ols.1))),2)
colnames(ols.table) <- c('Unstd.Coef','S.E.','T.value','P.value','Std.Coef')
ols.table

# ��ǥ��ȭ �� ǥ��ȭ ������� ������ write.csv() �Լ��� �̿��Ͽ� ����ȭ�Ϸ� ������ �� �ִ�
write.csv(ols.table, file = "C:/data/example/OLS_R_introduction_book.csv", row.names=FALSE) 

# ������ ȿ���� ��ǥ�� �׸���
# �켱 �������ɰ� �ְ����� ǥ���� �� �ִ� �����͸� �������� �ϳ� �����Ѵ�. 
pred.pew.age <- pew.listwise[18:97,]
pred.pew.age$age.m.c <- 18:97 - mean(pew.listwise$age.m)
pred.pew.age$age.m <- 18:97
# ������ ����, ��������, �ҵ�, SNS�̿뿩�θ� ������ ���� ǥ���� ��հ����� ��ü�Ͽ� �����Ѵ�.  
# �߽�ȭ ��ȯ�� �ǽ��Ͽ��� ������ ������ 0���� ��ġ�ϸ� ������, �Ʒ��� ���� ǥ���� ��հ��� 0���� ��ġ�Ͽ���. 
pred.pew.age$female <- mean(pew.listwise$female)
pred.pew.age$black <- mean(pew.listwise$black)
pred.pew.age$hispanic <- mean(pew.listwise$hispanic)
pred.pew.age$other.race <- mean(pew.listwise$other.race)
pred.pew.age$educ.m.c <- mean(pew.listwise$educ.m.c)
pred.pew.age$inc.m.c <- mean(pew.listwise$inc.m.c)
pred.pew.age$sns.user <- mean(pew.listwise$sns.user)
# ������ 17���� 18���� �����δٰ� �� ��, �տ��� ������ ù��° OLS ȸ�͹����Ŀ� ����
# �������� �Ʒ��� ���� Ȯ���� �� �ִ�.
pred.life.quality <- predict(obj.ols.1, pred.pew.age, interval = 'confidence')
pred.life.quality
pred.pew.age <- data.frame(pred.pew.age,pred.life.quality)
# ���ɿ� ���� �ְ��� ���� �������� ��� ��ȭ�ϴ����� ������ ���� �÷��� �̿��ؼ� ���� �� �ִ�.  
plot(pred.pew.age$age.m,pred.pew.age$fit, ylim = c(3,4.5),
    type = 'l', lwd = 3, lty = 1,  col = 'blue',
    xlab = "Respondents' age, years", ylab = "Perceived life quality (OLS regression prediction)",
    main = "Quadratic relationship between perceived life quality and respondents' age")
# �������� 95% �ŷڱ����� ǥ���ϸ� ������ ����.  
points(pred.pew.age$age.m,pred.pew.age$lwr,type = 'l', lty = 2, col = 'blue')
points(pred.pew.age$age.m,pred.pew.age$upr,type = 'l', lty = 2, col = 'blue')

# ��ȣ�ۿ� ȿ�� ������ ��ǥ�� �׸���
pred.pew.educ <- pew.listwise[1:4,]
pred.pew.educ$female <- mean(pew.listwise$female)
pred.pew.educ$black <- mean(pew.listwise$black)
pred.pew.educ$hispanic <- mean(pew.listwise$hispanic)
pred.pew.educ$other.race <- mean(pew.listwise$other.race)
# ��� �߽�ȭ ��ȯ�� �ǽ��ϸ� �Ʒ� ������ ��ɹ�, ��� 0�� ������ �Ͱ� ������ ȿ���� �ִ�.  
pred.pew.educ$inc.m.c <- mean(pew.listwise$inc.m.c)
pred.pew.educ$age.m.c <- mean(pew.listwise$age.m.c)
pred.pew.educ$sns.user <- c(0,0,1,1)
pred.pew.educ$educ.m.c <- c(1,7,1,7) - mean(pew.listwise$educ.m)
# ���������� �ּ�-�ִ�� ��ȭ�� ��, �׸��� SNS �̿���¿� ���� �տ��� ������ OLS �����Ŀ� ����
# �������� �Ʒ��� ���� Ȯ���� �� �ִ�.
pred.life.quality <- predict(obj.ols.2, pred.pew.educ, interval = 'confidence')
pred.life.quality
pred.pew.educ <- data.frame(pred.pew.educ,pred.life.quality)
# �켱 SNS�� �̿����� �ʴ� ���, ��������(�ּ�-�ִ�)�� ȸ�ͺм������������� ��������
plot(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==0],
    pred.pew.educ$fit[pred.pew.educ$sns.user==0], ylim = c(2.5,3.5),
    type = 'l', lwd = 3, lty = 1,  col = 'red',
    xlab = "Education level (centered)", ylab = "Perceived life quality (OLS regression prediction)",
    main = 'Interaction effect between education and SNS use status\non perceived life quality')
# �켱 SNS�� �̿��ϴ� ���, ��������(�ּ�-�ִ�)�� ȸ�ͺм������������� ��������
points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==1],
    pred.pew.educ$fit[pred.pew.educ$sns.user==1],
    type = 'l', lwd = 3, lty = 1, col = 'blue')
legend('bottomright',legend=c('Non SNS-users','SNS users'),col=c('red','blue'),lty=1, lwd=3)
# �Ʒ��� ��ɹ��� �̿��ϸ� �������� 95% �ŷڱ����� ǥ���� �� �ִ�.
# �׷��� ������ ������ ��� �׷����� ���������� �������� �ɷ��� �������ٰ� �����ϱ⿡ �׸����� �ʾҴ�.
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==0],
#     pred.pew.educ$lwr[pred.pew.educ$sns.user==0],type = 'l', lty = 2, col = 'red')
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==0],
#     pred.pew.educ$upr[pred.pew.educ$sns.user==0],type = 'l', lty = 2, col = 'red')
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==1],
#     pred.pew.educ$lwr[pred.pew.educ$sns.user==1], type = 'l', lty = 2, col = 'blue')
# points(pred.pew.educ$educ.m.c[pred.pew.educ$sns.user==1],
#     pred.pew.educ$upr[pred.pew.educ$sns.user==1], type = 'l', lty = 2, col = 'blue')


# ��������
# �̻�ġ ���� (��ȣ�ۿ� ȿ������ ����� ��츸 ���� �Ұ��Ѵ�)
cooksd <- cooks.distance(obj.ols.2)
cook.threshold <- 4/(dim(pew.listwise)[1] - length(obj.ols.2$coef))
plot(cooksd)
abline(h = cook.threshold)
outlier <- rep(0,length(cooksd))
# �Ʒ��� ���� �̻�ġ�� �ش�Ǵ� ����� ���ڸ� ����� �� �ִ�. �� 99��(�� 6%)�� �̻�ġ�� �ش�Ǵ� ������ ���´�  
outlier[cooksd > cook.threshold] <- 1
table(outlier)

# ���Լ� ����
# 'car' ���̺귯���� ������ �ſ� �����ϴ� 
library('car')
par(mfrow=c(1,2))
qqPlot(obj.ols.1)
qqPlot(obj.ols.2)

# ���ǵ� ���� ����� �̿��� �̺л꼺 ����
library('lmtest')
bptest(obj.ols.1)
bptest(obj.ols.2)

# �̺л꼺(heteroskedascity) ����(���� 1�� �����)
par(mfrow=c(2,2))
plot(fitted.values(obj.ols.1), resid(obj.ols.1))
plot(pew.listwise$age.m, resid(obj.ols.1))
plot(pew.listwise$educ.m, resid(obj.ols.1))
plot(pew.listwise$inc.m, resid(obj.ols.1))

# ������ ����: ���߰�����(multi-collinearity)
# �л���â������ ���ϴ� ����� ������ ����.
round(vif(obj.ols.1),2)
round(vif(obj.ols.2),2)
# ���� ���ε�(tolerance)�� �˷��� ���� ���� �� �ִ�.
round(1/vif(obj.ols.1),2)
round(vif(obj.ols.2),2)
