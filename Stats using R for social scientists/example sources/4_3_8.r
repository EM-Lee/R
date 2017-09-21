
# �ּ��� �м�
mydata <- read.csv(file="C:/data/example/example_EFA.csv",header=TRUE)
# �Ʒ��� �ܺ� ���̺귯���� �ʿ��ϴ�. 
library('psych')
library('GPArotation')
# ����������� ���� ������ ���踦 Ȯ���غ���
cor(mydata)

# ������ �ּ����� ������ ��ũ����ǥ�� �̿��ؼ� ���غ���.  
mypc <- princomp(mydata[,-16], cor=TRUE)
summary(mypc)
# �ּ����� �л��� �ּ����� ǥ�������� �����ϸ� �ȴ�. 
summary(mypc)$sdev
# �ּ����� �л��� ���� �ּ��� �м��� ���Ե� ������ ���� ǥ��ȭ�� �л� 1�� ���� ���� ����. 
sum(summary(mypc)$sdev^2)

plot(mypc,type='lines',ylim=c(0,5),main='Screeplot for PCA (Principal Component Analysis)')
abline(h=1)

mypc$loading[,1:3]


# ����� �ּ����� ȸ������� ���� �ּ��км��� ���� ��
mypc.no <- principal(mydata[,-16], nfactors = 3, rotate = 'norotate')
mypc.vm <- principal(mydata[,-16], nfactors = 3, rotate = 'varimax')
mypc.pm <- principal(mydata[,-16], nfactors = 3, rotate = 'promax')
mypc.om <- principal(mydata[,-16], nfactors = 3, rotate = 'oblimin')
# ȸ������ 
mypc.no

# ����ȸ��(Varimax rotation)�� ���
mypc.vm

# �簢ȸ��(oblimin rotation)
mypc.om

# ���θƽ�(Promax) ȸ��
mypc.pm

# ȸ������� ���� �ּ��� �м� ��� ������ ���� 
mypcs <- cbind(mypc.vm$scores,mypc.om$scores,mypc.pm$scores)
colnames(mypcs) <- c('CompB.V','CompC.V','CompA.V',
      'CompB.O','CompC.O','CompA.O','CompB.P','CompC.P','CompA.P')
round(cor(mypcs),2)

# ȸ������� �ּ��км����� ������ �ּ��а� Ž���� ���ںм����� ������ ���ڸ� �̿��� ȸ�͸��� �񱳴� 
# Ž���� ���ںм� �ʿ��� �ٷ��� ��. 

# Ž���� ���ںм�(������ ������ �������� �̷��� ������ �ٰ��Ͽ� ������) 
# �ּ��� �м��� �޸� Ž���� ���ںм��� ������ ���� �����ڰ� �����ؾ� �Ѵ�. 
# scores = ��regression���� ȸ�ͺм� ������ �̿��ؼ� ���ڰ��� �����ߴٴ� �ǹ��̴�. 
mymlefa.vm <- factanal(mydata[,-16], factors = 3, rotation = 'varimax', scores = 'regression')
mymlefa.om <- factanal(mydata[,-16], factors = 3, rotation = 'oblimin', scores = 'regression')
mymlefa.pm <- factanal(mydata[,-16], factors = 3, rotation = 'promax', scores = 'regression')
# ����ȸ��(Varimax rotation)
print(mymlefa.vm,cutoff=.00)

# �簢ȸ��(Oblimin rotation)
print(mymlefa.om,cutoff=.00)

# ���θƽ� ȸ��(Promax rotation)
print(mymlefa.pm,cutoff=.00)

# ȸ������� ���� Ž���� ���ںм� ��� ������ ���� 
myefas <- cbind(mymlefa.vm$scores,mymlefa.pm$scores,mymlefa.om$scores)
colnames(myefas) <- c('FB.V','FA.V','FC.V',
        'FB.O','FC.O','FA.O','FB.P','FA.P','FC.P')
round(cor(myefas[,c('FB.V','FA.V','FC.V',
         'FB.O','FC.O','FA.O','FB.P','FA.P','FC.P')]),2)

# ������ Ž���� ���ںм� ����� �տ��� ����� �ּ��км��� mydata ���Ͽ� �߰��Ͽ���. 
mydata <- data.frame(mydata,mypcs,myefas)
# �Ʒ��� ��ɹ��� ����� �ּ����̳� ���ڸ� ������� �ʰ�, �׸��� ��հ��� �̿��ϴ� ���� �����ش�. 
mydata$meanA <- apply(mydata[,1:5],1,mean) 
mydata$meanB <- apply(mydata[,6:10],1,mean)
mydata$meanC <- apply(mydata[,11:15],1,mean)

# ����� �ּ���, ����, �׸��� ��հ��� �̿��Ͽ� OLS ȸ�ͺм��� �ǽ��� ��츦 ���Ͽ���. 
# �ش� �ּ���/������ ȿ���� ���� ��V�ٸ� ���� Ȯ���� �� �ִ�. 
model.mean.value <- lm(outcome ~ meanA + meanB + meanC, mydata)
model.PC.varimax <- lm(outcome ~ CompA.V + CompB.V + CompC.V, mydata)
model.PC.oblimin <- lm(outcome ~ CompA.O + CompB.O + CompC.O, mydata)
model.PC.promax <- lm(outcome ~ CompA.P + CompB.P + CompC.P, mydata)
model.EFA.varimax <- lm(outcome ~ FA.V + FB.V + FC.V, mydata)
model.EFA.oblimin <- lm(outcome ~ FA.O + FB.O + FC.O, mydata)
model.EFA.promax <- lm(outcome ~ FA.P + FB.P + FC.P, mydata)
# �׸��� ��հ��� �̿��Ͽ� outcome�� �����ϴ� ���
summary(model.mean.value)

# ����ȸ���� �ּ����� ���������� �̿��Ͽ� outcome�� �����ϴ� ���
summary(model.PC.varimax)

# �簢ȸ���� �ּ����� ���������� �̿��Ͽ� outcome�� �����ϴ� ���
summary(model.PC.oblimin)

# ���θƽ� ������� ȸ���� �ּ����� ���������� �̿��Ͽ� outcome�� �����ϴ� ���
summary(model.PC.promax)

# ����ȸ���� ���ڸ� ���������� �̿��Ͽ� outcome�� �����ϴ� ���
summary(model.EFA.varimax)

# �簢ȸ���� ���ڸ� ���������� �̿��Ͽ� outcome�� �����ϴ� ���
summary(model.EFA.oblimin)

# ���θƽ� ������� ȸ���� ���ڸ� ���������� �̿��Ͽ� outcome�� �����ϴ� ���
summary(model.EFA.promax)

