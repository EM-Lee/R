# 4��. ���м�
# 3��. �ٺ��� �߸����м�
# 1. t-���� 

# TESS �����͸� �ҷ����̴� ���� �տ��� �̹� �����Ͽ���. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# �м��� ����� �Ǵ� XIDEO ������ ��ǥ�� ���캸�Ҵ�. 13���� ��ʴ� ���������� ������ ó���Ǿ� �ִ�. 
table(tess$XIDEO, useNA = 'always')
# t.test() �� ������Ʈ�� t-�����ϴ� ��ɹ��̴�. mu �� ����ǥ�� t-������ �������� �ǹ��Ѵ�. 
t.test(tess$XIDEO, mu = 4)

# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# ����� �� ������ ��ǥ�� ���캸�� ������, �� -1�� �ش�Ǵ� ��ʵ��� ���� 26, 32��ŭ �����Ѵ�. 
# �̵��� ���������� ó���Ͽ���.  
table(tess$Q4, useNA = 'always')
table(tess$Q8, useNA = 'always')
tess$Q4r <- tess$Q4 
tess$Q8r <- tess$Q8
tess$Q4r[tess$Q4 == -1 ] <- NA
tess$Q8r[tess$Q8 == -1 ] <- NA
# �� �������� ��հ� ǥ�������� ��ü������ ���캸�� �Ʒ��� ����. 
mean(tess$Q4r, na.rm = TRUE); sd(tess$Q4r, na.rm = TRUE)
mean(tess$Q8r, na.rm = TRUE); sd(tess$Q8r, na.rm = TRUE)
# ���������� ó���� Q4r�� Q8r �� ����ǥ�� t-�������� ��հ��� ���̸� �׽�Ʈ�Ͽ���.  
# paired �ɼ��� ��� �ش� t-������ ����ǥ�� t-�������� �����ִ� ������ �Ѵ�. 
# ���� paired = FALSE�� ��� �Ʒ��� ����� ����ǥ�� t-������ ����ȴ�. 
t.test(tess$Q4r, tess$Q8r, paired = TRUE)

# ��������(difference score)�� �̿��ؼ� ����ǥ�� t-�������� �׽�Ʈ�� ���� �ִ�. 
# ���⼭ �������� 0���� �����Ǿ�� �Ѵ�. �� ������ �͹������� ������ ���������� ����� �ϱ� �����̴�. 
tess$diff.Q4r.Q8r <- tess$Q4r - tess$Q8r
t.test(tess$diff.Q4r.Q8r, mu = 0)

# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# ����Ͽ��� Q8���� �������� ������, �̸� ���������� ó���ؾ� �Ѵ�.  
table(tess$Q8, useNA = 'always')
tess$Q8r <- tess$Q8
tess$Q8r[tess$Q8 == -1 ] <- NA
# ���������� �����̹����� ���Ե� ���(13��24)�� �����̹����� ���õ�  ���(1��12)�� ������ 
# �����̹����� ���� 0, �����̹����� ���� 1�� �ڵ��Ͽ���.  
table(tess$XTESS089, useNA = 'always')
tess$other.female <- NA
tess$other.female[tess$XTESS089 >= 13] <- 1 
tess$other.female[tess$XTESS089 <= 12] <- 0
# �켱 �� ���ܰ� ���(ǥ������)�� ������ġ�� ���캸�Ҵ�. aggregate()�� �տ��� ������ �� �ִ�. 
aggregate(Q8r �� other.female, data=tess, FUN = mean)
aggregate(Q8r �� other.female, data=tess, FUN = var)
# bartlett.test()�� �� ���ܺ��� ��������� Q8r�� �л굿������ �����Ѵ�. '������� �� ��������'�� �����̸�, data���� ������ ���� �����ͼ��� �̸��� �־��ָ� �ȴ�.  
bartlett.test(Q8r �� other.female, data=tess)
# ���������(nonparametric technique)���ν� fligner.test(Q8r �� other.female, data=tess)�� 
# �̿��� ���� �ִ�. ���������� ����������� ���̴� �ش� ���������� �����϶�.  
# t.test()�� �̿��ؼ� ����ǥ�� t-������ �ǽ��Ͽ���.  �л굿���� ���� ��� �� ������ �л��� ��������� 
# ���ǹ��ϰ� �ٸ��� ������, var.equal�� FALSE�� �����ؾ� �Ѵ�.  
t.test(Q8r �� other.female, var.equal = FALSE, data=tess)
# ���� �л굿������ �̷������� ������ ��� var.equal�� TRUE�� �����ϸ� �ȴ�. ���� ����� ���ϸ� 
# �л굿������ ������ �� �������� �� ũ�� ��Ÿ���� ���� Ȯ���� �� �ִ�.  
t.test(Q8r �� other.female, var.equal = TRUE, data=tess)

################################---- ���� 
# ���Ƿ�-��ũ�� ���Լ� ���� 
shapiro.test(tess$XIDEO)

# car ���̺귯���� �����Ѵ�. ���� 'car'�� ��ġ�Ǿ� ���� ������ 
# install.packages('car')�� ���� ��ġ�� �� ���̺귯���� ������Ų��.  
library('car')
# ������ �л굿���� ������ leveneTest()�� �����ϴ�. 
# ��ɹ��� ������ bartlett.test()�� �����ϴ�. ������ ���� 
# ������ ������ ���� �Ŀ� �Ұ��� �л�м��� ����ϱ� ������ ���������� �������� 
# �����Ͽ��� �Ѵٴ� ���̴�. as.factor() �� ���ؼ��� �տ��� ������ �� �ִ�.  
leveneTest(Q8r �� as.factor(other.female), data=tess)

# set.seed() ��ɹ��� �������� ����Ǵ� ����� �������� ������Ű�� ������ �Ѵ�. 
# ���� �ش��ɹ��� ������ ���⼭ ���� ����� �ٸ� ����� ���� ���� �ִ�.  
set.seed(2013)
# ����� 4�̸� ǥ�������� 1�� ����0�� ����� 5�̸� ǥ�������� 4�� ����1�� 
# ���� 10���� �����ϵ��� �ùķ��̼��Ͽ���.  
sim.group0 <- rnorm(10, mean = 4, sd = 1)
sim.group1 <- rnorm(10, mean = 5, sd = 4)
# �л굿������ ������ ���� �������� ���� ��� ����ǥ�� t-������ �ǽ��Ͽ���.  
t.test(sim.group0, sim.group1, paired = FALSE, var.equal = TRUE)
t.test(sim.group0, sim.group1, paired = FALSE, var.equal = FALSE)
 