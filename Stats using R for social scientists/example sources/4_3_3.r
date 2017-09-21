# 4��. ���м�
# 3��. �ٺ��� �߸����м�
# 3. ������� �м�(correlation analysis)

# �ݺ��Ǵ� �κп� ���ؼ��� ������ �����Ѵ�.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# �� ������ üũ�ؼ� ���������� ó���� �κ��� �ִ����� ���캻��. 
# ������(q2)�� ���� 98(�� ��), 99(����ź�)�� ���������� ó���Ͽ���. 
# ���������� ��� 8(�� ��), 9(����ź�)�� ���������� ó���Ͽ���.  
table(pew$q2, useNA = 'always')
table(pew$educ, useNA = 'always')
pew$q2r <- pew$q2
pew$educr <- pew$educ
pew$q2r[pew$q2 == 98 | pew$q2 == 99] <- NA
pew$educr[pew$educ == 8 | pew$educ == 9] <- NA
# �켱 �������� ������׷��� ���� ������ Ȯ���� ����. 
hist(pew$q2r,breaks = 15) 
hist(pew$educr,breaks = 7)

# q2r�� ��� ��� ������� �������� �ſ� ������, ��κ��� ������� ���� å�� ���� �ʴ� ���� ���δ�. 
# �̷��� ġ��ģ ����(skewed distribution)�� ��� �α���ȯ�� �ϸ� ���Ժ����� ����� ������ ���� �� �ִ�.  
# ���⼭�� ��(base)�� 10�� ���α׸� ���Ͽ���. 0���� ���� ����� ó���ϱ� ���� ��� 1�� ���Ͽ���. 
pew$log.q2r <- log((pew$q2r+1), 10)
# plot()�� �̿�, �� ���� ������ �������� �׷�����.  type = 'p'�� ���� ��´ٴ� �ǹ̴�. 
plot(pew$educr, pew$log.q2r, type = 'p')

# ���� �÷��� �������ذ� �������� ���踦 ȿ�������� �������� ���ϴ� ������ �ִ�. ǥ���� Ŭ ��� 
# �������� ȿ�������� ���� ��찡 ������, �̷� ��� ������ ������ ������ ������� ó���� 
# �е�������(density scatterplot)�� ȿ������ �� �ִ�. �е��������� ��� hexbin�̶�� ���̺귯��
# ���� ���� �� �ִ�. ��ġ�� �Ǿ� ���� ������, install.packages('hexbin')�� ���� ��ġ�� ��Ű��.  
library('hexbin')
# ���������� X��, �α�ġȯ�� �������� Y�࿡ ��ġ�Ѵ�. xbins�� ��� X�࿡ ��ġ�� ��������� ������ 
# ������ �ǹ��Ѵ� (6�� �����ϸ� 7���� ������ ���´�). �̷��� ��ȯ�� �� hex.plot�̶�� ������Ʈ�� �����Ѵ� 
hex.plot <- hexbin(pew$educr, pew$log.q2r, xbins = 6) 
# hex.plot ������Ʈ�� �׸��� �׸���. xlab�� ylab�� ���� X��� Y���� �̸���, �׸��� main�� �׷����� �̸���
# �����ϴ� �ɼ��̴�.  
plot(hex.plot, xlab = 'educational achievement', ylab = 'book-reading (log-transformed)',
  main = "Relationship between educational acheivement and book reading")
  
# �е����������� �� �� �ֵ��� �� ������ ����� �ܼ����� ���迡 ����� �� �ϴ�. 
# cor.test()�� �� ������ ��������� ����� ���ǵ��� �׽�Ʈ�Ѵ�. type�� ���� �Ǿ r���� ��Ȯ�ϰ� �Ͽ���.  
cor.test(pew$educr, pew$log.q2r, method = 'pearson') 
# �Ǿ�������� ������ ���� ����� ���� �ִ�. �켱 �� �������� �������� ���� ��ʵ��� ��󳽴�. 
# �Ǿ r�� ������ ���α׷����ϸ� cor.test()������ �������� ������ ���� ���� �� �ִ�.  
pew.subset <- pew[complete.cases(pew$educr,pew$log.q2r),] 
cov(pew.subset$educr,pew.subset$log.q2r)/(sd(pew.subset$educr)*sd(pew.subset$log.q2r)) 

# cor.test()���� type�� ���� spearman���� �ٲٸ� rho�� ���� �� �ִ�.  
cor.test(pew$educr, pew$log.q2r, method = 'spearman') 
# cor.test()���� type�� ���� kendall���� �ٲٸ� tau�� ���� �� �ִ�.  
cor.test(pew$educr, pew$log.q2r, method = 'spearman') 

# r�� ��� .358, rho�� ��� .363���� ����� ���� ���� �ٸ��ٴ� ���� �� �� �ִ�. 
# �̿� ���� tau�� .297�� ��������� ����. 
# ����  �������� �α���ȯ��Ű�� ������ r�� ������� ��ȭ�� �ִ� �ݸ�, rho�� tau�� ��ȭ����. 
cor.test(pew$educr, pew$q2r, method = 'pearson') 
cor.test(pew$educr, pew$q2r, method = 'spearman') 
cor.test(pew$educr, pew$q2r, method = 'kendall') 

# �������� ������ȯ(linear transformation)��Ű�� ��, rho�� tau�� �޶����� �ʴ´�.  
cor.test(pew$educr, pew$q2r^2, method = 'pearson') 
cor.test(pew$educr, pew$q2r^2, method = 'spearman') 
cor.test(pew$educr, pew$q2r^2, method = 'kendall') 

cor.test(pew$educr, sqrt(pew$q2r), method = 'pearson') 
cor.test(pew$educr, sqrt(pew$q2r), method = 'spearman') 
cor.test(pew$educr, sqrt(pew$q2r), method = 'kendall') 

# �ݺ��Ǵ� �κп� ���ؼ��� ������ �����Ѵ�.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# �����д��� �ƴ� ����(1)�� ������ �ƴ� ������ ����(�Ҽ�����, 0)���� �����Ѵ�.
# �߸�(8)�� ����ź�(9)�� ���������� ó���ߴ�. 
table(pew$race, pew$hisp, useNA = 'always')
pew$white <- 0
pew$white[pew$race == 1] <- 1
pew$white[pew$race == 8 | pew$race == 9] <- NA
# �����̸鼭 �����д��̶�� ������ ����� �Ҽ��������� �ڵ��Ͽ���.  
pew$white[pew$hisp == 1] <- 0
# �����дп��ε� �߸�(8)�� ����ź�(9)�� ���������� ó���ߴ�
pew$white[pew$hisp == 8 | pew$hisp == 9] <- NA
table(pew$white, useNA = 'always') 
# ���ͳ� �̿뿩�ο��� �߸�(8)�� ����ź�(9)�� ���������� ó���ߴ�. 
# ���̿��ڴ� 0, �̿��ڴ� 1�� �ڵ��Ͽ���.   
table(pew$intuse, useNA = 'always') 
pew$intaccess <- pew$intuse
pew$intaccess[pew$intuse == 8 | pew$intuse == 9] <- NA
pew$intaccess[pew$intuse == 2] <- 0
# ������ ���ͳ� �̿뿩���� ����ǥ�� ���캻 �� white.intaccess ������Ʈ�� �����Ѵ�.  
white.intaccess <- table(pew$white, pew$intaccess)
# phi�� ���ϱ� ���ؼ��� �ܺ� ���̺귯���� psych�� �ʿ��ϴ�. ��ġ�� �Ǿ� ���� ���� ��� ���̺귯�� 
# ���������� install.packages('psych')�� ���̺귯���� ��ġ�Ͽ��� �Ѵ�.  
library('psych') 
# phi()�� table()�� ���� 2X2 ����ǥ�� ũ������ ���̰��� �����ش�. 
phi(white.intaccess)

# ������ ���� Ȥ�� ������ ���� ��� �̿��ϴ� �������� ���ϸ� ������ ����. 
cor.test(pew$white, pew$intaccess, method = 'pearson')
cor.test(pew$white, pew$intaccess, method = 'spearman')
cor.test(pew$white, pew$intaccess, method = 'kendall')

