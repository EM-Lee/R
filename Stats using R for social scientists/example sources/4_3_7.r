

# ------------------ 3.3.3.7.1. ũ�й����� ����(Cronbach��s )
# �������װ��� ������ġ���� ����ϴ� ũ�й����� ����(Cronbach's alpha)�� ���ϰ� ����� ��
# �ִ� ���̺귯���� psych�� �ִ�. �ش� ���̺귯���� ��ġ�Ǿ� ���� ������
# install.packages()�� ���� psych ��ġ����.
library('psych')
# tess �����͸� �ҷ����� �� ������ ������ ��Ȳ �װ������� �ڽ��� ������ ����� Ÿ���� ������
# ������ ��츦 ���� �ڷй��� ���ĸ� �̿��ؼ� ����غ���.
# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���.
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav",
use.value.labels = FALSE, to.data.frame = TRUE)
# �켱 �ڽ��� ������ �� ������ Q1~Q4�����̴�. Ÿ���� ������ �� ������ ��� Q5~Q8�̴�.
# �ش� ��Ȳ�� ���ؼ��� �����Ϳ� �Բ� ÷�ε� �������� ��������. -1�� ��� ������ó���Ͽ���.
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
# ũ�й��� ���İ��� ���ϱ� ���ؼ��� ������(R)X�׸�(C)�� �� �����ͼ��� ������ �ʿ䰡 �ִ�.
# �ε����� �̿��ϸ� ����� ���ϴ�.
# �켱 �ڽ��� �����Ե� ������ ���� �Ʒ��� ����. �켱 �������� ���캸�� ũ�й��� ���ĸ� ���ߴ�
alpha(tess[,c('q1r','q2r','q3r','q4r')], na.rm = TRUE)
# Ÿ���� ������ �� ������ ���� ������ ����.
alpha(tess[,c('q5r','q6r','q7r','q8r')], na.rm = TRUE)
# �׷��ٸ� �ڽŰ� Ÿ���� ������ �� ����� ���õ� 8�� �׸� ��ü�� �غ��� ���?
alpha(tess[,c('q1r','q2r','q3r','q4r','q5r','q6r','q7r','q8r')], na.rm = TRUE)


#------ 3.3.3.7.2. �ڴ��� �ŷڵ�: ������ ī�Ŀ� ũ���浵������ ����
# �ڴ��� �ŷڵ� ����� ���ؼ��� irr ���̺귯���� �����ϴ�.
# irr ���̺귯���� ��ġ�Ǿ� ���� ������ install.packages('irr')�� �����ϸ� �ȴ�.
library('irr')
# �Ʒ��� Krippendorff (1980)�� �Ƿ��ִ� ����̴�. �� 12���� ������� 4���� �ڴ���
# ����м��� �ǽ��ߴ�. NA�� �ش��ڴ��� ����м������ ���������� �� ����̴�.
# ũ���浵���� ������ ��� �ڴ��� �����ٿ�, ��ʰ� �����ٿ� ��ġ�Ǿ�� �Ѵ�. 
content <- matrix(c(1,1,NA,1,2,2,3,2,3,3,3,3,3,3,3,3,2,2,2,2,1,2,3,4,4,4,4,4,
1,1,2,1,2,2,2,2,NA,5,5,5,NA,NA,1,1,NA,NA,3,NA), nrow=4)
colnames(content) <- paste('obs',1:12,sep='')
rownames(content) <- paste('coder',1:4,sep='')
content
# �� �ڴ����� ��� ����м��� �ߴ����� ���캸�� �Ʒ��� ����.
table(content[1,], useNA = 'always')
table(content[2,], useNA = 'always')
table(content[3,], useNA = 'always')
table(content[4,], useNA = 'always')
# ���� ����м��� ����� ����� ������� �����ϸ� ������ ����.
kripp.alpha(content, method = 'nominal')
# ������ ������� �����ϸ� �Ʒ��� ����.
kripp.alpha(content, method = "ordinal")
# ��� ������ �����ϸ� �Ʒ��� ����.
kripp.alpha(content, method = "interval")
# ������ ������� �����ϸ� �Ʒ��� ����.
kripp.alpha(content, method = "ratio")
# 4���� 3���� �Ǵ��� ������ ����(�� ��������) 12��°�� ������ ��꿡 ���Ե��� ���Ѵ�.
# �Ʒ��� ��� 12��° ������ �����ߴ�. ����� ���� �����ϴ�.
kripp.alpha(content[,1:11], method = 'nominal')
kripp.alpha(content[,1:11], method = "ordinal")
kripp.alpha(content[,1:11], method = "interval")
kripp.alpha(content[,1:11], method = "ratio")
# �׷��� 4���� 2���� �Ǵ��� ������ ���ϴ��� �ٸ� 2���� �Ǵ��� ���� 11��°�� ������ ����� ���Եȴ�.
# �Ʒ��� ��� 12��°, �׸��� 11��° ������ �����ߴ�. ����� �޶����� ���� Ȯ���϶�.
kripp.alpha(content[,1:10], method = 'nominal')
kripp.alpha(content[,1:10], method = "ordinal")
kripp.alpha(content[,1:10], method = "interval")
kripp.alpha(content[,1:10], method = "ratio")  
# ���������� �������� ������ ������ ī��, ����Ʈ�� ī��, �ö��̽��� ī�Ĵ� ������ ���� ���� �� �ִ�. 
# �켱 ����Ͽ��� ������ ī�Ĵ� ���� 2���� �ڴ��� �ŷڵ��� ���� �� �ִ�. 
# ũ���浵���� ���Ŀ� ������ ������ �ٸ���. ī���� ��� ��ʰ� �����ٿ�, �ڴ��� �����ٿ� ��ġ�ȴ�. 
t.content <- t(content)
t.content
kappa2(t.content[,c(1,2)])  
kappa2(t.content[,c(1,3)])  
kappa2(t.content[,c(1,4)])  
kappa2(t.content[,c(2,3)])  
kappa2(t.content[,c(2,4)])  
kappa2(t.content[,c(3,4)])  
# �Ʊ� ���������, ����Ʈ�� ī�Ŀ� �ö��̽��� ī���� ��� �������� ����Ʈ ���� ������� �����Ѵ�. 
# ����Ʈ�� �ö��̽��� ī�ĸ� �̿��Ͽ� �ڴ��� �ŷڵ��� ����� �� ���ԵǴ� ��ʼ��� 8�̶�� ���� �ָ��϶�. 
kappam.light(t.content)
kappam.fleiss(t.content)
# �������� ������ �� �ڵ������ ����� ������� ������ �� ũ���浵���� ���ĸ� ���ϸ� 
# �� ���� �ſ� �����ϰ�  ������ ���� Ȯ���� �� �ִ�.  
kripp.alpha(content[,2:9],method='nominal')
