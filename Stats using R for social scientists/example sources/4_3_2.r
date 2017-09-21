# 4��. ���м�
# 3��. �ٺ��� �߸����м�
# 2. ī������ ����

# �ݺ��Ǵ� �κп� ���ؼ��� ������ �����Ѵ�.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# �켱 ������ �������� üũ�Ѵ�. 8�� 9�� ������ ���� '���� ����', '����ź�'�̱⿡ ���������� ó���Ͽ���.  
table(pew$intuse, useNA = 'always')
table(pew$q1, useNA = 'always')
pew$intuse.r <- pew$intuse
pew$life.quality <- pew$q1
pew$intuse.r[pew$intuse ==8 | pew$intuse == 9] <- NA
pew$life.quality[pew$q1 ==8 | pew$q1 == 9] <- NA
table(pew$intuse.r, pew$life.quality, useNA = 'always')
# ���ͳ� �̿뿩�ο� ���� ���� ���� ��������� �� ���ܺ��� ���캸�Ҵ� (�̿��ڿ� ���̿��ڰ� �ٸ��� ����) 
round(table(pew$intuse.r, pew$life.quality)[1,]/sum(table(pew$intuse.r, pew$life.quality)[1,]),2) 
round(table(pew$intuse.r, pew$life.quality)[2,]/sum(table(pew$intuse.r, pew$life.quality)[2,]),2) 
# ī������ ������ table()�� ���� ������Ʈ�� ���� ��갡���ϴ�. 
chisq.test(table(pew$intuse.r, pew$life.quality)) 

# ���� ���� �����ϰ� ���α׷����� �ϴ� �ͺ��� �ܺ� ���̺귯���� ����ϸ� ���� ����� ����� ���� ���� �� �ִ�. 
# descr ���̺귯���� ��ġ�Ǿ� ���� ������ �켱 install.packages('descr')�� �̿��� ��ġ�Ѵ�. 
library('descr') 
# CrossTable()�� SPSS�� crosstabs�� SAS�� proc freq �� �����ϴ�. prop.r�� �����ٿ� ���� ����, 
# prop.c�� �����ٿ� ���� ����, prop.t�� ��üĭ���� �������� �� ������ �ǹ��Ѵ�. expected�� ��� 
# ���󵵸� ǥ�������� �����ϴ� �ɼ��̸�, chisq�� ��� ī������ �������� �������� �����ϴ� �ɼ��̴�. 
CrossTable(pew$intuse.r, pew$life.quality, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, expected = TRUE, chisq = TRUE)

################################---- ���� 
CrossTable(pew$intuse.r, pew$sex, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, 
expected = TRUE, chisq = TRUE)

