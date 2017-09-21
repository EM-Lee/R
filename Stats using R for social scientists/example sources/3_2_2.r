# 3��. ������ ����ó��
# 2��. ���� ����
# 2. ���ڵ��ϱ�(reverse-coding)

# �ռ��� pew$q16m�� ����� �۾��� �����Ѵ�.  
# reversecoding�̶�� ������Ʈ�� ����µ�, �� ����� 4, 3, 2, 1�� �Ѵ�. 
reversecoding <- c(4,3,2,1)
# reversecoding�� ��Ͽ� �ش�Ǵ� ��� 1,2,3,4�� 4,3,2,1�� �ٲپ� q16mr�̶�� ������ ����� 
# pew�����Ϳ� �����Ѵ�. 
pew$q16mr <- reversecoding[pew$q16m]
# ���ڵ��� ����� ����Ǿ����� Ȯ���Ѵ�. 
table(pew$q16mr, pew$q16m, useNA = 'always')

# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# Q4�� Ȯ���ϸ� ���������� -1�� ������ �� �� �ִ�.  
table(tess$Q4)
# Q4���� ���ڵ��� �� Q4r�� �ϳ� �� �����Ѵ�. 
tess$Q4r <- tess$Q4
# �������� ó���Ͽ���.  
tess$Q4r[tess$Q4 == -1] <- NA 
# ���ڵ��� �ǽ��Ͽ���.  
tess$Q4r <- 10 - tess$Q4r
# ���ڵ��� ����� �ǽõǾ����� üũ�Ͽ���.  
table(tess$Q4, tess$Q4r, useNA = 'always')



