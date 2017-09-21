# 4��. ���м�
# 2��. ���Ϻ��� ������м�
# 2. ������ �߽�ġ(central tendency) ���� ��谪

# ������ ��� 
# �ݺ��Ǵ� �κп� ���ؼ��� ������ �����Ѵ�.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
table(pew$q16)
pew$q16m <- pew$q16 
pew$q16m[pew$q16 == 8 | pew$q16 == 9] <- NA 
# ���� ���Դ� mean()�� �̿��ϴ� ���̴�. sum()�� ���������� �������� ó���ϴ� �ɼ��� �ٷ� na.rm�̴�.  
mean(pew$q16m, na.rm = TRUE)
# Ȥ�� ���հ� ��ʼ� ������ �̿��Ͽ� �������� ���� �� �ִ�.  
sum.q16m <- sum(pew$q16m, na.rm = TRUE)
length.q16m <- length(pew$q16m[!is.na(pew$q16m)]) 
sum.q16m/length.q16m 

# ������ �߽ɰ� 
# ���� ���Դ� mean()�� �̿��ϴ� ���̴�. sum()�� ���������� �������� ó���ϴ� �ɼ��� �ٷ� na.rm�̴�.  
median(pew$q16m, na.rm = TRUE)
# Ȥ�� ������ ������ �Ŀ� ��ʼ� ������ �̿��Ͽ� �߽ɰ��� ���α׷��ϴ� �͵� �����ϴ�. 
# mid.q16m�� q16m�� �߰����� ��ġ������ ��Ÿ����. round()�� �ݿø��� �����Ѵ�. 
# ������ ���� Ȧ���� ����� �߽ɼ����� ����ϱ� �����̴�.  
mid.q16m <- round(length(pew$q16m[!is.na(pew$q16m)])/2, digit=0)
# order()�� �ش� ������Ʈ�� �����Ѵ�. na.last = TRUE�� ��� �������� �������� ��ġ�Ѵ�. 
# length()���� �������� ���ܵǾ� �ֱ� ������ na.last = TRUE�� ���̴� ����� �߸��� �� �ִ�. 
# [mid.q16m]�� ���ĵ� q16m�� �߰����� �ε������ش�.  
pew$q16m[order(pew$q16m, na.last = TRUE)][mid.q16m]

# ������ �ֺ� 
# ���� �����ϰԴ� table()�� �̿��ؼ� � ���� ���� ���� ��ʼ��� �ִ����� ������ Ȯ���ϴ� ���̴�. 
table(pew$q16m, useNA = 'always')
# ���� �� �����ϰ� R���α׷����� �ϴ� �͵� �����ϴ�.  
# 1. which.max()�� ������Ʈ���� ���� ū ��ʼ��� ��ġ�� ��Ÿ����. 
# 2. [which.max()] �� ���ؼ� ������Ʈ���� ���� ū ��ʼ��� �ִ� ��ġ�� �ش�Ǵ� ���� �ε������ش�.  
table.q16m <- table(pew$q16m, useNA = 'always')
names(table.q16m)[which.max(table.q16m)]
# which.min()�� ���� ���� ���� �������� ��Ÿ����. 

# ������ ����(������, ���� �幮 ��)
# ���� ���Ǵ� ���ġ�� �ƴ�����, ���� ���� ����ϰ� ���� �� �ִ�.  
table.q16m <- table(pew$q16m, useNA = 'always')
names(table.q16m)[which.min(table.q16m)]
# useNA�ɼ��� ������ ������ �ٸ��� ��Ÿ����. 
table.q16m <- table(pew$q16m)
names(table.q16m)[which.min(table.q16m)]

