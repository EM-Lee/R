# 4��. ���м�
# 2��. ���Ϻ��� ������м�
# 3. ������ ����(dispersion) ���� ��谪

# ������ ��հ�  ǥ������ 
# �ݺ��Ǵ� �κп� ���ؼ��� ������ �����Ѵ�.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
table(pew$q16)
pew$q16m <- pew$q16 
pew$q16m[pew$q16 == 8 | pew$q16 == 9] <- NA 
# �л����� ���� ���� ���� �Լ��� var()�̴�. 
var(pew$q16m, na.rm = TRUE)
# ǥ����������� ���� ���� ���� �Լ��� sd()�̴�. 
sd(pew$q16m, na.rm = TRUE)
# ��հ� ��ʼ� ������ �̿��Ͽ� �л��� ���� �� �ִ�. 
mean.q16m <- mean(pew$q16m, na.rm = TRUE)
length.q16m <- length(pew$q16m[!is.na(pew$q16m)]) 
# ǥ���� ǥ�������� ��� ��ʼ����� 1�� ���ش�. �̿� ���� �ڼ��� ������ ����� �̷��� �����϶�.
# ^2�� ������ �Ѵٴ� ���̴�. ����ϰ� ^3�� ��������, ^4�� �������� ���Ѵ�.  
var.q16m <- sum((pew$q16m[!is.na(pew$q16m)] - mean.q16m)^2)/(length.q16m-1)
var.q16m
# �л꿡 �������� ���ϸ� ǥ�������� �ȴ�. sqrt()�� ������Ʈ�� ǥ�������� ���ϴ� �Լ��̴�.  
sqrt(var.q16m)

# ������ ������ ���ϴ� ���� ���� ����� range()�� �̿��ϴ� ���̴�. 
range(pew$q16m, na.rm = TRUE)
# 25%°, 75%°�� ��� quantile()�� �̿��ϸ� �����ϴ�.  
quantile(pew$q16m, na.rm = TRUE)
# ���� 10%�� 90%�� ���� ���  prob �ɼ����� ���ϴ�  ������ ������ �� �ִ�. 
quantile(pew$q16m, prob = c(0.1, 0.9), na.rm = TRUE)
# ������ ���İ� �ε����� �̿��Ͽ� ���� �����ϰ� ���α׷����ϴ� �͵� �����ϴ�.  
length.q16m <- length(pew$q16m[!is.na(pew$q16m)]) 
# 1��°�� ���� �������� �������� �����Ѵ�. �� ������ �ּҰ��� �ִ밪�� �����ϴ� range()�� �����ϴ�. 
range.q16m <- pew$q16m[order(pew$q16m, na.last = TRUE)][c(1,length.q16m)]
range.q16m
# 25%°�� 75%°�� �������� �����Ѵ�. �� prob�ɼ��� ������ �������� �ʴ� quantile()�� �����ϴ�. 
interquartile.q16m <- pew$q16m[order(pew$q16m, na.last = TRUE)][c(round(0.25*length.q16m,digit=0),round(0.75*length.q16m,digit=0))]
interquartile.q16m

# ������ġ�� ����ؼ� �����ִ� �Լ����� 
descriptive.statistics <- function(myvariable) {
  # �������� �����ϴ� ��ü��ʼ� 
  myvar.length.missing.include <- length(myvariable)
  # �������� �����ϴ� ��ü��ʼ�
  myvar.length.missing.exclude <- length(myvariable[!is.na(myvariable)])
  # ������������ ������ �� ���� 
  myvar.sum <- sum(myvariable, na.rm = TRUE)
  # ������ ������ ������ ������ 
  myvar.mean <- mean(myvariable, na.rm = TRUE)
  # ������ ������ ������ ������ 
  myvar.median <- median(myvariable, na.rm = TRUE)
  # ������ ������ ������ �ֺ� 
  table.myvariable <- table(myvariable) 
  myvar.mode <- as.numeric(names(table.myvariable)[which.max(table.myvariable)])
  # �������� ������ ������ �ּҰ�, range()�� ��� ù��°�� �ּҰ��� �ι�°�� �ִ밪�� �����Ͽ��ش�.  
  myvariable.range.min <- range(myvariable, na.rm = TRUE)[1]
  # �������� ������ ������ �ִ밪
  myvariable.range.max <- range(myvariable, na.rm = TRUE)[2]  
  # �������� ������ ������ 25%°�� ��
  myvariable.interquartile25 <- quantile(myvariable, prob = c(0.25, 0.75), na.rm = TRUE)[1]
  # �������� ������ ������ 75%°�� ��
  myvariable.interquartile75 <- quantile(myvariable, prob = c(0.25, 0.75), na.rm = TRUE)[2]
  # �������� ������ ������ �л� 
  myvariable.var <- var(myvariable, na.rm = TRUE)
  # �������� ������ ������ ǥ������ 
  myvariable.sd <- sd(myvariable, na.rm = TRUE)
  # cbind()�� �̿��ؼ� �� ���ġ���� ��� �� descriptive.myvariable�̶�� ������Ʈ�� �����Ͽ���.  
  descriptive.myvariable <- rbind(myvar.mean,myvar.median,myvar.mode,
        myvariable.range.min,myvariable.range.max,
        myvariable.interquartile25,myvariable.interquartile75,
        myvariable.var,myvariable.sd,myvar.sum,
        myvar.length.missing.exclude,myvar.length.missing.include)
  # rownames()�� descriptive.myvariable�� �̸��� �����ϴ� �Լ��̴�. ������ �µ��� �̸��� �ٿ���.  
  rownames(descriptive.myvariable) <- c('������','������','�ֺ�',
        '�ּڰ�','�ִ�','25%��','75%��',
        '�л�','ǥ������','����',
        '��ʼ�(������ ����)','��ʼ�(������ ����)')
  colnames(descriptive.myvariable) <- '��' 
  # �Ҽ��� 2�ڸ����� �����ϵ��� �Ͽ���.  
  round(descriptive.myvariable, digit = 2)
} 

# ������ ������ q16m ������ �����Ͽ� ���� ������ �ߴ� ��İ� �������� �� �� ���� ���̴�.  
descriptive.statistics(pew$q16m)
# pew �����Ϳ��� '����(age)'�� ������ġ�� ���ϸ� ������ ���� �ϸ�俬�ϰ� ���´�. 
descriptive.statistics(pew$age)

