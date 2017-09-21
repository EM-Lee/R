# 3��. ������ ����ó��
# 2��. ���� ����
# 3. ���ڵ��ϱ�(recoding)

# �ռ� ����ߴ� tess �����͸� �ҷ����� ���� �����Ѵ�. 
# �켱 XREL1 ������ ��Ѱ��� ���캻��. 
table(tess$XREL1, useNA = 'always')
# ���ڵ��� ������ �켱 �غ��Ѵ�.
tess$religion5 <- tess$XREL1
# XREL1�� ���� ���� religion5������ ���� ���ڵ��Ѵ�. []���� | ǥ�ô� or�� �ǹ��̴�. 
# �� XREL1������ ���� ���ǿ� �ϳ��� �ش�Ǹ� �캯�� ��(100��500)�� �ο��Ѵ�. 
tess$religion5[tess$XREL1 == 3] <- 100
tess$religion5[tess$XREL1 == 1 | tess$XREL1 == 2 | tess$XREL1 == 4 | tess$XREL1 == 9] <- 200
tess$religion5[tess$XREL1 == 10 | tess$XREL1 == 11] <- 300
tess$religion5[tess$XREL1 == 13] <- 400
tess$religion5[tess$XREL1 == 5 | tess$XREL1 == 6 | tess$XREL1 == 7 | 
	tess$XREL1 == 8 | tess$XREL1 == 12] <- 500
# ���ڵ��� ����� ����Ǿ����� Ȯ���Ѵ�. 
table(tess$XREL1, tess$religion5, useNA = 'always')

# ���ڵ��� ������ �켱 �غ��Ѵ�.
tess$religion5.text <- tess$XREL1
# XREL1�� ���� ���� religion5.text������ ���� ���ڵ��Ѵ�. 
tess$religion5.text[tess$XREL1 == 3] <- 'catholic'
tess$religion5.text[tess$XREL1 == 1 | tess$XREL1 == 2 | tess$XREL1 == 4 
	| tess$XREL1 == 9] <- 'protestant'
tess$religion5.text[tess$XREL1 == 10 | tess$XREL1 == 11] <- 'other.cristian'
tess$religion5.text[tess$XREL1 == 13] <- 'atheist'
tess$religion5.text[tess$XREL1 == 5 | tess$XREL1 == 6 | tess$XREL1 == 7 
	| tess$XREL1 == 8 | tess$XREL1 == 12] <- 'misc.religion'
# ���ڵ��� ����� ����Ǿ����� Ȯ���Ѵ�. 
table(tess$XREL1, tess$religion5.text, useNA = 'always')

# car ���̺귯���� �ν���Ǿ� ���� �ʴٸ� install.packages('car')�� �̿��Ͽ� ���� �ν����� ��Ű���� �Ѵ�. 
library('car')
# recode()�� �̿��Ͽ� tess$XREL1�� ""���̿� �µ��� ���ڵ��Ѵ�. 
tess$religion5.car <- recode(tess$XREL1, "1=200; 2=200; 3=100; 4=200; 
	5=500; 6=500; 7=500; 8=500; 9=200; 10=300; 11=300; 12=500; 13=400")
# ���ڵ��� ����� ����Ǿ����� Ȯ���Ѵ�. 
table(tess$XREL1, tess$religion5.car, useNA = 'always')

# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", use.value.labels = FALSE, to.data.frame = TRUE)
# Q4�� �������� -1�� ���������� ó���Ѵ�.  
table(tess$Q4)
# 3������ ��� ������ ���� �������� �Ǵ��ϱ� ���ؼ� Q4�� ������ �����Ͽ���. dim()�� �պκп��� �����Ͽ���. 
# dim()���� �����͸� ���Ǹ� ù��° ���� ������ ��, �׸��� �ι�° ���� ������ ������ �� �� �ִ�. 
# �� �Ʒ��� ��ɹ��� Q4�� �� ���� ���� ����(proportion)�� Ȯ���� �� �ִ�.  
table(tess$Q4)/dim(tess)[1]
# ���� ������ �������� [0,3]�� ��� 'Very insensitive(100)', [4]�� ��� 'Insensitive(200)', 
# [5,6] �� ��� 'Sensitive(300)', [7,10]�� ��� 'Very Sensitive(400)'�� �����Ͽ���. 
# (���ֺ� ��ʼ��� ����ϰ� ��)
# ���ڵ��� ������ Q4r�� �����Ѵ�.
tess$Q4r <- tess$Q4
# �������� ó���Ͽ���.  
tess$Q4r[tess$Q4 == -1] <- NA 
# ���ڵ��� �ǽ��Ͽ���.  []���� &�� ����(joint)������ ��Ÿ����. 
# A&B�� ��A�� B�� ������ �� ������ų ������� ǥ���̴�. 
tess$Q4r[tess$Q4 >= 0 & tess$Q4 <= 3] <- 100
tess$Q4r[tess$Q4 == 4]  <- 200
tess$Q4r[tess$Q4 == 5 | tess$Q4 == 6]  <- 300
tess$Q4r[tess$Q4 >= 7 & tess$Q4 <= 10]  <- 400
# ���ڵ��� ����� �ǽõǾ����� üũ�Ͽ���.  
table(tess$Q4, tess$Q4r, useNA = 'always')
# �� ���ֺ� ������ üũ�Ͽ���.  
table(tess$Q4r, useNA = 'always')/dim(tess)[1]

# 0��10�� 11���� ���� 4���� �����ϰ� ��ȹ�� �� ���ڵ��ϴ� ����, [0, 2.5]�� 'Very insensitive(100)', 
# (2.5,5]�� ��� 'Insensitive(200)', (5,7.5]�� ��� 'Sensitive(300)', 
# (7.5,10]�� ��� 'Very Sensitive(400)'�� �����Ͽ���. (���ֺ� �������� ������ ����ϰ� ��)
# ���ڵ��� ������ Q4r2�� �����ϰ� ?1�� ���������� ������ó���Ͽ���. 
tess$Q4r2 <- tess$Q4
tess$Q4r2[tess$Q4 == -1] <- NA 
# cut() �Լ��� �ش� ���͸� breaks �� ������ ���ڿ� �µ��� ��ȹ�����ش�. ����ߵ� �� 4���� �׷����� ��������. 
tess$Q4r2 <- cut(tess$Q4r2, breaks = 4)
# ���ڵ��� ����� �ǽõǾ����� üũ�Ͽ���.  
table(tess$Q4, tess$Q4r2, useNA = 'always')
# �� ���ֺ� ������ üũ�Ͽ���.  
table(tess$Q4r2, useNA = 'always')/dim(tess)[1]
# cut() �Լ��� �̿��ؼ� 3�������� �����ϴ� ���� ���� ���ϰ� �� ���� �ִ�. �� ��� quantile()�� �̿��ϸ� 
# ���ϴ�. quantile()�� prob �ɼ��� ������ ������ �´� ������ �ش��ϴ� ������Ʈ�� ���� �������ش�. 
# 3������ �����ϱ� ���ؼ��� 4���� ����Ʈ�� �ʿ��ϴ�. 
# ���ڵ��� ������ Q4r3�� �����ϰ� ?1�� ���������� ������ó���Ͽ���. 
tess$Q4r3 <- tess$Q4
tess$Q4r3[tess$Q4r3 == -1] <- NA 
# �ش纯���� 3�������� ������ 4���� ����Ʈ�� �����Ͽ���. ���� ������ �ϴ� ������δ� 
# prob = (0:3)/3�� �̿��� ���� �ִ�. na.rm = TRUE�� Q4r3�� �������� �ֱ� �����̴�. 
q4.point <- quantile(tess$Q4r3, prob = c(0, 1/3, 2/3, 1), na.rm = TRUE)
# �տ��� ����� q4.point�� ���� Q4r3�� �������־���. 
tess$Q4r3 <- cut(tess$Q4r3, q4.point)
# �պκп��� �� �Ͱ� cut()�� �̿��� ���� ������ Ȯ���Ͽ���. 
table(tess$Q4r, tess$Q4r3, useNA = 'always')


