# 3��. ������ ����ó��
# 1��. ������ �� �Ϻθ� ��󳻱�
# 2. ������ �� �Ϻ� ��ʸ� ���ϱ�(selecting cases)

# �������� � Ư¡(feature)�� ���� �ִ� ��ʵ鸸�� ���ϱ� 
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# ��ȭ������θ� ������ ǥ�� ����� 
# �ε����� ���� ���ϴ� ��ʵ��� ��󳻴� ����� �Ʒ��� ����. Ư���� ��Ÿ���� ������ pew��� 
# ������Ʈ�� �����ִ�. �� pew$party��� ������Ʈ �� �� ���� 1�� ����(�� ��ȭ���� �����ϴ�) ���ǿ� 
# �ش�Ǵ� �����ٸ��� �͵����Ͽ� �ش� ��ʵ�� ������ 
# ���ο� ������Ʈ pew.republican.indexing �� �����Ͽ���.  
pew.republican.indexing <- pew[(pew$party == 1),]
# subset() �Լ��� ������ ���ǿ� �´� ��ʸ� ��󳻴� ����� �Ѵ�. 
# ���⼭�� pew ������Ʈ ���� party��� ������ 1�� ���� ���� ��ʵ鸸���� ������ ������Ʈ�� �����ϴ� ���̴�.  
pew.republican.subset <- subset(pew, party == 1) 
# �����̸� ���ִ���� �����ڷθ� ������ ǥ�� ����� 
# ���� �޶��� ���� ������, �߰������� ���ٿ���. ���� &�� �Ҹ���(Boolean) �Լ���� �Ҹ���.  
pew.female.democrat.indexing <- pew[(pew$party == 2 & pew$sex == 2),]
# ���� �ε��̿��� ������ �Ͱ�  ���������̴�.  
pew.female.democrat.subset <- subset(pew, party == 2 & sex == 2) 

# ��� Ư���� ���� ��ʷ� ������ �����Ϳ��� ���ϴ� �����鸸 �����ϱ�
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# colnames()�� ���ؼ� �����̸����� Ȯ��. ����('sex'), ����('race'), ����('age'), �ҵ�('inc'), 
# ��������('educ')�� ��������('party')�� ���� �� ���� ��� ����
var.keep <- c('sex','race','age','inc','educ','party')
# �Ʒ��� 4���� ����� ������ ���� �ٸ���� ������ ������
# (1) ������ �����ϴ� ����, �׸��� ������ Ư¡�� �����ϴ� ������ �ε����� ���� ������ �����ϴ� ���
pew.select.1 <- pew[,var.keep] 
pew.select.1 <- pew.select.1[(pew.select.1$party==1 & pew.select.1$sex==1),]
# (2) ������ �����ϴ� ����, �׸��� ������ Ư¡�� �����ϴ� ������ �ε��̰� 
# subset()�� �̿��Ͽ� ������ �����ϴ� ���
pew.select.2 <- pew[,var.keep] 
pew.select.2 <- subset(pew.select.2, party==1 & sex==1)
# (3) ������ �����ϴ� ����, �׸��� ������ Ư¡�� �����ϴ� ������ �ε����� ���� ���ÿ� �����ϴ� ���
pew.select.3 <- pew[(pew$party==1 & pew$sex==1),var.keep] 
# (4) ������ �����ϴ� ����, �׸��� ������ Ư¡�� �����ϴ� ������ subset()�� �̿��Ͽ� ���ÿ� �����ϴ� ���
pew.select.4 <- subset(pew, party==1 & sex==1)[,var.keep] 




