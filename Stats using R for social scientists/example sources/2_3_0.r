# 2��. ������ ����
# 3��. �������� ���հ� �м�

# library()�� �̿�, ���������� �ҷ����� ���ؼ� xlsx ���̺귯���� Ȱ��ȭ��Ŵ
# ���� xlsx ���̺귯���� ������ install.packages("xlsx")�� �̿��Ͽ� �ν����� ��.
library('xlsx')
# �켱 ������ ������ ���������� �ҷ��� (sheetIndex = 1�� �����α����)
life.world.xlsx <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls",
		sheetIndex = 1, header = TRUE)
# �������� ������ ���δ� GDP ���������� �ҷ���
gdp.world.xlsx <- read.xlsx(file = "C:/data/example/WorldBank_GDPperCapita_2010.xls",
		sheetIndex = 1, header = TRUE)
# �����ڵ�(Country.Code)�� �߽����� ���δ� GDP�� �������� ���� ��ġ��Ŵ
life.gdp <- merge(gdp.world.xlsx, life.world.xlsx, by = 'Country.Code')
# head() ��ɹ��� ��ȣ�� ������Ʈ�� ù 6�ٿ� �ش��ϴ� �����͸� �������ش�.
# �� ���� head() ��ɹ��� ���ؼ� ������ ��ġ�Ⱑ ����� ����Ǿ����� Ȯ���Ͽ���.
head(gdp.world.xlsx)
head(gdp.world.xlsx)
head(life.gdp)

# �켱 ������ ������ ���������� �ҷ��� (sheetIndex = 2�� ������ ������)
# �켱 ������ ������ ���������� �ҷ��� (sheetIndex = 3�� ������ ������)
life.world.fe <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls",
	sheetIndex = 2)
life.world.ma <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls",
	sheetIndex = 3)
# �� �����ͺ��� �ش�Ǵ� �������� �������� �ƴϸ� ���������� ������
# ������ �����ӿ� ���̴� $��� ǥ�ô� �����Ͷ�� ���� ������Ʈ�� �����ִ� gender��� ����,
# �� �����Ϳ� �μӵ� ���� ������Ʈ���� ��Ÿ����.
# �ٽ� ���� life.world.fe$gender <- 'female�� ��
# life.world.fe�� �ִ� gender��� ������ ��female���̶�� �Է°��� ��ġ�Ͽ����� �ǹ��Ѵ�.
life.world.fe$gender <- 'female'
life.world.ma$gender <- 'male'
# life.world.fe�� life.world.ma�� �������� �������� �״� ������� ���Խ�Ŵ.
life.fe.ma <- data.frame(rbind(life.world.fe, life.world.ma))
# dim() ��ɹ��� ���ؼ� ������(��������)�� ������(����)�� ���� Ȯ���Ͽ���.
# dim()�� �־��� ������Ʈ�� ����(dimension)�� ��ʼ��� �����ش�.
# ������ ������ �������� R��C�� 2���� ���(matrix)�̱� ������ �������� ���� �������� ���� ǥ�õȴ�.
# ����� ���� life.world.fe�� life.world.ma�� ��� 246���� �����ٰ� 3���� ��������,
# �׸��� life.fe.ma�� ��� 492���� �����ٰ� 3���� �������� ������ ���� Ȯ���� �� �ִ�.
# �� �������� ������ ������� ������ �������� ���� ǥ�õ� ���������� Ȯ���� �� �ִ�.
dim(life.world.fe)
dim(life.world.ma)
dim(life.fe.ma)


