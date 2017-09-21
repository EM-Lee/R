# 2��. ������ ����
# 2��. ������ �Է��ϱ� �� �ҷ�����
# 2. �ܺ� ������ �ҷ�����

# load() ��ɹ��� () �� file�̶�� ��õ� ���� ����� R������ �����͸� �ҷ����� ��ɹ��̴�.
# ����� ���� R�� ��� ������ ������ ������ ������ ��������(\)�� �ƴ϶� ������(/)�� �̿��Ѵ�.
# ���� �����ø� �̿��ϰ� ���� �ʴٸ�, �Ʒ�ó�� �������ø� �ι� ���� �ȴ�
# ("C:\\data\\example\\WorldBank_LifeExpectancy_2010.rda").
life.world.rda <- load(file = "C:/data/example/WorldBank_LifeExpectancy_2010.rda")

# install.packages()�� ��ȣ���� ���̺귯���� �ν����Ű�� ��ɹ��̴�.
install.packages('xlsx')
# ��xlsx�� ���̺귯���� �ν���Ǿ� ���� ���� ��� ���� ��ɹ� �����ؾ� ������,
# �̹� �ν���Ǿ� �ִٸ� ������ �ʿ䰡 ����.
# library()�� ��ȣ���� ���̺귯���� Ȱ��ȭ��Ű�� ��ɹ��̴�. �ν����� �Ǿ� �־
# library() ��ɹ��� ���̴� xlsx ���̺귯�� ����� ����� �� ����.
library('xlsx')
# read.xlsx() ��ɹ��� 2007���� ������ ����ȭ��(*.xlsx)�� �ҷ����� ����� �Ѵ�.
# read.xlsx() �� file �ɼ��� ����� ����ȭ���� ������ġ�� ��Ÿ����. �ռ��� read.xls() �� �����ϴ�.
# read.xlsx() �� sheetIndex �ɼ��� ����� ����ȭ���� ��(tab)�� ������ ��Ÿ����.
# sheetIndex = 1 �̸� ù��° ���̸� sheetIndex = 2��� �ι�° ���̴�.
life.world.xlsx <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xlsx",
sheetIndex = 1)
# read.xlsx() ��ɹ��� 2007 ���������� ����ȭ��(*.xls)������ ���������� �۵��Ѵ�.
life.world.xls <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls", sheetIndex = 1)

# read.table()�� ��ȣ���� ������ ��ǥ(table)������ ������Ʈ�� �ҷ����� �Ѵ�.
# read.table()�� sep�� �Է°����� �и��� ����� ����Ѵ�. ����ǥ �ӿ� �޸��� ������
# �޸��� ���еǾ����� �ǹ��ϰ�, �ݷ��� �� ������ �ݷ����� ���еǾ����� �ǹ��Ѵ�.
life.world.spc <- read.table(file = "C:/data/example/WorldBank_LifeExpectancy_2010.csv",
		sep = ',', header = TRUE)
# ���� �޸��� ���е� ���� �ƴ϶� �ݷ����� ���еǾ��ٸ�, �Ʒ��� ���� ��ɹ��� �����ϸ� �ȴ�.
life.world.cln <- read.table(file = "C:/data/example/WorldBank_LifeExpectancy_2010.txt",
		sep = ';', header = TRUE)
# CSV ������ ������ �������ϰ� ���̱� ������ read.csv()��� ������ ��ɹ��� �����Ѵ�.
# ������ read.table�� ����������, header��� �ɼ��� �̿��ؼ� ������ ù���� �̸���
# ���������� �ν��ϰ� �� �� �ִ�. header = TRUE�� ù �������� �Է°��� �����̸�����
# �Է��ϵ��� �ϰڴٴ� �ǹ��̴�. ���� ������ ù���� �������� �ƴ϶�� header = FALSE ��� ����ϸ� �ȴ�.
life.world.spc <- read.csv(file = "C:/data/example/WorldBank_LifeExpectancy_2010.csv", header = TRUE)

# foreign ���̺귯���� �ν���Ǿ� ���� ���� ��� ������ ��ɹ� �����ؾ� �Ѵ�.
# �׷��� �̹� �ν���Ǿ� �ִٸ� ������ �ʿ䰡 ����. ���� ��ɹ��� �����ϸ� ������ ������� �߸鼭
# ��� �ٿ�ε带 �ϱ� ���ϴ����� ���´�. ��� �ƹ� ������ �ٿ��� �޾Ƶ� ��������� �׳� �⺻��(default)��
# ������ ���� ���� �����ϴ�.
install.packages('foreign')
# �ν����� ��ģ�� library()�� �̿��Ͽ� foreign ���̺귯���� Ȱ��ȭ��Ų��.
library('foreign')
# read.spss() ��ɹ��� SPSS ������ ������ �ҷ����� ����� �Ѵ�.
# read.spss()�� use.value.labels = FALSE �� ���, SPSS�� ������ �Է°��� ���� ���� ����
# �ҷ����� �ʰ� �����Ϳ� ��ϵ� �״�� �ҷ��´ٴ� ���� �ǹ��Ѵ�. ��ȸ���п����� 5�� Ȥ��
# 7�� ô���� ���� �̿��Ѵ�. �̸��׸� ��å�� ���� ��ȣ�� ��1 = ���� �������� �ʴ´١�
# ��5 = �ſ� �����Ѵ١���� �Ѵٸ� use.value.labels = TRUE��� �Ǿ� �ִ� ��� R �����Ϳ� �Է°���
# 1 Ȥ�� 5�� �ԷµǴ� ��� ������ �������� �ʴ´١� Ȥ�� ���ſ� �����Ѵ١��� �Էµȴ�.
# use.value.labels = FALSE�� ��� ������ �Է°��� 1��5������ ���ڰ� R �����Ϳ� �Էµȴ�.
# ���������δ� use.value.labels = FALSE �� �������� ó���� �� ���ϴٰ� ��õ�Ѵ�.
# read.spss()�� to.data.frame = TRUE �� ���, �ҷ��� SPSS�� �����͸� data frame�� ���·�
# �����ϴ� ���� �ǹ��Ѵ�. ���� to.data.frame = FALSE ��� �Ǿ� �ִ� ��� �����͸�
# data frame�� �ƴ� ��Ʈ���� ���·� �����Ѵ�. ���м��� ���ؼ��� data frame ������ �����ͼ��� �����ؾ�
# �ϱ� ������, �����ϸ� �ɼ��� TRUE��� ������ ���� ���Ѵ�.
life.world.sps <- read.spss(file = "C:/data/example/WorldBank_LifeExpectancy_2010.sav", use.value.labels = FALSE, to.data.frame = TRUE)
# SAS ������ ������ read.ssd��� ��ɹ����� �����ϴ�. ?read.ssd�� ���� �ڼ��� �̿���� �̿��ϱ� �ٶ���.
# STATA ������ ������ read.dta��� ��ɹ����� �����ϴ�. ?read.dta�� ���� �ڼ��� �̿���� �̿��ϱ� �ٶ���.


