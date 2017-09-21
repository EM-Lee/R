# 3��. ������ ����ó��
# 2��. ���� ����
# 4. (���� �����Ϳ�����) ����(factor)���� ��ȯ

# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# �ش� �����Ϳ��� �������� ��ġ�� �����ڱ��� XTESS089��� �����̴�.  
table(tess$XTESS089)
# is.numeric()�� �ش� ������Ʈ�� ��ġ��(numeric)������ �׽�Ʈ�Ѵ�. TRUE�� ��� ��ġ������, 
# FALSE�� ���� ��ġ�� �ƴ��� ��Ÿ����.  ���������� is.factor() ���� �����̸� TRUE, 
# ������ �ƴϸ� FALSE �� �����Ѵ�.  
is.numeric(tess$XTESS089) 
is.factor(tess$XTESS089) 
# as.factor()�� �ش� ����(����)�� ����(factor)�� �ٲپ��ִ� �Լ��̴�.  
tess$stimulus <- as.factor(tess$XTESS089) 
# is.numeric()�� is.factor()�� ���ؼ� stimulus�� ���������� ��ȭ�Ͽ����� Ȯ���� �� �ִ�.  
is.numeric(tess$stimulus) 
is.factor(tess$stimulus) 
# �������� �� ������ ��� as.numeric�� ����Ͽ� ��ġ���� ������ �ٽ� ��ȭ�����ϴ�. 
tess$stimulus.num <- as.numeric(tess$stimulus)
# is.numeric()�� is.factor()�� ���ؼ� stimulus.num�� ��ġ�� ������ ��ȭ������ Ȯ���� �� �ִ�.  
is.numeric(tess$stimulus.num) 
is.factor(tess$stimulus.num) 



