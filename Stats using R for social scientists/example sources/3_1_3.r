# 3��. ������ ����ó��
# 1��. ������ �� �Ϻθ� ��󳻱�
# 3. ���ܼ����� ���ġ�� ����� �����ͼ�(aggregated dataset)

# �������� ����(data aggregation)
# state�� ��� CSV ������ ��(�)�� ���ڷ� �ԷµǾ� �ִ�. �� ��� SPSS������ ������ �ҷ����� ���ѵ�, 
# �� ������ SPSS�� ��� ������������ ���̺�(label)�� �پ��־� � ���ڰ� � ������ ���� �˾ƺ� �� �ֱ�
# �����̴�. ���̺��� �ҷ����� ���ؼ��� use.value.labels�� �ɼ��� TRUE�� �ٲپ��ָ� �ȴ�. 
pew <- read.spss(file = "C:/data/example/Omnibus_Oct_2012_Political_Video_spss.sav", 
	use.value.labels = TRUE, to.data.frame = TRUE)
# aggregate()�� ī�װ����� Ư���� ������ ���ġ�� ����Ͽ��ִ� �Լ��̴�. �Ʒ��� ��ɹ��� ��(�)���� 
# �����ڵ��� ��������� ����� �� pew.state.age.mean�̶�� ������Ʈ�� �����϶�� ���̴�. 
# FUN�� aggregation�� ����� ���� �����϶�� �ǹ��̴�. 
pew.state.age.mean <- aggregate(age �� state, data = pew, FUN = mean)
# ���� �ǹ̴� ����. FUN = median���� ������ �� �ֵ� �����ڵ��� ���� �߰����� ����� �� �����ϴ� ���� �ٸ���.  
pew.state.age.median <- aggregate(age �� state, data = pew, FUN = median)
# ���� �ǹ̴� ����. FUN = length�� �� �ֺ��� �����ڵ��� �� �������� ����ϴ� ������ �Ѵ�. 
# length()��� �Լ��� �־��� ����(vector)�� ����, �� ��ʼ��� ������ ������ִ� �Լ��̴�. 
pew.state.age.case <- aggregate(age �� state, data = pew, FUN = length)
# ���� �� ���� ������Ʈ���� ������ �̸�(�� �����̸�)�� Ȯ���Ͽ���. ������� �� �� �ֵ� �� ��° ������ �̸��� 
# ��� age�� ���� ������, �츮�� �ش� �̸��� ���� �������� �ٸ��ٴ� ���� �˰� �ִ�. 
colnames(pew.state.age.mean)
colnames(pew.state.age.median)
colnames(pew.state.age.case)
# ���� �� ��° �������� �̸��� ���� �ٸ��� ����Ͽ���.  �̸��׸� colnames(pew.state.age.mean)[2]�� ��� 
# [2]��� �ε����� ���������ν� ������Ʈ�� ������ �̸� ��� �� �� ��° �̸��� �ε����� �� �ִ�. 
# �캯���� Ȯ���� �� �ֵ� �ش� �������� �̸��� mean.age��� �̸����� �ο��Ǿ���. 
colnames(pew.state.age.mean)[2] <- "mean.age"
colnames(pew.state.age.median)[2] <- "median.age"
colnames(pew.state.age.case)[2] <- "number.cases"
# �ռ� �н��ߴ� ������ġ�⸦ ���� ���յ� ������(�̸��� pew.state.age)�� ���� �� �ִ�. 
pew.state.age <- merge(pew.state.age.case, pew.state.age.mean, by = "state")
pew.state.age <- merge(pew.state.age, pew.state.age.median, by = "state")
# dim()�� ���� �� ���� �����ٰ� �������� ���� ���������� Ȯ���Ͽ���. �� 49���� �������� ������ �ִµ�,
# �� ������ �˷���ī(Alaska)�� �Ͽ���(Hawaii), �� �� ���� ��� �� �Ѹ��� �����ڵ� ǥ������ �ʾұ� �����̴�. 
dim(pew.state.age)

# merge()�� �ƴ� data.frame()�� �̿��Ͽ� �����͸� ��ĥ ���� �ִ�. �׷��� data.frame()�� ��� �������� 
# ��Ȯ�ϰ� ��ġ���� ���� ��� �����͸� ��ĥ �� ������ �߻��ϱ� ������ ���������δ� ���� ������ �ʴ´�. 
# �� ��� pew.state.age.mean, pew.state.age.median, pew.state.age.case �� �� ������Ʈ�� ��Ȯ�ϰ� 
# ��ġ�ϴ� ������ ������ ������ �ֱ� ������ �Ʒ��� ���� ������ ��ġ�⵵ �����ϴ�.  pew.state.age.median�� 
# pew.state.age.case�� ��� �� ��° �����ٸ� �ε����� �� ������ ù ��° ������(��(�)�� �̸�)�� ��� 
# pew.state.age.mean�� �̹� ������ �Ǿ� �־� �ߺ��Ǳ� �����̴�. 
pew.state.age <- data.frame(pew.state.age.mean,pew.state.age.median[,2],pew.state.age.case[,2])




