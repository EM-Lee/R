# 4��. ���м�
# 1��. R���α׷����� ���� Ȯ������ �н�
# 2. R���α׷������� ���캻 �߽ɱ�������

# ī������������ ���(ǥ�� ������� 50, �������� 2�� ���)
# �켱 �ùķ��̼ǵ� ī������������ ��հ��� 10,000���� ������ �� �ִ� ���͸� �����Ѵ�.  
sim.chi.mean <- rep(NA,10000)
# ī�������������� ǥ���� ����� 50�̰� �������� 2�� ������ ����� 10,000�� �ݺ��� �� 
# sim.chi.mean ���Ϳ� �迭�Ѵ�. 
for (i in 1:10000) {
  sim.chi.mean[i] <- mean(rchisq(50, 2))
  }
# ����� ���͸� ����� 0, ǥ�������� 1�� �ǵ��� ǥ��ȭ��Ų��.  
sim.chi.mean.std <- scale(sim.chi.mean)
# �ϳ��� �гο� 2���� �׸��� ��ġ�ǵ��� ��ȹ�Ѵ�.  
par(mfrow=c(1,2))
# ǥ��ȭ��Ű������ sim.chi.mean�� ������׷��� ���ʿ� ��ġ�Ѵ�. 
# prob=T�� �ǹ̴� Y�࿡ ��(frequency)�� �ƴ� Ȯ��(probability)�� �迭�Ѵٴ� �ǹ��̴�.  
# main�ɼ��� \n�� ���� �ٲ۴ٴ� �ǹ��̴�.
hist(sim.chi.mean, prob=T, 
	main='Histogram of means of simulated chi-squares\n(Raw values)')
# ǥ��ȭ��Ų �� sim.chi.mean.std�� ������׷��� �����ʿ� ��ġ�Ѵ�. 
hist(sim.chi.mean.std, prob=T, 
	main='Histogram of means of simulated chi-squares\n(Standardized values)')
# ǥ��ȭ��Ų �� sim.chi.mean.std�� ������׷��� ����� 0, ǥ�������� 1�̸� 
# ������ -3���� 3������ �̷��� ���Ժ������ �߰���Ų��.  
curve(dnorm(x,0,1), -3, 3, col='blue', add=T)

# F������ ���(df1 = 2, df2 = 47, ǥ�� ������� 50)
# �켱 �ùķ��̼ǵ� F������ ��հ��� 10,000���� ������ �� �ִ� ���͸� �����Ѵ�.  
sim.f.mean <- rep(NA,10000)
# F�������� ǥ���� ����� 50�̰� �������� 2�� ������ ����� 10,000�� �ݺ��� ��
# sim.f.mean ���Ϳ� �迭�Ѵ�. 
for (i in 1:10000) {
  sim.f.mean[i] <- mean(rf(50, 2, 47))
  }
# ����� ���͸� ����� 0, ǥ�������� 1�� �ǵ��� ǥ��ȭ��Ų��.  
sim.f.mean.std <- scale(sim.f.mean)
# �ϳ��� �гο� 2���� �׸��� ��ġ�ǵ��� ��ȹ�Ѵ�.  
par(mfrow=c(1,2))
# ǥ��ȭ��Ű�� ���� sim.f.mean�� ������׷��� ���ʿ� ��ġ�Ѵ�. 
hist(sim.f.mean, breaks = 30, prob = T, 
	main='Histogram of means of simulated F-values\n(Raw values)')
# ǥ��ȭ��Ų ���� sim.f.mean.std�� ������׷��� �����ʿ� ��ġ�Ѵ�. 
hist(sim.f.mean.std, breaks = 30, prob = T, 
	main='Histogram of means of simulated F-values\n(Standardized values)')
# ǥ��ȭ��Ų ���� sim.f.mean.std�� ������׷��� ����� 0, ǥ�������� 1�̸� ������ -3���� 3������ 
# �̷��� ���Ժ������ �߰���Ų��.  
curve(dnorm(x,0,1), -3, 3, col='blue', add = T)

# ���׺����� ���(n = 50, k = 15, p = .50)
# �켱 �ùķ��̼ǵ� ���׺����� ��հ��� 10,000���� ������ �� �ִ� ���͸� �����Ѵ�.  
sim.binom.mean <- rep(NA,10000)
# F�������� ǥ���� ����� 50�̰� �������� 2�� ������ ����� 10,000�� �ݺ��� �� 
# sim.binom.mean ���Ϳ� �迭�Ѵ�. 
for (i in 1:10000) {
  sim.binom.mean[i] <- mean(rbinom(15, 50, 0.50))
  }
# ����� ���͸� ����� 0, ǥ�������� 1�� �ǵ��� ǥ��ȭ��Ų��.  
sim.binom.mean.std <- scale(sim.binom.mean)
# �ϳ��� �гο� 2���� �׸��� ��ġ�ǵ��� ��ȹ�Ѵ�.  
par(mfrow=c(1,2))
# ǥ��ȭ��Ű������ sim.binom.mean�� ������׷��� ���ʿ� ��ġ�Ѵ�. 
hist(sim.binom.mean, breaks = 30, prob = T,
	main='Histogram of means from simulated binominal distribution\n(Raw values)')
# ǥ��ȭ��Ű������ sim.binom.mean.std�� ������׷��� �����ʿ� ��ġ�Ѵ�. 
hist(sim.binom.mean.std, breaks = 30, prob = T, 
	main='Histogram of means from simulated binominal distribution\n(Standardized values)')
# ǥ��ȭ��Ű������ sim.binom.mean.std�� ������׷��� ����� 0, ǥ�������� 1�̸� 
# ������ -3���� 3������ �̷��� ���Ժ������ �߰���Ų��.  
curve(dnorm(x,0,1), -3, 3, col='blue', add = T)

  	