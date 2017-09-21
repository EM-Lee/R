# 4부. 통계분석
# 1장. R프로그래밍을 통한 확률분포 학습
# 2. R프로그래밍으로 살펴본 중심극한정리

# 카이제곱분포의 경우(표본 사이즈는 50, 자유도는 2일 경우)
# 우선 시뮬레이션된 카이제곱분포의 평균값들 10,000개를 저장할 수 있는 벡터를 구성한다.  
sim.chi.mean <- rep(NA,10000)
# 카이제곱분포에서 표본의 사이즈가 50이고 자유도가 2인 값들의 평균을 10,000번 반복한 후 
# sim.chi.mean 벡터에 배열한다. 
for (i in 1:10000) {
  sim.chi.mean[i] <- mean(rchisq(50, 2))
  }
# 얻어진 벡터를 평균이 0, 표준편차가 1이 되도록 표준화시킨다.  
sim.chi.mean.std <- scale(sim.chi.mean)
# 하나의 패널에 2개의 그림이 배치되도록 구획한다.  
par(mfrow=c(1,2))
# 표준화시키기전의 sim.chi.mean의 히스토그램을 왼쪽에 배치한다. 
# prob=T의 의미는 Y축에 빈도(frequency)가 아닌 확률(probability)을 배열한다는 의미이다.  
# main옵션의 \n은 줄을 바꾼다는 의미이다.
hist(sim.chi.mean, prob=T, 
	main='Histogram of means of simulated chi-squares\n(Raw values)')
# 표준화시킨 후 sim.chi.mean.std의 히스토그램을 오른쪽에 배치한다. 
hist(sim.chi.mean.std, prob=T, 
	main='Histogram of means of simulated chi-squares\n(Standardized values)')
# 표준화시킨 후 sim.chi.mean.std의 히스토그램에 평균이 0, 표준편차가 1이며 
# 범위가 -3에서 3까지인 이론적 정규분포곡선을 추가시킨다.  
curve(dnorm(x,0,1), -3, 3, col='blue', add=T)

# F분포의 경우(df1 = 2, df2 = 47, 표본 사이즈는 50)
# 우선 시뮬레이션된 F분포의 평균값들 10,000개를 저장할 수 있는 벡터를 구성한다.  
sim.f.mean <- rep(NA,10000)
# F분포에서 표본의 사이즈가 50이고 자유도가 2인 값들의 평균을 10,000번 반복한 후
# sim.f.mean 벡터에 배열한다. 
for (i in 1:10000) {
  sim.f.mean[i] <- mean(rf(50, 2, 47))
  }
# 얻어진 벡터를 평균이 0, 표준편차가 1이 되도록 표준화시킨다.  
sim.f.mean.std <- scale(sim.f.mean)
# 하나의 패널에 2개의 그림이 배치되도록 구획한다.  
par(mfrow=c(1,2))
# 표준화시키기 전의 sim.f.mean의 히스토그램을 왼쪽에 배치한다. 
hist(sim.f.mean, breaks = 30, prob = T, 
	main='Histogram of means of simulated F-values\n(Raw values)')
# 표준화시킨 후인 sim.f.mean.std의 히스토그램을 오른쪽에 배치한다. 
hist(sim.f.mean.std, breaks = 30, prob = T, 
	main='Histogram of means of simulated F-values\n(Standardized values)')
# 표준화시킨 후인 sim.f.mean.std의 히스토그램에 평균이 0, 표준편차가 1이며 범위가 -3에서 3까지인 
# 이론적 정규분포곡선을 추가시킨다.  
curve(dnorm(x,0,1), -3, 3, col='blue', add = T)

# 이항분포의 경우(n = 50, k = 15, p = .50)
# 우선 시뮬레이션된 이항분포의 평균값들 10,000개를 저장할 수 있는 벡터를 구성한다.  
sim.binom.mean <- rep(NA,10000)
# F분포에서 표본의 사이즈가 50이고 자유도가 2인 값들의 평균을 10,000번 반복한 후 
# sim.binom.mean 벡터에 배열한다. 
for (i in 1:10000) {
  sim.binom.mean[i] <- mean(rbinom(15, 50, 0.50))
  }
# 얻어진 벡터를 평균이 0, 표준편차가 1이 되도록 표준화시킨다.  
sim.binom.mean.std <- scale(sim.binom.mean)
# 하나의 패널에 2개의 그림이 배치되도록 구획한다.  
par(mfrow=c(1,2))
# 표준화시키기전의 sim.binom.mean의 히스토그램을 왼쪽에 배치한다. 
hist(sim.binom.mean, breaks = 30, prob = T,
	main='Histogram of means from simulated binominal distribution\n(Raw values)')
# 표준화시키기후인 sim.binom.mean.std의 히스토그램을 오른쪽에 배치한다. 
hist(sim.binom.mean.std, breaks = 30, prob = T, 
	main='Histogram of means from simulated binominal distribution\n(Standardized values)')
# 표준화시키기후인 sim.binom.mean.std의 히스토그램에 평균이 0, 표준편차가 1이며 
# 범위가 -3에서 3까지인 이론적 정규분포곡선을 추가시킨다.  
curve(dnorm(x,0,1), -3, 3, col='blue', add = T)

  	