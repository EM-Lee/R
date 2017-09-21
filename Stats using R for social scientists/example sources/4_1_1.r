# 4부. 통계분석
# 1장. R프로그래밍을 통한 확률분포 학습
# 1. 사회과학에서 자주 등장하는 확률분포

# R프로그래밍을 이용한 정규분포 재현
# 아래의 seq() 함수는 일련의 벡터(vector)를 생성하는 함수이다. 즉 x라는 벡터를 만드는데, 
# -4부터 4까지 범위의 숫자를 같은 간격을 갖도록 총 500개를 생성한다. 
x <- seq(-4, 4, length = 500)
# 아래는 앞에서 설명한 정규분포의 확률밀도함수를 프로그래밍한 것이다. sqrt()는 루트(square root)를, 
# pi는 파이값이며, exp()은 지수함수이다. 
y <- (1/sqrt(2*pi))*exp(-(x^2/2))
# plot()는 x 오브젝트를 X축에, 그리고 y 오브젝트를 Y축에 배치하는 플롯을 그린다. 
# 매우 자주 등장하는 함수이므로 반드시 숙지한다. type 옵션의 l은 그래프를 그릴 때 line, 
# 즉 선으로 표시하는 플롯이라는 의미이고, col은 color, 즉 색깔의 약자로 선의 색을 붉은 색으로 표시하였다. 
# main 옵션으로 그래프의 이름을 붙일 수 있다. 
plot(x, y, type = "l", col = "red", main = "Normal distribution, example")

# R프로그래밍을 이용한 정규분포 이해 
# 위에서와 마찬가지로 즉 x라는 벡터를 만드는데, -4부터 4까지 범위의 숫자를 같은 간격을 갖도록 
# 총 500개를 생성하였다. 
x <- seq(-4, 4, length = 500)
# dnorm() 함수를 통해서 앞에서 소개한 4가지 시나리오에 따른 함수값을 생성하였다. mean은 가정된 모집단 평균을, 
# 그리고 sd는 가정된 모집단 표준편차를 설정하는 옵션이다. 
y1 <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 0, sd = 2)
y3 <- dnorm(x, mean = 1, sd = 1)
y4 <- dnorm(x, mean = 1, sd = 2)
# plot()를 이용해서 x 오브젝트를 X축에, 그리고 y1 오브젝트를 Y축에 배치하는 플롯을 그렸다. 
# ylim 옵션을 이용하여 y축의 범위를 조정할 수 있다. 범례를 넣기 위해 여백을 생성하였다. 
# type과 col, main 옵션들은 앞에서 이미 설명하였다. 
plot(x, y1, type = "l", col = "red", ylim = c(0,0.6), main = "t distribution")
# lines()은 앞에서 그린 플롯 위에 x 오브젝트를 X축에, 그리고 y2, y3, y4 오브젝트를 각각 Y축에 배치하는 
# 라인(line) 그래프를 덧붙이는 기능을 한다. type은 line 형태로 고정을 시켰지만, y2의 경우 청색(blue), 
# y3의 경우 녹색(green), y4의 경우 보라색(purple)로 변화를 주어 시각적으로 구분이 되도록 하였다. 
lines(x, y2, type = "l", col = "blue")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "purple")
# legend()는 범례를 표시하는 함수이다. "top"은 범례의 위치를 맨위에 배치하겠다는 의미이며, 
# ncol은 범례의 세로줄(column) 수를 4로 설정한다는 의미이며, col는 범례의 순서별 색깔, 
# lty = 1은 실선을, bty = "n"은 범례의 테두리를 표시하지 않는다는 의미이다. legend옵션에는 
# 각각의 시나리오를 명시하였다. 
legend("top", ncol = 4, col = c("red","blue","green","purple"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))
	
# R프로그래밍을 이용한 t분포 이해 
# 위에서와 마찬가지로 즉 x라는 벡터를 만드는데, -4부터 4까지 범위의 숫자를 같은 간격을 갖도록 
# 총 500개를 생성하였다. 
x <- seq(-4, 4, length = 500)
# dt() 함수를 통해서 앞에서 소개한 4가지 시나리오에 따른 함수값을 생성하였다. 
# df 는 자유도를 의미하는 옵션이다. 3^2는 3의 제곱이며 다른 경우도 마찬가지이다. 
# 시나리오5의 경우 dnorm()을 이용하였다. dnorm()은 위에서 이미 설명하였다. 
y1 <- dt(x, df = 3)
y2 <- dt(x, df = 3^2)
y3 <- dt(x, df = 3^3)
y4 <- dt(x, df = 3^4)
y5 <- dnorm(x, mean = 0, sd = 1)
# plot()를 이용해서 x 오브젝트를 X축에, 그리고 y5 오브젝트를 Y축에 배치하는 플롯을 그렸다. 
# type과 col, ylim 옵션은 앞에서 이미 설명하였다. 
plot(x, y5, type = "l", col = "black", ylim = c(0,0.5), 
	main = "t distribution versus normal distribution")
# lines()은 앞에서 그린 플롯 위에 x 오브젝트를 X축에, 그리고 y1, y2, y3, y4 오브젝트를 
# 각각 Y축에 배치하는 라인(line) 그래프를 덧붙이는 기능을 한다. 
lines(x, y1, type = "l", col = "red")
lines(x, y2, type = "l", col = "purple")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "blue")
# legend()는 범례를 표시하는 함수이며, 이에 대해서는 앞의 사례에서 설명하였기에 반복하지 않는다. 
legend("top", ncol = 3, col = c("red","purple","green","blue","black"), 
	lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4","scenario 5"))

# qt()는 확률밀도함수에 근거한 확률값(p, probability)과 자유도(df)의 조건을 충족시키는
# t값을 보고하는 함수이다. p = .975인 이유는 alpha = .05로 설정할 때 양방검증을 적용하면 왼편과 
# 오른편 영역의 크기가 .025이기 때문이다. 만약 일방검증을 적용한다면 p = .95를 입력하면 된다. 
qt(p = .975, df = 3)
qt(p = .975, df = 3^2)
qt(p = .975, df = 3^3)
qt(p = .975, df = 3^4)
qnorm(p = .975)
# qnorm()은 qt()와 개념적으로 동등하다. 그러나 정규분포는 자유도가 무한대이기 때문에 df라는 조건을 
# 따로 설정하기 않았다. 
qt(p = .975, df = 473)

# R프로그래밍을 이용한 카이제곱분포 재현
# 아래의 seq() 함수는 일련의 벡터(vector)를 생성하는 함수이다. 즉 x라는 벡터를 만드는데,
# 0부터 50까지 범위의 숫자를 같은 간격을 갖도록 총 500개를 생성한다. 
x <- seq(0, 10, length = 500)
# dchisq()는 카이제곱분포의 확률밀도함수이다. 여기서는 자유도를 1∼4까지 변화시켰다. 
y1 <- dchisq(x, df=1)
y2 <- dchisq(x, df=2)
y3 <- dchisq(x, df=3)
y4 <- dchisq(x, df=4)
# plot()는 앞에서 이미 설명하였기 때문에 반복하지 않았다. 
plot(x, y1, ylim=c(0,1), type = "l", col = "red", main = "Chi-squares distribution")
# lines의 경우도 반복되기 때문에 설명하지 않았다. 
lines(x, y2, type = "l", col = "purple")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "blue")
# legend() 역시 앞의 사례에서 설명하였기에 반복하지 않는다. 
legend("top", ncol = 4, col = c("red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))

# qchisq()는 확률밀도함수에 근거한 확률값(p, probability)과 자유도(df)의 조건을 충족시키는 
# 카이제곱값을 보고하는 함수이다. 
qchisq(p = .95, df = 1)
qchisq(p = .95, df = 2)
qchisq(p = .95, df = 3)
qchisq(p = .95, df = 4)

# R프로그래밍을 이용한 F분포 재현
# 아래의 seq() 함수는 일련의 벡터(vector)를 생성하는 함수이다. 즉 x라는 벡터를 만드는데, 
# 0부터 50까지 범위의 숫자를 같은 간격을 갖도록 총 500개를 생성한다. 
x <- seq(0, 10, length = 500)
# df()는 F분포의 확률밀도함수이다. 위에서 언급한 시나리오 8개에 맞게 두 가지의 자유도를 변화시켰다. 
ya1 <- df(x, df1 = 1, df2 = 50)
ya2 <- df(x, df1 = 2, df2 = 50)
ya3 <- df(x, df1 = 3, df2 = 50)
ya4 <- df(x, df1 = 4, df2 = 50)
yb1 <- df(x, df1 = 4, df2 = 3)
yb2 <- df(x, df1 = 4, df2 = 3^2)
yb3 <- df(x, df1 = 4, df2 = 3^3)
yb4 <- df(x, df1 = 4, df2 = 3^4)
# plot()는 x 오브젝트를 X축에, 그리고 y 오브젝트를 Y축에 배치하는 플롯을 그린다. 
# 우선 시나리오A1부터 시나리오A4를 그렸다. 
plot(x, ya1, ylim=c(0,1.5), type = "l", col = "red", main = "F Distribution, Scenario A")
lines(x, ya2, type = "l", col = "purple")
lines(x, ya3, type = "l", col = "green")
lines(x, ya4, type = "l", col = "blue")
# legend()는 앞의 사례에서 설명하였기에 반복하지 않는다. 
legend("top", ncol = 2, col = c("red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))

# plot() 이전의 부분은 바로 앞의 R프로그래밍 부분을 그대로 실행한다. 여기서는 시나리오B1∼시나리오B4를 그렸다. 
plot(x, yb1, ylim=c(0,1), type = "l", col = "red", main = "F distribution, Scenario B")
lines(x, yb2, type = "l", col = "purple")
lines(x, yb3, type = "l", col = "green")
lines(x, yb4, type = "l", col = "blue")
legend("top", ncol = 2, col = c("red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))

# qf()는 확률밀도함수에 근거한 확률값(p, probability)과 두가지의 자유도(df)의 조건을 충족시키는 
# F값을 보고하는 함수이다. 
qf(p = .95, df = 1, df2 = 50)
qf(p = .95, df = 2, df2 = 50)
qf(p = .95, df = 3, df2 = 50)
qf(p = .95, df = 4, df2 = 50)
qf(p = .95, df = 4, df2 = 3)
qf(p = .95, df = 4, df2 = 3^2)
qf(p = .95, df = 4, df2 = 3^3)
qf(p = .95, df = 4, df2 = 3^4)

# R프로그래밍을 이용한 F분포 재현
# 아래의 seq() 함수를 이용해서 각 시나리오별로 변동하는 n, k, p를 설정한 것이다.  
n <- seq(1, 20, length = 20)
k <- seq(1, 20, length = 20)
p <- seq(0.1, 0.9, length = 90)
# dbinom()는  이항분포의 확률밀도함수이다. 위에서 언급한 시나리오에 맞게 두 가지의 모수를 고정한 상태에서 
# 각각 n, k, p를 변화시킨 것이다. 
ya <- dbinom(1, size = n, prob = 0.50)
yb <- dbinom(k, size = 20, prob = 0.50)
yc <- dbinom(10, size = 20, prob = p)
# plot()는 x 오브젝트를 X축에, 그리고 y 오브젝트를 Y축에 배치하는 플롯을 그린다. 
# par()는 플롯을 그리는 패널(panel)을 구획시키는(partition) 역할을 한다. 
# 또한 mfrow의 옵션은 가로줄이 1이고 세로줄이 3개이도록 패널을 구획한다는 의미이다. 
# 이렇게 미리 설정된 패널에 위에서 언급한 세 시나리오를 각각 도시하였다. 
par(mfrow=c(1,3))
plot(n, ya, type = "l", col = "black", main = "scenario A")
plot(k, yb, type = "l", col = "black", main = "scenario B")
plot(p, yc, type = "l", col = "black", main = "scenario C")

# R프로그래밍을 이용한 F분포 재현
# 아래의 seq() 함수를 이용 k라는 벡터를 만드는데, 1부터 20까지 범위의 실수를 20개 생성하였다.  
k <- seq(1, 20, length = 20)
# 위의 시나리오에 맞도록 lambda의 값 다섯 개를 설정하였다. 
# dpois()는 포아송분포의 확률밀도함수이다. 위에서 언급한 시나리오 5개에 맞게 lambda의 값을 변화시켰다. 
y1 <- dpois(k, lambda = 1)
y2 <- dpois(k, lambda = 2)
y3 <- dpois(k, lambda = 3)
y4 <- dpois(k, lambda = 4)
y5 <- dpois(k, lambda = 5)
# plot()을 이용 5개의 시나리오에 해당되는 포아송분포를 겹쳐서 그렸다. type의 옵션을 b로 바꾸었는데, 
# 말로된 설명보다는 그림을 확인하면 이해가 더 쉬울 것이다. 
plot(k, y1, type = "b", col = "orange", main = "Poisson distribution, example")
lines(k, y2, type = "b", col = "red")
lines(k, y3, type = "b", col = "purple")
lines(k, y4, type = "b", col = "green")
lines(k, y5, type = "b", col = "blue")
legend("top", ncol = 2, col = c("orange","red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("lambda = 1","lambda = 2","lambda = 3","lambda = 4","lambda = 5"))
  
# qpois()는 확률밀도함수에 근거하여 lambda의 조건과 확률값(p, probability)에 따라 
# 몇 번의 사건이 필요한지 보고하는 함수이다. 
qpois(p = .95, lambda = 1)
qpois(p = .95, lambda = 2)
qpois(p = .95, lambda = 3)
qpois(p = .95, lambda = 4)
qpois(p = .95, lambda = 5)

  	