# 4��. ���м�
# 1��. R���α׷����� ���� Ȯ������ �н�
# 1. ��ȸ���п��� ���� �����ϴ� Ȯ������

# R���α׷����� �̿��� ���Ժ��� ����
# �Ʒ��� seq() �Լ��� �Ϸ��� ����(vector)�� �����ϴ� �Լ��̴�. �� x��� ���͸� ����µ�, 
# -4���� 4���� ������ ���ڸ� ���� ������ ������ �� 500���� �����Ѵ�. 
x <- seq(-4, 4, length = 500)
# �Ʒ��� �տ��� ������ ���Ժ����� Ȯ���е��Լ��� ���α׷����� ���̴�. sqrt()�� ��Ʈ(square root)��, 
# pi�� ���̰��̸�, exp()�� �����Լ��̴�. 
y <- (1/sqrt(2*pi))*exp(-(x^2/2))
# plot()�� x ������Ʈ�� X�࿡, �׸��� y ������Ʈ�� Y�࿡ ��ġ�ϴ� �÷��� �׸���. 
# �ſ� ���� �����ϴ� �Լ��̹Ƿ� �ݵ�� �����Ѵ�. type �ɼ��� l�� �׷����� �׸� �� line, 
# �� ������ ǥ���ϴ� �÷��̶�� �ǹ��̰�, col�� color, �� ������ ���ڷ� ���� ���� ���� ������ ǥ���Ͽ���. 
# main �ɼ����� �׷����� �̸��� ���� �� �ִ�. 
plot(x, y, type = "l", col = "red", main = "Normal distribution, example")

# R���α׷����� �̿��� ���Ժ��� ���� 
# �������� ���������� �� x��� ���͸� ����µ�, -4���� 4���� ������ ���ڸ� ���� ������ ������ 
# �� 500���� �����Ͽ���. 
x <- seq(-4, 4, length = 500)
# dnorm() �Լ��� ���ؼ� �տ��� �Ұ��� 4���� �ó������� ���� �Լ����� �����Ͽ���. mean�� ������ ������ �����, 
# �׸��� sd�� ������ ������ ǥ�������� �����ϴ� �ɼ��̴�. 
y1 <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 0, sd = 2)
y3 <- dnorm(x, mean = 1, sd = 1)
y4 <- dnorm(x, mean = 1, sd = 2)
# plot()�� �̿��ؼ� x ������Ʈ�� X�࿡, �׸��� y1 ������Ʈ�� Y�࿡ ��ġ�ϴ� �÷��� �׷ȴ�. 
# ylim �ɼ��� �̿��Ͽ� y���� ������ ������ �� �ִ�. ���ʸ� �ֱ� ���� ������ �����Ͽ���. 
# type�� col, main �ɼǵ��� �տ��� �̹� �����Ͽ���. 
plot(x, y1, type = "l", col = "red", ylim = c(0,0.6), main = "t distribution")
# lines()�� �տ��� �׸� �÷� ���� x ������Ʈ�� X�࿡, �׸��� y2, y3, y4 ������Ʈ�� ���� Y�࿡ ��ġ�ϴ� 
# ����(line) �׷����� �����̴� ����� �Ѵ�. type�� line ���·� ������ ��������, y2�� ��� û��(blue), 
# y3�� ��� ���(green), y4�� ��� �����(purple)�� ��ȭ�� �־� �ð������� ������ �ǵ��� �Ͽ���. 
lines(x, y2, type = "l", col = "blue")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "purple")
# legend()�� ���ʸ� ǥ���ϴ� �Լ��̴�. "top"�� ������ ��ġ�� ������ ��ġ�ϰڴٴ� �ǹ��̸�, 
# ncol�� ������ ������(column) ���� 4�� �����Ѵٴ� �ǹ��̸�, col�� ������ ������ ����, 
# lty = 1�� �Ǽ���, bty = "n"�� ������ �׵θ��� ǥ������ �ʴ´ٴ� �ǹ��̴�. legend�ɼǿ��� 
# ������ �ó������� ����Ͽ���. 
legend("top", ncol = 4, col = c("red","blue","green","purple"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))
	
# R���α׷����� �̿��� t���� ���� 
# �������� ���������� �� x��� ���͸� ����µ�, -4���� 4���� ������ ���ڸ� ���� ������ ������ 
# �� 500���� �����Ͽ���. 
x <- seq(-4, 4, length = 500)
# dt() �Լ��� ���ؼ� �տ��� �Ұ��� 4���� �ó������� ���� �Լ����� �����Ͽ���. 
# df �� �������� �ǹ��ϴ� �ɼ��̴�. 3^2�� 3�� �����̸� �ٸ� ��쵵 ���������̴�. 
# �ó�����5�� ��� dnorm()�� �̿��Ͽ���. dnorm()�� ������ �̹� �����Ͽ���. 
y1 <- dt(x, df = 3)
y2 <- dt(x, df = 3^2)
y3 <- dt(x, df = 3^3)
y4 <- dt(x, df = 3^4)
y5 <- dnorm(x, mean = 0, sd = 1)
# plot()�� �̿��ؼ� x ������Ʈ�� X�࿡, �׸��� y5 ������Ʈ�� Y�࿡ ��ġ�ϴ� �÷��� �׷ȴ�. 
# type�� col, ylim �ɼ��� �տ��� �̹� �����Ͽ���. 
plot(x, y5, type = "l", col = "black", ylim = c(0,0.5), 
	main = "t distribution versus normal distribution")
# lines()�� �տ��� �׸� �÷� ���� x ������Ʈ�� X�࿡, �׸��� y1, y2, y3, y4 ������Ʈ�� 
# ���� Y�࿡ ��ġ�ϴ� ����(line) �׷����� �����̴� ����� �Ѵ�. 
lines(x, y1, type = "l", col = "red")
lines(x, y2, type = "l", col = "purple")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "blue")
# legend()�� ���ʸ� ǥ���ϴ� �Լ��̸�, �̿� ���ؼ��� ���� ��ʿ��� �����Ͽ��⿡ �ݺ����� �ʴ´�. 
legend("top", ncol = 3, col = c("red","purple","green","blue","black"), 
	lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4","scenario 5"))

# qt()�� Ȯ���е��Լ��� �ٰ��� Ȯ����(p, probability)�� ������(df)�� ������ ������Ű��
# t���� �����ϴ� �Լ��̴�. p = .975�� ������ alpha = .05�� ������ �� �������� �����ϸ� ����� 
# ������ ������ ũ�Ⱑ .025�̱� �����̴�. ���� �Ϲ������ �����Ѵٸ� p = .95�� �Է��ϸ� �ȴ�. 
qt(p = .975, df = 3)
qt(p = .975, df = 3^2)
qt(p = .975, df = 3^3)
qt(p = .975, df = 3^4)
qnorm(p = .975)
# qnorm()�� qt()�� ���������� �����ϴ�. �׷��� ���Ժ����� �������� ���Ѵ��̱� ������ df��� ������ 
# ���� �����ϱ� �ʾҴ�. 
qt(p = .975, df = 473)

# R���α׷����� �̿��� ī���������� ����
# �Ʒ��� seq() �Լ��� �Ϸ��� ����(vector)�� �����ϴ� �Լ��̴�. �� x��� ���͸� ����µ�,
# 0���� 50���� ������ ���ڸ� ���� ������ ������ �� 500���� �����Ѵ�. 
x <- seq(0, 10, length = 500)
# dchisq()�� ī������������ Ȯ���е��Լ��̴�. ���⼭�� �������� 1��4���� ��ȭ���״�. 
y1 <- dchisq(x, df=1)
y2 <- dchisq(x, df=2)
y3 <- dchisq(x, df=3)
y4 <- dchisq(x, df=4)
# plot()�� �տ��� �̹� �����Ͽ��� ������ �ݺ����� �ʾҴ�. 
plot(x, y1, ylim=c(0,1), type = "l", col = "red", main = "Chi-squares distribution")
# lines�� ��쵵 �ݺ��Ǳ� ������ �������� �ʾҴ�. 
lines(x, y2, type = "l", col = "purple")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "blue")
# legend() ���� ���� ��ʿ��� �����Ͽ��⿡ �ݺ����� �ʴ´�. 
legend("top", ncol = 4, col = c("red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))

# qchisq()�� Ȯ���е��Լ��� �ٰ��� Ȯ����(p, probability)�� ������(df)�� ������ ������Ű�� 
# ī���������� �����ϴ� �Լ��̴�. 
qchisq(p = .95, df = 1)
qchisq(p = .95, df = 2)
qchisq(p = .95, df = 3)
qchisq(p = .95, df = 4)

# R���α׷����� �̿��� F���� ����
# �Ʒ��� seq() �Լ��� �Ϸ��� ����(vector)�� �����ϴ� �Լ��̴�. �� x��� ���͸� ����µ�, 
# 0���� 50���� ������ ���ڸ� ���� ������ ������ �� 500���� �����Ѵ�. 
x <- seq(0, 10, length = 500)
# df()�� F������ Ȯ���е��Լ��̴�. ������ ����� �ó����� 8���� �°� �� ������ �������� ��ȭ���״�. 
ya1 <- df(x, df1 = 1, df2 = 50)
ya2 <- df(x, df1 = 2, df2 = 50)
ya3 <- df(x, df1 = 3, df2 = 50)
ya4 <- df(x, df1 = 4, df2 = 50)
yb1 <- df(x, df1 = 4, df2 = 3)
yb2 <- df(x, df1 = 4, df2 = 3^2)
yb3 <- df(x, df1 = 4, df2 = 3^3)
yb4 <- df(x, df1 = 4, df2 = 3^4)
# plot()�� x ������Ʈ�� X�࿡, �׸��� y ������Ʈ�� Y�࿡ ��ġ�ϴ� �÷��� �׸���. 
# �켱 �ó�����A1���� �ó�����A4�� �׷ȴ�. 
plot(x, ya1, ylim=c(0,1.5), type = "l", col = "red", main = "F Distribution, Scenario A")
lines(x, ya2, type = "l", col = "purple")
lines(x, ya3, type = "l", col = "green")
lines(x, ya4, type = "l", col = "blue")
# legend()�� ���� ��ʿ��� �����Ͽ��⿡ �ݺ����� �ʴ´�. 
legend("top", ncol = 2, col = c("red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))

# plot() ������ �κ��� �ٷ� ���� R���α׷��� �κ��� �״�� �����Ѵ�. ���⼭�� �ó�����B1���ó�����B4�� �׷ȴ�. 
plot(x, yb1, ylim=c(0,1), type = "l", col = "red", main = "F distribution, Scenario B")
lines(x, yb2, type = "l", col = "purple")
lines(x, yb3, type = "l", col = "green")
lines(x, yb4, type = "l", col = "blue")
legend("top", ncol = 2, col = c("red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("scenario 1","scenario 2","scenario 3","scenario 4"))

# qf()�� Ȯ���е��Լ��� �ٰ��� Ȯ����(p, probability)�� �ΰ����� ������(df)�� ������ ������Ű�� 
# F���� �����ϴ� �Լ��̴�. 
qf(p = .95, df = 1, df2 = 50)
qf(p = .95, df = 2, df2 = 50)
qf(p = .95, df = 3, df2 = 50)
qf(p = .95, df = 4, df2 = 50)
qf(p = .95, df = 4, df2 = 3)
qf(p = .95, df = 4, df2 = 3^2)
qf(p = .95, df = 4, df2 = 3^3)
qf(p = .95, df = 4, df2 = 3^4)

# R���α׷����� �̿��� F���� ����
# �Ʒ��� seq() �Լ��� �̿��ؼ� �� �ó��������� �����ϴ� n, k, p�� ������ ���̴�.  
n <- seq(1, 20, length = 20)
k <- seq(1, 20, length = 20)
p <- seq(0.1, 0.9, length = 90)
# dbinom()��  ���׺����� Ȯ���е��Լ��̴�. ������ ����� �ó������� �°� �� ������ ����� ������ ���¿��� 
# ���� n, k, p�� ��ȭ��Ų ���̴�. 
ya <- dbinom(1, size = n, prob = 0.50)
yb <- dbinom(k, size = 20, prob = 0.50)
yc <- dbinom(10, size = 20, prob = p)
# plot()�� x ������Ʈ�� X�࿡, �׸��� y ������Ʈ�� Y�࿡ ��ġ�ϴ� �÷��� �׸���. 
# par()�� �÷��� �׸��� �г�(panel)�� ��ȹ��Ű��(partition) ������ �Ѵ�. 
# ���� mfrow�� �ɼ��� �������� 1�̰� �������� 3���̵��� �г��� ��ȹ�Ѵٴ� �ǹ��̴�. 
# �̷��� �̸� ������ �гο� ������ ����� �� �ó������� ���� �����Ͽ���. 
par(mfrow=c(1,3))
plot(n, ya, type = "l", col = "black", main = "scenario A")
plot(k, yb, type = "l", col = "black", main = "scenario B")
plot(p, yc, type = "l", col = "black", main = "scenario C")

# R���α׷����� �̿��� F���� ����
# �Ʒ��� seq() �Լ��� �̿� k��� ���͸� ����µ�, 1���� 20���� ������ �Ǽ��� 20�� �����Ͽ���.  
k <- seq(1, 20, length = 20)
# ���� �ó������� �µ��� lambda�� �� �ټ� ���� �����Ͽ���. 
# dpois()�� ���Ƽۺ����� Ȯ���е��Լ��̴�. ������ ����� �ó����� 5���� �°� lambda�� ���� ��ȭ���״�. 
y1 <- dpois(k, lambda = 1)
y2 <- dpois(k, lambda = 2)
y3 <- dpois(k, lambda = 3)
y4 <- dpois(k, lambda = 4)
y5 <- dpois(k, lambda = 5)
# plot()�� �̿� 5���� �ó������� �ش�Ǵ� ���Ƽۺ����� ���ļ� �׷ȴ�. type�� �ɼ��� b�� �ٲپ��µ�, 
# ���ε� �����ٴ� �׸��� Ȯ���ϸ� ���ذ� �� ���� ���̴�. 
plot(k, y1, type = "b", col = "orange", main = "Poisson distribution, example")
lines(k, y2, type = "b", col = "red")
lines(k, y3, type = "b", col = "purple")
lines(k, y4, type = "b", col = "green")
lines(k, y5, type = "b", col = "blue")
legend("top", ncol = 2, col = c("orange","red","purple","green","blue"), lty = 1, bty = "n",
	legend = c("lambda = 1","lambda = 2","lambda = 3","lambda = 4","lambda = 5"))
  
# qpois()�� Ȯ���е��Լ��� �ٰ��Ͽ� lambda�� ���ǰ� Ȯ����(p, probability)�� ���� 
# �� ���� ����� �ʿ����� �����ϴ� �Լ��̴�. 
qpois(p = .95, lambda = 1)
qpois(p = .95, lambda = 2)
qpois(p = .95, lambda = 3)
qpois(p = .95, lambda = 4)
qpois(p = .95, lambda = 5)

  	