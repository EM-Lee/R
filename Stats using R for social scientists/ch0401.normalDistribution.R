### normal distribution

x <- seq(-4, 4, length = 500)
y <- (1 / sqrt(2 * pi)) * exp(-(x^2 / 2))#https://datascienceschool.net/view-notebook/56e7a25aad2a4539b3c31eb3eb787a54/
plot(x, y, type = "l", col = "red", main = "Normal Distribution, Example")

# different scenarios using dnorm() function
x <- seq(-4, 4, length.out = 500)
y1 <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 0, sd = 2)
y3 <- dnorm(x, mean = 1, sd = 1)
y4 <- dnorm(x, mean = 1, sd = 2)
plot(x, y1, type = "l", col = "red", ylim = c(0, .6), main = "t distribution")
lines(x, y2, type = "l", col = "blue")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "purple")
legend("top", ncol = 4, col = c("red", "blue", "green", "purple"), lty = 1, bty = "n",
       legend = c("scenario 1", "scenario 2", "scenario 3", "scenario 4"))

# different scenarios using dt() with df (degree of freedom)
x <- seq(-4, 4, length = 500)
y1 <- dt(x, df = 3)
y2 <- dt(x, df = 3^2)
y3 <- dt(x, df = 3^3)
y4 <- dt(x, df = 3^4)
y5 <- dnorm(x, mean = 0, sd = 1)
plot(x, y5, type = "l", col = "black", ylim = c(0, .5),
     main = "t distribution vs. normal distribution")
lines(x, y1, type = "l", col = "red")
lines(x, y2, type = "l", col = "purple")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "blue")
legend("top", ncol = 3, col = c("red", "purple", "green", "blue", "black"),
       lty = 1, bty = "n",
       legend = c("scenario 1", "scenario 2", "scenario 3", "scenario 4", "scenario 5"))
