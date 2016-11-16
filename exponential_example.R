# Experements with exponential regression.
library(ggplot2)

N <- 100000

df <- data.frame(
 x_1 = runif(N), x_2 = runif(N)
)

df$lp <- 1 + 0.5*df$x_1 - df$x_2
df$rate <- 1/exp(df$lp)
df$y <- unlist(lapply(df$rate, function(x) rexp(1, rate=x)))

ggplot() + geom_density(aes(x=df$y))

M <- glm(y ~ x_1 + x_2, data=df, family=Gamma(link='log'))
summary(M, dispersion=1)
