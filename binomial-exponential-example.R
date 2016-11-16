# Experements with exponential + binomial regression.
# The model in this example will be a composite: a binomial (logistic) regression
# will determine whether the response in positive or negative.  Given this information,
# the response will be exponentially distributed with different rates on the
# left and right side of zero.
library(ggplot2)
# library(GGally)

N <- 250000

df <- data.frame(
  x_1 = runif(N), x_2 = runif(N), x_3 = runif(N)
)

# The binomial data, controlls whether the sample is to the left or right of zero.
df$lp_binomial <- 1 - df$x_1 + 2*df$x_3
df$p <- 1 / (1 + exp(-df$lp_binomial))
df$binomial <- unlist(lapply(df$p, function(x) rbinom(1, 1, x)))

# The exponential data, conditional on whether the sample is to the left or right of zero.
df$lp_right <- 1 + 0.5*df$x_1 - df$x_2
df$lp_left <- df$x_1 - df$x_2 + 0.25 * df$x_3
df$lp <- df$binomial * df$lp_left + (1 - df$binomial) * df$lp_right
df$rate <- 1/exp(df$lp)
df$y <- (2*df$binomial - 1) * unlist(lapply(df$rate, function(x) rexp(1, rate=x)))

# The unconditional distribution of the response.
ggplot() + geom_density(aes(x=df$y))

# Create an indicator for whether the data is to the right or left of zero.
df$y_right_left <- as.numeric(df$y >= 0)

# Relationship between the predictors and the right-left indicator
# ggpairs(df[, c("y_right_left", "x_1", "x_2", "x_3")])

# Predict whether an observation is on the right or left.
M_logistic <- glm(y_right_left ~ x_1 + x_2 + x_3, data=df, family=binomial())
summary(M_logistic)

# Conditional on the left, predict the exponential varaible.
df_left <- df[df$y < 0, ]
M_left <- glm(-y ~ x_1 + x_2 + x_3, data=df_left, family=Gamma(link='log'))
summary(M_left, dispersion=1)

# Conditional on the right, predict the exponential variable
df_right <- df[df$y >= 0, ]
M_right <- glm(y ~ x_1 + x_2 + x_3, data=df_right, family=Gamma(link='log'))
summary(M_right, dispersion=1)
