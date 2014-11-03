#q1: note it wants P-value, not t-value
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y~x))

#q2
summary(lm(y~x))$sigma
# or sqrt(sum(resid(lm(y~x))^2) / (length(y) - 2))

#q3: note, at average weight, predict the line
data(mtcars)
fit <- lm(mtcars$mpg ~ mtcars$wt)
sumCoef <- summary(fit)$coefficients
n <- length(mtcars$mpg)
ssx <- sum((mtcars$wt - mean(mtcars$wt))^2)
sigma <- summary(fit)$sigma
beta0 <- sumCoef[1,1]
beta1 <- sumCoef[2,1]
#wrong: sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
interval = sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]
xVal = mean(mtcars$wt)
yVal = beta0 + beta1 * xVal
yVal - qt(.975, df = fit$df) * sumCoef[2,2]
# this is equal
se1 <- sigma * sqrt(1 / n + (xVal - mean(mtcars$wt))^2/ssx)
yVal - qt(.975, df = fit$df) * se1

#q4
#mpg   Miles/(US) gallon
#wt   Weight (lb/1000)

#q5 unchecked
x <- mtcars$wt
y <- mtcars$mpg
ssx <- sum((mtcars$wt - mean(mtcars$wt))^2)
fit = lm(mtcars$mpg ~ mtcars$wt)
sumCoef <- summary(fit)$coefficients
sigma <- summary(fit)$sigma
beta0 <- sumCoef[1,1]
beta1 <- sumCoef[2,1]
n <- length(mtcars$mpg)
xVal <- 3000 / 1000
yVal <- beta0 + beta1 * xVal
se2 <- sigma * sqrt(1 + 1 / n + (xVal - mean(mtcars$wt))^2/ssx)
yVal + 2 * se2

#q6
2 * (sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2])


#q8
#Z = X + c, X = Z - c
#Y = beta0 + beta1 * (Z-c) + epsilon

#q9
x <- mtcars$wt
y <- mtcars$mpg
denominator = sum((y - mean(y))^2)
numerator = sum(resid(lm(y~x))^2)
numerator / denominator

