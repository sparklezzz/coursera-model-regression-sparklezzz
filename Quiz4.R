#q1
library(MASS)
fit <- glm(use ~ wind, family = binomial, data = shuttle)
exp(fit$coef)

#q2
fit2 <- glm(use ~ wind + factor(magn), family = binomial, data = shuttle)
exp(fit2$coef)

#q3
y1 = c()
y1[shuttle$use == 'auto'] = 0
y1[shuttle$use != 'auto'] = 1
fit3 <- glm(y1 ~ shuttle$wind, family = binomial)
summary(fit3)

y2 = 1 - y1
fit4 <- glm(y2 ~ shuttle$wind, family = binomial)
summary(fit4)

#q4
fit5 <- glm(count ~ factor(spray) - 1, family = poisson, data = InsectSprays)
coef <- exp((fit5$coef))
coef[1] / coef[2]

#q5
t <- rep(0, length(InsectSprays$count))
fit6 <- glm(count ~ spray + offset(t), family = poisson, data = InsectSprays)
fit6$coef
t2 <- t + log(10)
fit7 <- glm(count ~ spray + offset(t2), family = poisson, data = InsectSprays)
fit7$coef

#q6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
x1 <- c(1:5)
y1 <- y[7:11]
summary(lm(y1~x1))

