#q1
data(mtcars)
z <- mtcars$mpg
x <- mtcars$cyl
y <- mtcars$wt
fit <- lm(z~factor(x)+y)
summary(fit)

#q2
z <- mtcars$mpg
x <- mtcars$cyl
y <- mtcars$wt
fit <- lm(z~factor(x)+y)
fit2 <- lm(z~factor(x))
summary(fit)
summary(fit2)

#q3 interaction mean factor(cyl) + wt + factor(cyl) : wt
z <- mtcars$mpg
x <- mtcars$cyl
y <- mtcars$wt
fit <- lm(z~factor(x)+y)
fit2 <- lm(z~factor(x)*y)
anova(fit, fit2)


#q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
hatvalues(lm(x~y))
#lm.influence(lm(y~x))$hat

#q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
hatvalues(lm(x~y))
dfbeta(lm(y~x))




