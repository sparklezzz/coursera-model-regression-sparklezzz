
plot(mtcars)
str(mtcars)

#factor: cyl, vs, am, gear

fit <- lm(mpg ~ factor(cyl) + disp + hp + drat + wt + qsec + factor(vs) + factor(am) + factor(gear) + carb, data = mtcars)
step(fit)

fit2 <- lm(formula = mpg ~ wt + qsec + factor(am), data = mtcars)
summary(fit2)

layout(matrix(c(1,2,3,4),2,2))
plot(fit2)