#q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
sum(x * w) / sum(w)

#q2
#note the condition: fit the regrssion through the origin
#we want regrssion through the origin, i.e. beta0 = 0
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
beta1 = sum(x*y) / sum(x^2)
# or lm(y~x-1)

#q3
data(mtcars)
lm(mtcars$mpg ~ mtcars$wt)[[1]][2]

#q4
stdev_y = 1
stdev_x = 0.5 * stdev_y
cor_y_x = 0.5
cor_y_x * stdev_y / stdev_x

#q5
cor_y_x = 0.4
slope = cor_y_x 
1.5 * slope

#q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x - mean(x)) / sd(x)

#q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)[1]

#q8
#beta0 = mean_y - beta1 * mean_x = 0 - beta1 * 0 = 0

#q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

#q10
#beta1 = cor(y,x) * sd(y) / sd(x)
#gamma1 = cor(y,x) * sd(x) / sd(y)
#beta1 / gamma1 = sd(y)^2 / sd(x)^2 = var(y) / var(x)


