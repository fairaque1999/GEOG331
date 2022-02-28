# subset the iris dataset
flower <- iris[iris$Species == "virginica",]

# fit a linear model
fit <- lm(flower$Petal.Length~flower$Sepal.Length)

# view results 
summary(fit)

# Plotting the relationship

plot(flower$Sepal.Length, flower$Petal.Length,
     main = "Iris virginica",
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = "purple")

plot(flower$Sepal.Length, summary(fit)$residuals,
     main = "Iris virginica",
     xlab = "Sepal Length",
     ylab = "Petal Length")
abline(h = 0 )

# shapiro wilks test

shapiro.test(summary(fit)$residuals)

# qqnorm and qqline

qqnorm(summary(fit)$residuals, pch = 16)

qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm, 
       probs = c(0.25, 0.75), qtype = 7, pch = 16)
