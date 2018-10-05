iris
modelo_anova <- aov(iris$Petal.Length~iris$Species)
summary(modelo_anova)
TukeyHSD(modelo_anova)


modelo2 <- aov(mpg ~ as.factor(cyl) + as.factor(am),mtcars)
summary(modelo2)
TukeyHSD(modelo2,which = "as.factor(cyl)")
modelo3 <- aov(mpg ~ .,mtcars)
summary(modelo3)


plot(mtcars$wt,mtcars$mpg)
regressao <- lm(mpg~wt,mtcars)
summary(regressao)
abline(regressao$coefficients)
