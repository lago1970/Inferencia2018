bsb <- read.csv("https://raw.githubusercontent.com/cursoRunb/Inferencia2018/master/Aula4/climaBsb.csv")
names(bsb)

rquadrado <- c()
for( i in 8:21){
  mod <- lm(bsb$temp_inst ~ bsb[,i])
  rquadrado <- c(rquadrado,summary(mod)$r.squared)
}
round(rquadrado,3)
names(rquadrado) <- names(bsb)[8:21]
round(rquadrado,4)
plot(bsb$umid_inst,bsb$temp_inst)
mod1 <- lm(bsb$temp_inst ~ bsb$umid_inst)
abline(mod1$coefficients)
summary(mod1)

mod1.2 <- lm(temp_inst ~radiacao,bsb)
summary(mod1.2)
mod2 <- lm(temp_inst ~  radiacao + umid_inst,bsb)
summary(mod2)
shapiro.test(mod2$residuals)
bptest(mod2)
names(mod2)
plot(bsb$umid_inst,bsb$radiacao)
plot(mod2)

mod3 <- lm(temp_inst ~ temp_max + temp_min,bsb)
summary(mod3)
par(mfrow=c(2,2))


erros <- mod1$residuals
plot(erros)
hist(erros,breaks = 10)
qqnorm(erros)
qqline(erros)
plot(bsb$umid_inst,erros)
shapiro.test(erros)
ks.test(erros,"pnorm")

install.packages("lmtest")
require(lmtest)
bptest(mod1)
plot(mod1)

plot(bsb[,5:10])

?glm

