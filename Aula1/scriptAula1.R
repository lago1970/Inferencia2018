2+2
#calcular probabilidades das alturas
pnorm(-1)
pnorm(160,mean = 170, sd = 10)
1 - pnorm(160,mean = 170, sd = 10)
pnorm(160,mean = 170, sd = 10,lower.tail = FALSE)
pnorm(175,mean = 170, sd = 10) - pnorm(165,mean = 170, sd = 10)
#para encontrar o primeiro quartil
qnorm(0.25,mean = 170, sd = 10)

##gerando numeros aleatorios
numeros <- rnorm(n = 1000,mean = 170,sd = 10)
numeros
hist(numeros,breaks = c(135,140,145,150,155,160,165,170,175,180,185,190,195,200))
summary(numeros)
classes <- seq(from = 135,to = 200,by = 5)
hist(numeros,breaks = classes)
set.seed(12345)
rnorm(6,mean = 170, sd = 10)


###Teorema do limite central

populacao <- runif(10000)
hist(populacao)
mediapop <- mean(populacao)
mediapop
amostra1 <- sample(x = populacao,size = 10)
mean(amostra1)
amostra2 <- sample(x = populacao,size = 10)
mean(amostra2)
medias_amostrais <- c()
for(i in 1:1000){
  medias_amostrais[i] <- mean(sample(populacao,30))
}

mean(medias_amostrais)
hist(medias_amostrais)
shapiro.test(medias_amostrais)

###exponencial

populacao <- rexp(10000,rate = 1/10)
hist(populacao)
mediapop <- mean(populacao)
mediapop

medias_amostrais <- c()
for(i in 1:1000){
  medias_amostrais[i] <- mean(sample(populacao,30))
}

mean(medias_amostrais)
hist(medias_amostrais)
#?shapiro.test
shapiro.test(medias_amostrais)


### Intervalo de confiança e cobertura nominal


mediapop <- mean(populacao)
variancia <- var(populacao)
intervalos <- matrix(0,nrow=1000,ncol=2)
for(i in 1:1000){
  intervalos[i,] <- c(medias_amostrais[i]-1.96*sqrt(variancia/30),medias_amostrais[i]+1.96*sqrt(variancia/30))
}
#intervalos
resultado <- c()
for(i in 1:1000){
  resultado[i] <- intervalos[i,1] < mediapop & intervalos[i,2] > mediapop
}
mean(resultado)

