set.seed(123)
populacao <- runif(1000)
hist(populacao)
media_pop <- mean(populacao)
vari_pop <- var(populacao)
medias_amostras <- c()

intervalos_norm <- matrix(0,nrow=1000,ncol=2)
intervalos_t <- matrix(0,nrow=1000,ncol=2)
tamanho <- 10
for(i in 1:1000){
  amostra <- sample(x = populacao, size = tamanho)
  media <- mean(amostra)
  variancia <- var(amostra)
  medias_amostras[i] <- media
  intervalos_norm[i,] <- c(media-qnorm(.975)*sqrt(vari_pop/tamanho),media+qnorm(.975)*sqrt(vari_pop/tamanho))
  intervalos_t[i,] <- c(media-qt(.975,tamanho-1)*sqrt(variancia/tamanho),media+qt(.975,tamanho-1)*sqrt(variancia/tamanho))
}
mean(intervalos_norm[,1]<media_pop & intervalos_norm[,2]>media_pop)
mean(intervalos_t[,1]<media_pop & intervalos_t[,2]>media_pop)


require(animation)
conf.int()