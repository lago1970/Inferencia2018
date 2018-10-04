tipoa <- iris[iris$Species=="setosa",]
tipoa
mediaA <- mean(tipoa$Petal.Length)
variA <- var(tipoa$Petal.Length)
mediaA


tipob <- iris[iris$Species=="versicolor",]
tipob
mediaB <- mean(tipob$Petal.Length)
mediaB
variB <- var(tipob$Petal.Length)

tobs <- (mediaA - mediaB)/sqrt(variA/50+variB/50)
tobs

pvalor <- 2* pt(tobs,98)
pvalor
t.test(tipoa$Petal.Length, tipob$Petal.Length, paired = FALSE, var.equal = FALSE)
boxplot(iris$Petal.Length~iris$Species)




#########
tipoa <- iris[iris$Species=="versicolor",]
tipoa
mediaA <- mean(tipoa$Petal.Length)
variA <- var(tipoa$Petal.Length)
mediaA


tipob <- iris[iris$Species=="virginica",]
tipob
mediaB <- mean(tipob$Petal.Length)
mediaB
variB <- var(tipob$Petal.Length)

tobs <- (mediaA - mediaB)/sqrt(variA/50+variB/50)
tobs

pvalor <- 2* pt(tobs,98)
pvalor
t.test(tipoa$Petal.Length, tipob$Petal.Length, paired = FALSE, var.equal = FALSE)


fobs <- variB/variA
fobs
2 * (1 -pf(fobs,df1 = 49,df2 = 49))

t.test(tipoa$Petal.Length, tipob$Petal.Length, paired = FALSE, var.equal = TRUE)
var.test(tipoa$Petal.Length, tipob$Petal.Length)



#####
tipoa <- iris[iris$Species=="versicolor",]
tipoa
mediaA <- mean(tipoa$Petal.Length)
variA <- var(tipoa$Petal.Length)
mediaA


tipob <- iris[iris$Species=="virginica",]
tipob
mediaB <- mean(tipob$Petal.Length)
mediaB
variB <- var(tipob$Petal.Length)


variAB <- (49*variA + 49*variB)/98
tobs <- (mediaA - mediaB)/sqrt(variAB/50+variAB/50)
tobs
t.test(tipoa$Petal.Length, tipob$Petal.Length, paired = FALSE, var.equal = TRUE)
