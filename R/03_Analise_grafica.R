#Script de graficos dados iris
head(iris)
?iris
summary(iris)
#conhecendo as funcoes aggregate e tapply
table(iris$Species)
#media do comp da sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
#mesma tarefa executada por outra funcao
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
#mesma tarefa outra funcao
aggregate(Sepal.Length ~ Species, data = iris, mean)
#para as outras variaveis
aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)
#calculo desvio padrao
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)
#solução de como calular a média por espécie de todas as variáveis
# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
medias
for (i in 1:4) {
medias[i,] <- tapply(iris[,i], iris$Species, mean)
}

#Estatisticas descritivas
#media de tendencia central
vars <- iris[, -5]
apply(vars, 2, mean)
#mediana
apply(vars, 2, median)
#moda
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]
#medidas de dispersao
#variancia
apply(vars, 2, var)
#desvio padrao
sd01 <- apply(vars, 2, sd)
#coeficiente de variacao
#criando funcao de variacao usandoa funcao function
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)
# sumario de 5 numeros quantis percentis
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))
# intervalo range diferença entre o maior e o menor valor de uma variável
# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)
#O IIQ é a diferença entre o quartil superior 75 e quartil inferior 25
apply(vars, 2, IQR)
#correlacao
cor(vars)

#metodos graficos
#graficos de barras
barplot(table(iris$Species))
#histograma
par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1, 1))
#comp da sepala das especies de Iris
#para ver efeito do num de intervalos no histograma com uso arg breaks
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
par(mfrow=c(1, 1))
#curva de densidade
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)
par(mfrow=c(1, 1))
par(mfrow=c(1, 2))
#plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue")
par(mfrow=c(1, 1))

#Box plot
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)
#valores por especie
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)
#checando outliers
boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
outliers <- my_boxplot$out
#posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]
boxplot(Sepal.Width ~ Species, data = iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
outliers2 <- my_boxplot2$out
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species == "setosa",
     c("Sepal.Width", "Species")]
#distrib dos dados
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"],
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])
par(mfrow=c(1,1))

#relacao entre variaveis
pairs(vars)
