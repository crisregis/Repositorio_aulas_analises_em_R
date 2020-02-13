#Criando e salvando graficos
#tutorial aula 8
#carregando os pacotes
library (base)
library(ggplot2)

#lendo os dados
comm <- read.csv("data/cestes/comm.csv")
envir <- read.csv("data/cestes/envir.csv")
#riqueza por site
comm.pa <- comm[, -1] > 0
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
head(rich)
summary(rich)
#boxplot result acima
boxplot(rich)
?par
boxplot(rich, las = 1)
#nova tabela
localidades <- cbind(envir, rich)
head(localidades)

#grafico de dispersao
#criando modelos lineares
riqsilt <- lm(rich ~ Silt, data = localidades)
riqclay <- lm(rich ~ Clay, data = localidades)
riqsand <- lm(rich ~ Sand, data = localidades)
# extraindo os coeficientes do modelo
coef_s <- coef(riqsilt)
coef_c <- coef(riqclay)
coef_d <- coef(riqsand)
# definindo os limites dos eixos
limy <- c(min(localidades$rich),
          max(localidades$rich))
limx <- c(min(localidades[,c("Clay", "Sand", "Silt")]),
          max(localidades[,c("Clay", "Sand", "Silt")]))
#definicao do nome do eixo y
laby <- "Riqueza de espécies"
#definicao de parametros graficos
par(mfrow = c(1, 3),
    las = 1,
    bty = "l")
#plot riqueza funcao silte
plot(rich ~ Silt, data = localidades,
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby,
     xlab = "Teor de Silte (%)")
#linha tendencia silte
abline(a = coef_s[1], b = coef_s[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)
#plot riqueza funcao argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
#linha de tendencia argila
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)
#riqueza funcao areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
#linha de tendencia
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)

#exportanto os graficos
png("figs/figura01.png", res = 300, width = 2400, height = 1200)
#definicao de parametros graficos
par(mfrow = c(1, 3),
    las = 1,
    bty = "l")
#plot riqueza teor de silte
plot(rich ~ Silt, data = localidades,
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby,
     xlab = "Teor de Silte (%)")
#linha de tendencia
abline(a = coef_s[1], b = coef_s[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)
#plot riqueza funcao argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
#linha de tendencia
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)
#plot riqueza funcao areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
#linha de tendencia
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)
dev.off()

#construcao de box plot
#vetor de cores
cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")
#vetor com nome das especies
sp <- paste("I.", unique(iris$Species), sep = " ")
par(mfrow = c(2, 2),
    mar = c(4, 1, 1, 1),
    bty = 'l',
    las = 1)
boxplot(Sepal.Length ~ Species,
        data = iris,
        xlab = "",
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Sepal.Width ~ Species,
        data = iris,
        xlab = "",
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Petal.Length ~ Species, data = iris,  col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Petal.Width ~ Species,
        data = iris,
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
par(mfrow = c(1, 1))
#grafico de media com desvio padrao
# fixando uma semente de numeros aleatorios para manter o mesmo resultado no sample
set.seed(42)
#criando data frame com valores medios e desvio padrao
d2 <- data.frame(name = letters[1:5],
                 value = sample(seq(4, 15), 5),
                 sd = c(1, 0.2, 3, 2, 4))
plot(x = 1:5, d2$value, las = 1, bty = 'l',
     ylim = c(0, 18), pch = 19, xaxt = 'n',
     xlab = "names", ylab = "value")
axis(1, at = 1:5, labels = d2$name)
arrows(x0 = 1:5,
       y0 = d2$value + d2$sd,
       y1 = d2$value - d2$sd, angle = 90, length = 0.05, code = 3)
