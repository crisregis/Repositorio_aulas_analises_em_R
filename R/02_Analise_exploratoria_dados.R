#Script de Analise exploratoria de dados#
data("anscombe")
#Funcao basica para checar dados
#dimensao dos dados
dim(anscombe)
#seis primeiras linhas dos dados
head(anscombe)
#classe do objeto
class(anscombe)
#estrutura do objeto
str(anscombe)
#selecionando colunas de dados
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)
#funcao apply
#media de todos os vetores x
apply(anscombe[,1:4], 2, mean)
#media de todos os vetores y
apply(anscombe[,5:8], 2, mean)

#Descricao estatistica dos dados
#varianc
apply(anscombe, 2, var)
#correlação e coeficiente de regressão dos conjuntos x e y
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)
#coeficiente de regressao
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
#lista com todos os modelos
mlist <- list(m1, m2, m3, m4)
#calculo de forma menos repetitiva da regressao
lapply(mlist, coef)
anscombe
# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2,2),
    las=1,
    bty="l")
#plot das variaveis
plot(anscombe$y1 ~ anscombe$x1)
#adicao da reta pelo modelo de regressao
abline(mlist[[1]])

plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])

plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])

plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

# retorna a janela grafica para o padrao de 1 linha e 1 coluna
par(mfrow=c(1, 1))
