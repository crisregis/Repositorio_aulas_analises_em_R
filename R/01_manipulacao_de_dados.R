# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#-----------------------------------------------------------#
# carregando os pacotes necessários
library("tidyr")
?list.files
#listando arquivos do diretório
files.path <- list.files(path = "data/cestes",
                         pattern = ".csv",
                         full.names = TRUE)
files.path
comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])
#Aplicando as funções head, dim e summary para inspecionar todos os arquivos
head(comm)
dim(comm)
summary(comm)

head(coord)
dim(coord)
summary(coord)

head(envir)
dim(envir)
summary(envir)

head(splist)
dim(splist)
summary(splist)

head(traits)
dim(traits)
summary(traits)

#Sumário de dados
#dados de quantas espécies
nrow(splist)
#áreas amostradas
nrow(comm)
nrow(envir)
#variáveis ambientais
# todas as variáveis exceto a primeira coluna com o id
names(envir)[-1]
# contando quantas variáveis
length(names(envir)[-1])
#riqueza de cada área
comm.pa <- comm[ ,-1] > 0
# vamos nomear as linhas das planilhas com o id dos sites
row.names(comm.pa) <- envir$Sites
#TRUE e FALSE contam como 1 e 0. Calcular a riqueza da área 1 somando a primeira linha do novo objeto comm.pa.
sum(comm.pa[1, ])
#fazer a soma de forma automatizada para todas as áreas usando a função apply
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
summary(rich)
#juntando diferentes tabelas por identificadores comuns
#usando função merge do pacote base para adicionar a coluna de coordenadas ao objeto contendo as variáveis ambientais
envir$Sites
summary(envir$Sites)
#Transformando tipos de variáveis
# se checarmos a classe desse vetor, veremos que é numerica
class(envir$Sites)
# queremos que seja uma variável categórica. Para isso, convertemos em fator
as.factor(envir$Sites)
# se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
envir$Sites <- as.factor(envir$Sites)
#fazer o mesmo para a variável Sites do objeto coord
coord$Sites <- as.factor(coord$Sites)
#Juntando coord e envir usando função merge
envir.coord <- merge(x = envir,
                     y = coord,
                     by = "Sites")
#checando funções dim e head
dim(envir)
dim(coord)
dim(envir.coord)
head(envir.coord)

#Transformando a matriz
#vetor contendo todos os Sites
Sites <- envir$Sites
length(Sites)
# vetor número de espécies
n.sp <- nrow(splist)
n.sp
#criando tabela com cada especie em cada area especies em linhas
comm.df <- tidyr::gather(comm[, -1])
#checando cabeçalho e dimensoes do objeto
dim(comm.df)
head(comm.df)
colnames(comm.df)
colnames(comm.df) <-  c("TaxCode", "Abundance")
colnames(comm.df)
#adicionar coluna
#criando sequencia
seq.site <- rep(Sites, time = n.sp)
#checando dimensao
length(seq.site)
#adicionando ao objeto
comm.df$Sites <- seq.site
#checando como ficou
head(comm.df)

#Juntando todas variaveis
comm.sp <- merge(comm.df, splist, by = "TaxCode")
head(comm.sp)
names(traits)
#renomeando primeiro elemento
colnames(traits)[1] <- "TaxCode"
comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)
comm.total <- merge(comm.traits, envir.coord, by = "Sites")
head(comm.total)
# exportando a planilha final modificada
write.csv(x = comm.total,
          file = "data/01_data_format_combined.csv",
          row.names = FALSE)

