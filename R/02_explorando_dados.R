##################Análise Exploratória de Dados#############

library(knitr)
library(rmarkdown)
data("anscombe")

#Funções básicas para checar os dados#

dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
head(anscombe) # seis primeiras linhas dos dados

#Selecionando colunas dos dados
# fazer a média por das colunas com x
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

# o mesmo calculo, agora apenas em 1 linha de comando
## media de todos os vetores x

apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto
## media de todos os vetores y
apply(anscombe[,5:8], 2, mean)

# variância dos dados
apply(anscombe, 2, var) # aplica a funcao var a todas as linhas do objeto

##correlação e coeficiente de regressão dos conjuntos x e y
# correlação

cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

# coeficiente de regressão
## primeiro criamos objetos com as regressoes dos quatro conjuntos

m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)

## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)

## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

anscombe #Em que os dados são diferentes?

# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2, 2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L

#plot das variaveis
plot(anscombe$y1 ~ anscombe$x1)
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow=c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

### Uma rotina (entre muitas possíveis) de análise exploratória####
#Padrões morfológicos de espécies de Iris#

head(iris)
summary(iris)

table(iris$Species) #Quantas informações por espécie?

#Qual a média das variáveis por espécie?
# media do comprimento de sepala por especie

tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)

# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)

# ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)

# fazer o mesmo para as outras variáveis#
aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)

#calcular o desvio padrão das variáveis#

tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

##calular a média por espécie de todas as variáveis. Para isso, vamos usar o comando for e executar todas as tarefas em um mesmo ciclo##

#criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}
medias #chamar as medias

###Estatísticas descritivas###
#Medidas de tendência central
#Media#
vars <- iris[, -5]
apply(vars, 2, mean)

#Mediana: 50º quantil, de forma que divide os dados em duas metades
apply(vars, 2, median)

#Moda: valor mais frequente na amostra
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

##Medidas de dispersão
#Variância: desvio da média
apply(vars, 2, var)

#Desvio padrão: raiz quadrada da variância
sd01 <- apply(vars, 2, sd)

# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01 == sd02

##Coeficiente de variação: medida relativa de desvio padrão
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)

##Quantis ou percentis
# sumario de 5 numeros
apply(vars, 2, quantile)

# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

#Intervalo (range)
# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)

# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
# nunca nomeie um objeto com um nome já existente
my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)

##Intervalo interquartil (IIQ)
apply(vars, 2, IQR)

##Correlação
cor(vars)

#####Métodos gráficos#####

#Gráfico de barras
barplot(table(iris$Species))

#Histograma
par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
par(mfrow=c(1, 1))

##Curva de densidade
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)

par(mfrow=c(1, 1))

par(mfrow=c(1, 2))

# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE
par(mfrow=c(1, 1))

##Box-plot
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

# valores por espécie
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

##usar a própria função boxplot para identificar os outliers
boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot

# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out

#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)

# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

#identificar outliers de maneira espécie específica
boxplot(Sepal.Width ~ Species, data = iris)

my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2

# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out

# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species == "setosa",
     c("Sepal.Width", "Species")]


##Entendendo a distribuição dos dados##
# dados morfométricos das espécies de Iris e comparar com uma distribuição normal. No R, isto pode ser feito de forma visual com as funções qqnorm e qqline
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"],
main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])
par(mfrow=c(1,1))

##Relação entre variáveis##
#explorar a relação entre muitas variáveis
pairs(vars)

## correlação entre variáveis e a curva de densidade probabilística de cada uma das variáveis##
# EXEPCIONALMENTE vamos carregar o pacote agora, já que esse é um exercício bonus.
#library("GGally")
#ggpairs(vars)

