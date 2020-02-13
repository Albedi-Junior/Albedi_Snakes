
# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#--------------------------------------------------#

#library("tidyr")

files.path <- list.files(path = "data/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

files.path

comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])

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

nrow(splist)

nrow(comm)
nrow(envir)

names(envir)[-1] # todas as variáveis exceto a primeira coluna com o id

length(names(envir)[-1]) # contando quantas variáveis

comm.pa <- comm[, -1] > 0

head(comm.pa)
row.names(comm.pa) <- envir$Sites

#riqueza de cada area

sum(comm.pa[1, ])

#calcularriqueza de todos 97
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
summary(rich)

envir$Sites

summary(envir$Sites) #97 total

class(envir$Sites) #vetor, veremos que é numerica
as.factor(envir$Sites) #variável categórica. Para isso, convertemos em fator
envir$Sites <- as.factor(envir$Sites) #vamos então fazer uma atribuição

coord$Sites <- as.factor(coord$Sites)

envir.coord <- merge(x = envir,
                     y = coord,
                     by = "Sites")

# checar a junção com as funções
dim(envir)
dim(coord)
dim(envir.coord)
head(envir.coord)

#Transformando uma matrix espécie vs. área em uma tabela de dados

# vetor contendo todos os Sites
Sites <- envir$Sites
length(Sites)

# vetor número de espécies
n.sp <- nrow(splist)
n.sp

# criando tabela com cada especie em cada area especies em linhas
comm.df <- tidyr::gather(comm[, -1])

#checando o cabeçalho e as dimensões do objeto
dim(comm.df)
head(comm.df)

colnames(comm.df) #nomes atuais
colnames(comm.df) <-  c("TaxCode", "Abundance") # modificando os nomes das colunas
colnames(comm.df) # checando os novos nomes

seq.site <- rep(Sites, time = n.sp) # criandos sequências
length(seq.site) # checando a dimensão
comm.df$Sites <- seq.site
head(comm.df)

#Juntando todas as variáveis à comm.df
comm.sp <- merge(comm.df, splist, by = "TaxCode")

head(comm.sp)

names(traits)
colnames(traits)[1] <- "TaxCode"
comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)


comm.total <- merge(comm.traits, envir.coord, by = "Sites")
head(comm.total)

write.csv(x = comm.total,
          file = "data/01_data_format_combined.csv",
          row.names = FALSE)



##########################



