##======================================================================
## CE089 - Estatística Computacional 2
## Estatística - UFPR
## Prof. Fernando Mayer
##======================================================================

##======================================================================
## Revisão de conceitos fundamentais da linguagem R ====================
##======================================================================

##======================================================================
## Funções e argumentos

runif(10, 1, 100)
args(sample)
args(plot)

##======================================================================
## Funções for dummies

ola.mundo <- function(){
    writeLines("Olá mundo")
}

ola.mundo()


ola.mundo <- function(texto){
    writeLines(texto)
}

ola.mundo("Funções são legais")


##======================================================================
## Tipos e classes de objetos

## Vetores atômicos ----------------------------------------------------

## double
x <- c(2, 4, 6)
typeof(x)
class(x)
## integer
x <- c(2L, 4L, 6L)
typeof(x)
class(x)
## character
x <- c("a", "b", "c")
typeof(x)
class(x)
## logical
x <- c(TRUE, FALSE, TRUE)
typeof(x)
class(x)
## complex
x <- c(2 + 1i, 4 + 1i, 6 + 1i)
typeof(x)
class(x)
## raw
x <- raw(3)
typeof(x)
class(x)

## Lista ---------------------------------------------------------------
x <- list(1:10, letters[1:5])
typeof(x)
class(x)

##----------------------------------------------------------------------
## Vetores numéricos

num <- c(10, 5, 2, 4, 8, 9)
num
typeof(num)
class(num)

x <- c(10L, 5L, 2L, 4L, 8L, 9L)
x
typeof(x)
class(x)

object.size(num)
object.size(x)

##----------------------------------------------------------------------
## Representação numérica dentro do R

set.seed(123)
(x <- runif(10))

getOption("digits")

set.seed(12)
(y <- runif(10))

## Erro de ponto flutuante
sqrt(2)^2 - 2
print(sqrt(2)^2, digits = 22)

0.3 + 0.6 - 0.9
print(c(0.3, 0.6, 0.9), digits = 22)

print(x, digits = 1)
print(x, digits = 7) # padrão
print(x, digits = 22)

format(x, scientific = TRUE)

##----------------------------------------------------------------------
## Operações com vetores

num
num * 2
num * num
num + c(2, 4, 1) # como se chama?
num + c(2, 4, 1, 3)

##----------------------------------------------------------------------
## Outros tipos de vetores

## Vetor de caracteres -------------------------------------------------

caracter <- c("brava", "joaquina", "armação")
caracter
typeof(caracter)
class(caracter)

## Vetor lógico --------------------------------------------------------

logico <- caracter == "armação"
logico
typeof(logico)
class(logico)

logico <- num > 4
logico

##----------------------------------------------------------------------
## Coerção

## Coerção implícita
w <- c(5L, "a")
x <- c(1.7, "a")
y <- c(TRUE, 2)
z <- c("a", T)

## Coerção explícita
x <- 0:6
typeof(x)
class(x)
as.numeric(x); typeof(as.numeric(x))
as.logical(x); typeof(as.logical(x))
as.character(x); typeof(as.character(x))
as.factor(x); typeof(as.factor(x))

(x <- c(FALSE, TRUE))
class(x)
as.numeric(x)

x <- c("a", "b", "c")
as.numeric(x)
as.logical(x)

##----------------------------------------------------------------------
## Valores perdidos e especiais

perd <- c(3, 5, NA, 2)
perd
class(perd)

is.na(perd)
any(is.na(perd))

perd <- c(-1,0,1)/0
perd
is.na(perd)
is.infinite(perd)

##----------------------------------------------------------------------
## Outras classes

## Fator ---------------------------------------------------------------
fator <- factor(c("alta","baixa","baixa","media",
                  "alta","media","baixa","media","media"))
fator
class(fator)
typeof(fator)
unclass(fator)

as.character(fator)
as.integer(fator)

fator <- factor(c("alta","baixa","baixa","media",
                  "alta","media","baixa","media","media"),
                levels = c("alta","media","baixa"))
fator
typeof(fator)
class(fator)

fator <- factor(c("alta","baixa","baixa","media",
                  "alta","media","baixa","media","media"),
                levels = c("baixa", "media", "alta"),
                ordered = TRUE)
fator
typeof(fator)
class(fator)

levels(fator)
nlevels(fator)

## Matriz --------------------------------------------------------------
matriz <- matrix(1:12, nrow = 3, ncol = 4)
matriz
class(matriz)
typeof(matriz)

#'
#' De `?matrix`
#'
#'      A matrix is the special case of a two-dimensional array.  Since
#'      R 4.0.0, ‘inherits(m, "array")’ is true for a ‘matrix’ ‘m’.
#'

matriz <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
matriz
dim(matriz)

cbind(matriz, rep(99, 3))
rbind(matriz, rep(99, 4))

m <- 1:10
m
class(m)
dim(m)
dim(m) <- c(2, 5)
m
class(m)
typeof(m)

## Operações matemáticas em matrizes
matriz * 2
matriz2 <- matrix(1, nrow = 4, ncol = 3)
matriz %*% matriz2

## Array ---------------------------------------------------------------
(ar <- array(1:12, dim = c(2, 2, 3)))

(ar <- array(1:12, dim = c(3, 2, 2)))
class(ar)
typeof(ar)

##----------------------------------------------------------------------
## Lista
lista <- list(1:30, "R", list(TRUE, FALSE))
lista
class(lista)
typeof(lista)

str(lista)
dim(lista)
length(lista)

lista <- list(fator, matriz)
lista
length(lista)

## Data frame ----------------------------------------------------------
da <- data.frame(nome = c("João", "José", "Maria"),
                 sexo = c("M", "M", "F"),
                 idade = c(32, 34, 30))
da
class(da)
typeof(da)
dim(da)

str(da)

da <- data.frame(nome = c("João", "José", "Maria"),
                 sexo = c("M", "M", "F"),
                 idade = c(32, 34, 30),
                 stringsAsFactors = TRUE)
da
str(da)

##----------------------------------------------------------------------
## Atributos de objetos
x <- 1:6
attributes(x)

names(x)
names(x) <- c("um", "dois", "tres", "quatro", "cinco", "seis")
names(x)
attributes(x)

x
x + 2

names(x) <- NULL
attributes(x)
x

length(x)
length(x) <- 10
x

length(x) <- 6
dim(x)
dim(x) <- c(3, 2)
x
attributes(x)
dim(x) <- NULL
x

x <- list(Curitiba = 1, Paraná = 2, Brasil = 3)
x
names(x)

matriz
attributes(matriz)
rownames(matriz) <- c("A","B","C")
colnames(matriz) <- c("T1","T2","T3","T4")
matriz
attributes(matriz)

da
attributes(da)
names(da)
row.names(da)

##======================================================================
## Indexação

## Vetores -------------------------------------------------------------

cont <- c(8, 4, NA, 9, 6, 1, 7, 9)
cont

cont[]
cont[4]
cont[c(1, 4, 8)]
ind <- c(1, 4, 8)
cont[ind]
cont[-4]
cont[-c(1, 4, 8)]
cont[1:5]
cont[seq(1, 8, by = 2)]
cont[-1:5]
cont[-(1:5)]

cont[is.na(cont)]
cont[!is.na(cont)]

cont[is.na(cont)] <- 0
cont
cont[3] <- NA
cont

## Vetores numéricos ---------------------------------------------------

names(cont) <- letters[1:length(cont)]
cont
cont["d"]
cont[c("f", "a")]

## Matrizes ------------------------------------------------------------

mat <- matrix(1:9, nrow = 3)
mat
mat[2, 3]
mat[, 1]
mat[1, ]

mat[3, 2]
mat[3, 2, drop = FALSE]
mat[1, ]
mat[1, , drop = FALSE]

mat[c(1, 3), c(2, 3)]

colnames(mat) <- LETTERS[1:3]
mat[, "B"]
mat[1, "C"]

rownames(mat) <- LETTERS[24:26]
mat["X", ]
mat["Y", "A"]

## Listas --------------------------------------------------------------

lis <- list(c(3, 8, 7, 4), mat, 5:0)
lis

lis[1]
class(lis[1])

lis[[1]]
class(lis[[1]])

mean(lis[1])
mean(lis[[1]])

lis[[1]][3]
lis[[2]][2, 3]

lis <- list(vetor1 = c(3, 8, 7, 4), mat = mat, vetor2 = 5:0)
## names(lis) <- c("vetor1", "mat", "vetor2")
lis

lis$mat
lis$mat[2, 3]
lis$vetor1[3]

lis[["mat"]]
lis[["vetor1"]][3]

## Data frame ----------------------------------------------------------

da <- data.frame(A = 4:1, B = c(2, NA, 5, 8))
da

da[2, 1]
da[, 2]

da[,"B"]
da[1, ]

da["1", ]

da[, "B"]
da[, "B", drop = FALSE]

da$A
da$B[3]
da$B[c(2, 4)]

da[1]
class(da[1])

da[[1]]
class(da[[1]])
da[["A"]]
da[["A"]][2:3]

da[is.na(da), ]

da[is.na(da$A), ]
da[is.na(da$B), ]

is.na(da)
is.na(da$A)
is.na(da$B)

da[!is.na(da$B), ]

complete.cases(da)
da[complete.cases(da), ]


##======================================================================
## Seleção condicional

dados <- c(5, 15, 42, 28, 79, 4, 7, 14)

dados[dados > 15]

dados[dados > 15 & dados <= 35]

dados > 15 & dados <= 35
dados > 15 | dados <= 35

cara <- letters[1:length(dados)]

dados[cara == "c"]

cara == "a" & cara == "c" # porque não funciona?
cara == "a" | cara == "c"
dados[cara == "a" | cara == "c"]

dados[cara == c("a", "c")] # errado
dados[cara %in% c("a", "c")]
cara %in% c("a", "c")

cara == c("a", "c")
cara %in% c("a", "c")

cara[dados == 15]
cara[dados > 30]
cara[dados %in% c(4, 14)]

dados[dados > 15]
which(dados > 15)

dados[dados > 15 & dados <= 35]
which(dados > 15 & dados <= 35)

dados[cara == "c"]
which(cara == "c")

dados[cara %in% c("a", "c")]
which(cara %in% c("a", "c"))

## Data frame ----------------------------------------------------------
dados <- data.frame(ano = c(2001, 2002, 2003, 2004, 2005),
                    captura = c(26, 18, 25, 32, NA),
                    porto = c("SP", "RS", "SC", "SC", "RN"))

dados[dados$ano == 2004, ]
dados[dados$porto == "SC", ]
dados[dados$captura > 20, "captura"]

dados[dados$captura > 20 & !is.na(dados$captura), ]
dados[dados$captura > 20 & complete.cases(dados), ]

dados[dados$captura > 25 & dados$porto == "SP", ]

dados[dados$porto == "SC", ]
subset(dados, porto == "SC")
dados[dados$captura > 20, ]
subset(dados, captura > 20)
dados[dados$captura > 20 & !is.na(dados$captura), ]
dados[dados$captura > 20, "captura"]
subset(dados, captura > 20, select = captura)
subset(dados, captura > 20, select = captura,  drop = TRUE)
