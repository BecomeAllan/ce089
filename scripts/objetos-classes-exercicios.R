##======================================================================
## Resolução dos exercícios da página objetos-classes.html
##======================================================================

##----------------------------------------------------------------------
## ---- ex1
## 1.
runif(30, 0, 1)
runif(30, max = 5, min = -5)
runif(min = 10, max = 500, n = 30)
## 2.
?"+"
## 3.
soma <- function(x, y){
    x + y
}
soma(x = 2, y = 4)
soma(2, 4)
## 4.
dados1 <- function(){
    sample(1:6, size = 1)
}
dados1()
## 5.
dados2 <- function(){
    ## sample(1:6, size = 2, replace = TRUE)
    d1 <- sample(1:6, size = 1)
    d2 <- sample(1:6, size = 1)
    return(c(d1, d2))
}
dados2()
## 6.
## OBS.: Usando o argumento ...
dados3 <- function(x = 1:6, n = 1, ...){
    sample(x = x, size = n, ...)
}
dados3(n = 2, replace = TRUE)
dados3(n = 8)
dados3(n = 8, replace = TRUE)

##----------------------------------------------------------------------
## ---- ex2
## 1.
x <- 32 + 16^2 - 25^3
## 2.
y <- x/345
## 3.
uni <- runif(30, 10, 50)
## 4.
rm(y)
## 5.
rm(list = ls())
## 6.
apropos("poisson")
rpois(100, lambda = 5)

##----------------------------------------------------------------------
## ---- ex3
## 1.
x <- c(54, 0, 17, 94, 12.5, 2, 0.9, 15)
## a.
x + c(5, 6)
x + c(5, 6, 7)
## 2.
x <- rep(c("A", "B", "C"), times = c(15, 12, 8))
## a.
x == "B"
## b.
sum(x == "B")
## 3.
set.seed(11) # para que o resultado seja sempre o mesmo
y <- runif(100)
y >= 0.5
sum(y >= 0.5)
## 4.
int <- 1:50
x <- 2^int
x
# a.
y <- int^2
y
# b.
x == y
x[x == y] # avançado
# c.
sum(x == y)
## 5.
x <- seq(0, 2*pi, by = 0.1)
a <- sin(x)
b <- cos(x)
c <- tan(x)
# a.
c2 <- a/b
# b.
difer <- c - c2
difer
# c.
difer == 0
c[difer == 0] # quais
all.equal(c, c2)
identical(c, c2)
# d.
max(difer)
max(abs(difer))

##----------------------------------------------------------------------
## ---- ex4
## 1.
mm <- matrix(c(2, 8, 4,
               0, 4, 1,
               9, 7, 5),
             ncol = 3, byrow = TRUE)
mm
## 2.
colnames(mm) <- c("A", "B", "C")
rownames(mm) <- c("x", "y", "z")
mm
## 3.
ml <- list(rep(c("A", "B", "C"), times = c(2, 5, 4)),
           mm)
ml
## 4.
names(ml)
names(ml) <- c("vetor", "matriz")
ml
## 5.
praias <- as.factor(c("brava", "joaquina", "armação"))
praias
ml$praias <- praias
ml
## 6.
dados <- data.frame(local = LETTERS[1:4],
                    cont = c(42, 34, 59, 18))
dados
str(dados)
## 7.
## Deixando os caracteres serem convertidos para fator
da <- data.frame(nome = "Fernando",
                 sobrenome = "Mayer",
                 animal = TRUE,
                 nanimal = 1)
da
str(da)
## Para poder adicionar níveis de um fator, os níveis já precisam estar
## disponíveis
levels(da$nome)
levels(da$nome) <- c("Fernando", "João")
levels(da$sobrenome)
levels(da$sobrenome) <- c("Mayer", "Silva")
str(da)
da <- rbind(da, list(as.factor("João"), as.factor("Silva"), FALSE, 0))
da
str(da)
## Outra pessoa
levels(da$nome)
levels(da$nome) <- c("Fernando", "João", "Maria")
levels(da$sobrenome)
levels(da$sobrenome) <- c("Mayer", "Silva", "Madalena")
da <- rbind(da, list(as.factor("Maria"), as.factor("Madalena"), TRUE, 2))
da
str(da)
## Nova coluna - mesmo com cbind vira fator
da <- cbind(da, time = c(NA, "Atlético", "Paraná"))
da
str(da)

## Mantendo strings como caracteres tudo se simplifica
db <- data.frame(nome = "Fernando",
                 sobrenome = "Mayer",
                 animal = TRUE,
                 nanimal = 1,
                 stringsAsFactors = FALSE)
db
str(db)
db <- rbind(db, list("João", "Silva", FALSE, 0))
db
str(db)
db <- rbind(db, list("Maria", "Madalena", TRUE, 2))
db
str(db)
## Mesmo aqui o cbind é criado como fator
db <- cbind(db, time = c(NA, "Atlético", "Paraná"))
db
str(db)
db$time <- as.character(db$time)
str(db)

##======================================================================
