##======================================================================
## Encontrando erros

## Traceback
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) nada()
f()
traceback()

## Traceback
lm(y ~ x)
traceback()

## Debug

## Formula de baskara
baskara <- function(a, b, c) {
    delta <- b^2 - 4 * a * c
    denom <- 2 * a
    sqrt_delta <- sqrt(delta)
    x1 <- (-b - sqrt_delta)/denom
    x2 <- (-b + sqrt_delta)/denom
    return(c(x1, x2))
}

## Onde estão os problemas?
baskara(-3, 2, 1)
baskara(0, 2, 1)
baskara(3, 2, 1)

## Problema 1: a = 0
baskara(0, 2, 1)

baskara <- function(a, b, c) {
    if(a == 0) stop("a nao pode ser zero")
    ## stopifnot(a != 0)
    delta <- b^2 - 4 * a * c
    denom <- 2 * a
    sqrt_delta <- sqrt(delta)
    x1 <- (-b - sqrt_delta)/denom
    x2 <- (-b + sqrt_delta)/denom
    return(c(x1, x2))
}

baskara(0, 2, 1)
traceback()
baskara(-3, 2, 1)
baskara(3, 2, 1)

tryCatch(baskara(0, 2, 1),
         error = function(cmd) NA)

xx <- try(baskara(0, 2, 1))

baskara(-3, 2, 1)
baskara(3, 2, 1)



## Erro 2: raiz negativa
baskara <- function(a, b, c) {
    if(a == 0) stop("a nao pode ser zero")
    ## stopifnot(a != 0)
    delta <- b^2 - 4 * a * c
    if(delta < 0) stop("raiz negativa")
    denom <- 2 * a
    sqrt_delta <- sqrt(delta)
    x1 <- (-b - sqrt_delta)/denom
    x2 <- (-b + sqrt_delta)/denom
    return(c(x1, x2))
}

baskara(0, 2, 1)
traceback()
baskara(-3, 2, 1)
baskara(3, 2, 1)
traceback()

test <- function(x){
    mean(x),
}

test <- function(x){
    x <- x + 1
    y <- mean(x)
    z <- max(x)
    x <- ifelse(x > 5, x, x - 100)
    list(x, y)
}

url <- "http://leg.ufpr.br/~fernandomayer/data/pib_gapminder.csv"
da <- read.table(url, header = TRUE, sep = ",", dec = ".")
str(da)

subset.drop.all <- function(x, ...){
    if(!is.data.frame(x)) stop("'x' must be a data.frame")
    x <- subset(x, ...)
    factors <- which(sapply(x, class) %in% "factor")
    for(i in factors){
        x[,i] <- sapply(x[,i], "[", drop=T)
    }
    return(x)
}

profvis(
    db <- subset.drop.all(da, pais == "Brazil")
)

Rprof()
db <- subset.drop.all(da, pais == "Brazil")
Rprof(NULL)
summaryRprof()

str(db)


## Simulação
set.seed(123)
n <- 1000
b0 <- 10
b1 <- 0.5
x <- rnorm(n, mean = 150, sd = 15)
sigma2 <- 20
y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))
plot(x, y)

## Número de amostras
r <- 1e4
## Número de elementos em cada amostra
m <- 100

## Vetores para armazenar os resultados
b0.boot <- numeric(r)
b1.boot <- numeric(r)
s2.boot <- numeric(r)
set.seed(123)
for(i in 1:r){
    if(i == 1) {
        mm <- try(lm(letters ~ 1))
    } else {
        select <- sample(1:length(y), size = m, replace = TRUE)
        x.boot <- x[select]
        y.boot <- y[select]
        mm <- lm(y.boot ~ x.boot)
        if(class(mm) == "try-error") {
            b0.boot[i] <- NA
            b1.boot[i] <- NA
            s2.boot[i] <- NA
        } else {
            b0.boot[i] <- coef(mm)[1]
            b1.boot[i] <- coef(mm)[2]
            s2.boot[i] <- summary(mm)$sigma^2
        }
    }
}

par(mfrow = c(1, 3))
hist(b0.boot)
hist(b1.boot)
hist(s2.boot)
par(mfrow = c(1, 1))
