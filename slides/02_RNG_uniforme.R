##======================================================================
## Geração de números aleatórios uniformes


##----------------------------------------------------------------------
## Algumas informações importantes
unlist(.Platform)
capabilities()
unlist(.Machine)
2e308
1e308
1.7e308
1.79e308
1.8e308

##----------------------------------------------------------------------
## Geração de números uniformes

## Para ver a documentação geral de geração de números uniformes no R
?Random

## Basicamente qualquer função incremental pode gerar números aleatórios
naive <- function(n, x0, a) {
    x <- integer(n + 1)
    x[1] <- x0
    for(i in 2:length(x)) {
        x[i] <- (a * x[i - 1])
    }
    return(x[-1])
}
naive(n = 10, x0 = 2, a = 345)

## Gerador congruencial linear
rcl <- function(n, x0, m, a, c, unit = TRUE) {
    x <- integer(n + 1)
    x[1] <- x0
    for(i in 2:length(x)) {
        x[i] <- (a * x[i - 1] + c) %% m
    }
    if(unit) x <- x/m
    return(x[-1])
}

## Algumas sequencias
rcl(n = 10, x0 = 1, m = 1e6, a = 1, c = 1)
rcl(n = 10, x0 = 1, m = 1e6, a = 143, c = 1)

## Se c = 0, é chamado de gerador congruencial multiplicativo
rcl(n = 10, x0 = 1, m = 1e6, a = 1, c = 0)
rcl(n = 10, x0 = 1, m = 1e6, a = 143, c = 0)

## Periodo
## A sequencia se repete por um periodo que nao eh maior do que m. Se a,
## c e m sao escolhidos adequadamente, entao a sequencia tera periodo
## maximo igual a m

## x_n+p = x_n => p é o periodo

## Periodo = 4 < m = 10
rcl(n = 12, x0 = 7, m = 10, a = 7, c = 7, unit = FALSE)

## Outros períodos
rcl(n = 12, x0 = 8, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 1, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 0, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 2, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 2, m = 100, a = 7, c = 7, unit = FALSE)

## É preciso entao escolher valores adequados para se obter periodos
## mais longos possiveis

rcl(n = 12, x0 = 7, m = 2^31, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 7, m = 2^31, a = 7, c = 7, unit = TRUE)

## Park e Miller
rcl(n = 12, x0 = 7, m = 2^31 - 1, a = 7^5, c = 0, unit = FALSE)
rcl(n = 12, x0 = 7, m = 2^31 - 1, a = 7^5, c = 0, unit = TRUE)

library(randtoolbox)
congruRand(10)
setSeed(7)
congruRand(12)

setSeed(1)
congruRand(10)

setSeed(1)
SFMT(10)

set.seed(1)
runif(10)

hist(rcl(n = 100, x0 = 1, m = 1e6, a = 143, c = 1))

set.seed(1)
hist(runif(100))

##----------------------------------------------------------------------
## Testes

x <- rcl(n = 1000, x0 = 1, m = 2^12, a = 125, c = 1, unit = TRUE)
hist(x)

xc <- cut(x, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
table(xc)
chisq.test(x = table(xc))

ks.test(x, "punif")

plot(ecdf(x))
plot(ecdf(runif(1000)), add = T)

plot(x)
acf(x)

##======================================================================

## a = 7^5
## m = 2^31 - 1
rand_congr0 <- function(n = 1, x0, a = 16807, m = 2147483647) {
    if (missing(x0)) x0 <- as.integer(Sys.time())
    stopifnot(x0 < m)
    n <- n + 1
    x <- vector(mode = "integer", length = n)
    x[1] <- x0
    for (i in 2:n) {
        x[i] <- (a * x[i - 1]) %% m
    }
    return(x[-1])
}
rand_congr0(n = 10, x0 = 321)
rand_congr0(n = 10, x0 = 1, a = 1, m = 10)
