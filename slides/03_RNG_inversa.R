##======================================================================
## Geração de numeros aleatório não uniformes
## Método da transformação integral de probabilidade

##----------------------------------------------------------------------
## Variáveis discretas

## Bernoulli -----------------------------------------------------------
## Rizzo, ex. 3.4
n <- 1000
p <- 0.4
set.seed(1)
u <- runif(n)
x <- as.integer(u > 0.6)

x2 <- integer(n)
q <- 1 - p
for(i in 1:length(x2)) {
    if(u[i] < q) {
        x2[i] <- 0L
    } else {
        x2[i] <- 1L
    }
}
identical(x, x2)

x3 <- ifelse(u < q, 0L, 1L)
identical(x2, x3)

x4 <- integer(n)
x4[u >= q] <- 1L
identical(x3, x4)

plot(ecdf(x))
plot(ecdf(rbinom(1000, size = 1, p = p)), add = TRUE, col = 2)

rbinom(n, size = 1, prob = p)
sample(c(0, 1), size = n, replace = TRUE, prob = c(0.6, 0.4))

## Uniforme discreta ---------------------------------------------------

n <- 1000
set.seed(1)
u <- runif(n)
k <- 10
u[u < 1/k]
u[u >= 1/k & u < 2/k]
## Seria muito trabalhoso

xc <- cut(u, breaks = c(0, seq(1/k, (k-1)/k, 0.1), k/k),
          include.lowest = TRUE)
table(xc)
x <- as.numeric(xc)

plot(ecdf(x))
plot(ecdf(sample(1:10, size = n, replace = TRUE)), add = TRUE, col = 2)


x <- 1:4
px <- c(.2, .15, .25, .4)
cumsum(px)

u <- runif(1)
x <- 1
Fx <- px[x]
while(u > Fx) {
    x <- x + 1
    Fx <- Fx + px[x]
}
c(u, Fx, x)

randd <- function(x, px, x0 = 1) {
    u <- runif(1)
    x <- x0
    Fx <- px[x]
    while(u > Fx) {
        x <- x + 1
        Fx <- Fx + px[x]
    }
    return(x)
}

replicate(10, randd(x = 1:4, px = px))

x <- 1:10
px <- 1:10/10

set.seed(2)
replicate(10, randd(x = 1:10, px = rep(1/10, 10)))

randd(x = 0:1, px = c(.6, .4), x0 = 0)


## Poisson -------------------------------------------------------------

randpois <- function(lambda) {
    lambda <- 5
    u <- runif(1)
    x <- 0
    p0 <- exp(-lambda)
    p <- p0
    Fx <- p
    while(u > Fx) {
        x <- x + 1
        p <- (lambda/x) * p
        Fx <- Fx + p
    }
    return(x)
}

replicate(10, randpois(lambda = 5))
x <- replicate(10000, randpois(lambda = 5))
plot(ecdf(x))
plot(ecdf(rpois(x, lambda = 5)), add = TRUE, col = 2)
curve(ppois(x, lambda = 5), add = TRUE, type = "s", col = 3)


## Geometrica ----------------------------------------------------------

random_geo <- function(p) {
    q <- 1 - p
    pq <- p * q
    x <- 1
    Fx <- p
    u <- runif(1)
    while (u > Fx) {
        x <- x + 1 # Incrementa x.
        Fx <- Fx + pq # Calcula F(x).
        pq <- pq * q # Atualiza.
    }
    return(x)
}
x <- replicate(10000, random_geo(p = 0.5))
plot(ecdf(x))
curve(pgeom(x - 1, p = 0.5), add = TRUE, col = 2)
