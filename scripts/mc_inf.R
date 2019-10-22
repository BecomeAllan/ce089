##======================================================================
## Métodos de Monte Carlo em inferência estatística
##======================================================================

## Media e desvio padrão
n <- 20
mu <- 5
dp <- 1
x <- rnorm(n, mu, dp)
mean(x)
sd(x)
sd(x)/sqrt(n)
dp/sqrt(n)

simmc <- function(n, xbar, sd) {
    xmc <- rnorm(n, xbar, sd)
    mean(xmc)
}

m <- 1e4
xsim <- replicate(m,
                  simmc(n = n, xbar = mean(x), sd = dp))
length(xsim)
hist(xsim, freq = FALSE)
curve(dnorm(x, mean(xsim), sd(xsim)), add = TRUE, col = 2)
mean(xsim)
var(xsim)
sd(xsim)
quantile(xsim, probs = c(.025, .975))
mean(xsim) + 1.96 * sd(xsim) * c(-1, 1)


simmcvar <- function(n, xbar, sd) {
    xmc <- rnorm(n, xbar, sd)
    var(xmc)
}

m <- 1e4
xsimv <- replicate(m,
                   simmcvar(n = n, xbar = mu, sd = sd(x)))
length(xsimv)
hist(xsimv, freq = FALSE)
curve(dnorm(x, mean(xsimv), sd(xsimv)), add = TRUE, col = 2)
mean(xsimv)
var(xsimv)
sd(xsimv)
quantile(xsimv, probs = c(.025, .975))
mean(xsimv) + 1.96 * sd(xsimv) * c(-1, 1)

plot(ecdf(xsimv))
curve(pnorm(x, mean(xsimv), sd(xsimv)), add = TRUE, col = 2)

(vv <- (2*var(x)^2)/(n-1))
sqrt(vv)
## Quantis da qui-quadrado
q1 <- qchisq(.025, df = n-1)
q2 <- qchisq(.975, df = n-1)
## Intervalo de 95%
((n-1) * var(x))/c(q2,q1)

int <- replicate(100, expr = {
    x <- rnorm(n, mu, sd(x))
    ((n-1) * var(x))/c(q2,q1)
})
int <- t(int)
sum(int[, 1] < dp^2 & int[, 2] > dp^2)/100
plot(NULL, NULL, xlim = c(0, 100), ylim = range(int))
segments(1:100,
         int[, 1],
         1:100,
         int[, 2])
abline(h = dp^2, col = 2)

int2 <- replicate(100, expr = {
    x <- rnorm(n, mu, sd(x))
    var(x) + 1.96 * (var(x) * sqrt(2/(n-1))) * c(-1,1)
})
int2 <- t(int2)
sum(int2[, 1] < dp^2 & int2[, 2] > dp^2)/100
plot(NULL, NULL, xlim = c(0, 100), ylim = range(int2))
segments(1:100,
         int2[, 1],
         1:100,
         int2[, 2])
abline(h = dp^2, col = 2)




simmcvar <- function(n, xbar, sd) {
    xmc <- rnorm(n, xbar, sd)
    var(xmc)
}
m <- 1e4
xsimv <- replicate(m,
                   simmcvar(n = n, xbar = mean(x), sd = sd(x)))
length(xsimv)
hist(xsimv, freq = FALSE)
curve(dnorm(x, mean(xsimv), sd(xsimv)), add = TRUE, col = 2)
mean(xsimv)
var(xsimv)
sd(xsimv)
quantile(xsimv, probs = c(.025, .975))
mean(xsimv) + 1.96 * sd(xsimv) * c(-1, 1)

(vv <- (2*var(x)^2)/(n-1))
sqrt(vv)
## Quantis da qui-quadrado
q1 <- qchisq(.025, df = n-1)
q2 <- qchisq(.975, df = n-1)
## Intervalo de 95%
((n-1) * var(x))/c(q2,q1)

int <- replicate(100, expr = {
    x <- rnorm(n, mean(x), sd(x))
    ((n-1) * var(x))/c(q2,q1)
})
int <- t(int)
sum(int[, 1] < dp^2 & int[, 2] > dp^2)/100
plot(NULL, NULL, xlim = c(0, 100), ylim = range(int))
segments(1:100,
         int[, 1],
         1:100,
         int[, 2])
abline(h = dp^2, col = 2)

int2 <- replicate(100, expr = {
    x <- rnorm(n, mean(x), sd(x))
    var(x) + 1.96 * (var(x) * sqrt(2/(n-1))) * c(-1,1)
})
int2 <- t(int2)
sum(int2[, 1] < dp^2 & int2[, 2] > dp^2)/100
plot(NULL, NULL, xlim = c(0, 100), ylim = range(int2))
segments(1:100,
         int2[, 1],
         1:100,
         int2[, 2])
abline(h = dp^2, col = 2)


##======================================================================
## Regressão

## Simulação
set.seed(123)
n <- 1000
b0 <- 10
b1 <- 0.5
x <- rnorm(n, mean = 150, sd = 15)
sigma2 <- 20
y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))
plot(x, y)

m0 <- lm(y ~ x)
b0.lm <- unname(coef(m0)[1])
b1.lm <- unname(coef(m0)[2])
s2.lm <- summary(m0)$sigma^2
summary(m0)

b0.var <- unname(sqrt(diag(vcov(m0)))[1])
b1.var <- unname(sqrt(diag(vcov(m0)))[2])
var.sigma2.calc <- (2 * s2.lm^2)/(n-2)

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
    b0 <- rnorm(1, b0.lm, sqrt(b0.var))
    b1 <- rnorm(1, b1.lm, sqrt(b1.var))
    sigma2 <- rnorm(1, s2.lm, sqrt(var.sigma2.calc))
    y.mc <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))
    mm <- lm(y.mc ~ x)
    b0.boot[i] <- coef(mm)[1]
    b1.boot[i] <- coef(mm)[2]
    s2.boot[i] <- summary(mm)$sigma^2
}

hist(b0.boot)
mean(b0.boot)
var(b0.boot)
sd(b0.boot)
b0.var

hist(b1.boot)
mean(b1.boot)
var(b1.boot)
sd(b1.boot)
b1.var

hist(s2.boot)
mean(s2.boot)
var(s2.boot)
sd(s2.boot)
b1.var
var.sigma2.calc

stop
##======================================================================

eqm <- function(n, xbar, sd) {
    xmc <- rnorm(n, xbar, sd)
    mean(xmc)
}


sum((xsim - 10)^2)/m

xmc <- rnorm(1000, mean(x), sd(x))
mean(xmc)
sd(xmc)
sd(xmc)/length(xmc)


dp <- function(x) {
    n <- length(x)
    xbar <- mean(x)
    var <- sum((x - xbar)^2)/n
    sd <- sqrt(var)
    return(sd)
}



dp(x)
sd(x) * (n/(n - 1))
dp(x) * (n/(n - 1))
sd(x)

dp(x)^2
sd(x)^2

dp(x)^2
sd(x)^2 * (n/(n - 1))
var(x)

dp(x)/sqrt(n)

sum((x - mean(x))^2)/100
sqrt(sum((x - mean(x))^2)/100)
sqrt(sum((x - mean(x))^2)/100)/sqrt(length(x))


##======================================================================
## Rizzo

### Example 7.4 (Confidence interval for variance)

n <- 20
alpha <- .05
x <- rnorm(n, mean=0, sd=2)
UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1)

### Example 7.5 (MC estimate of confidence level)

n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
    x <- rnorm(n, mean = 0, sd = 2)
    (n-1) * var(x) / qchisq(alpha, df = n-1)
} )
##count the number of intervals that contain sigma^2=4
sum(UCL > 4)
##or compute the mean to get the confidence level
mean(UCL > 4)

hist(UCL)

### Example 7.6 (Empirical confidence level)

n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
    x <- rchisq(n, df = 2)
    (n-1) * var(x) / qchisq(alpha, df = n-1)
} )
sum(UCL > 4)
mean(UCL > 4)

hist(UCL)


##======================================================================
## Dados
n <- 250
y <- 32

## Proporção amostral
(theta.hat <- y/n)

## Intervalo de confiança para a proporção

## Com alpha = 0.05, define valor crítico
(zc <- qnorm(.025))
## Calcula erro padrão
(ep <- sqrt((.5 * .5)/n))
## Determina intervalo
theta.hat - zc * ep * c(-1, 1)

## Teste de hipótese
## H0: theta = 0.15
## Ha: theta < 0.15
theta0 <- 0.15

## Estatistica de teste
(zcalc <- (theta.hat - theta0)/sqrt((theta0 * (1-theta0))/n))
## Com alpha = 0.05, o valor cítico é
(zcrit <- qnorm(.025))
ifelse(abs(zcalc) > abs(zcrit), "Rejeita H0", "Não rejeita H0")

## p-valor
pnorm(zcalc)

## Por simulação, sob theta0

## Gera k amostras com n = 250 e assumindo que theta0 é verdadeiro
set.seed(12)
k <- 10000
am <- rbinom(k, size = 250, prob = theta0)

## Proporção amostral
theta.hat.am <- am/n
hist(theta.hat.am)

## Padroniza a distribuição das propoções amostrais
z <- (theta.hat.am - theta0)/sqrt((theta0 * (1-theta0))/n)
hist(z, freq = FALSE)
## Aproximação pela normal
curve(dnorm, -3, 3, add = TRUE)
abline(v = zcalc, col = 2)

## Proporcao da amostra abaixo de theta0 ~ "p-valor"
sum(z < zcalc)/k
