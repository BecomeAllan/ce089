##======================================================================
## Variâncias de estimadores pontuais
##======================================================================

## ----- simula --------------------------------------------------------
n <- 100
mu <- 12
sigma <- 2
set.seed(123)
x <- rnorm(n, mu, sigma)

## ----- pontuais ------------------------------------------------------
(mx <- mean(x))
(vx <- var(x))
(sdx <- sd(x))

## ----- assintot ------------------------------------------------------
MASS::fitdistr(x, "normal")

## ----- varmedia ------------------------------------------------------
(vm <- vx/n)
(epm <- sdx/sqrt(n))
2/sqrt(n)

## ----- resmedia ------------------------------------------------------
c(mx, epm)

## ----- varvar --------------------------------------------------------
vx
(vv <- (2*vx^2)/(n-1))
(epv <- vx * sqrt(2/(n-1))) # ou sqrt(vv)

## ---- varsd ----------------------------------------------------------
sdx
(vsdx <- (2*sdx^4)/(4*sdx^2*(n-1)))
## (1/(2*sdx))^2 * vv
(epsdx <- sqrt(vsdx))

## ----- resfinal ------------------------------------------------------
MASS::fitdistr(x, "normal")
matrix(c(mx, epm, sdx, epsdx), ncol = 2)

## ----- vies ----------------------------------------------------------

## Corrige vies da estimativa de MV
1.8164807^2 * (n/(n-1))
sqrt(1.8164807^2 * (n/(n-1)))
## Adicoina viés na variância amostral
var(x) * ((n-1)/n)
sqrt(var(x) * ((n-1)/n))

## ----- simula --------------------------------------------------------
n <- 100
mu <- 12
sigma <- 2
set.seed(123)
x <- rnorm(n, mu, sigma)

## Quantis da qui-quadrado
q1 <- qchisq(.025, df = n-1)
q2 <- qchisq(.975, df = n-1)
## Intervalo de 95%
((n-1) * vx)/c(q2,q1)
## Estimativa pontual
vx
(vv <- (2*vx^2)/(n-1))
(2 * sigma^4)/(n - 1)

vx + 1.96 * epv * c(-1,1)

## Transformation invariance
sqrt(((n-1) * vx)/c(q2,q1))
sdx

int <- replicate(1000, expr = {
    x <- rnorm(n, mu, sigma)
    ((n-1) * var(x))/c(q2,q1)
})
int <- t(int)

apply(int, 1, function(x) int[x, 1] < 4 & int[x, 2] > 4)

sum(int[, 1] < 4 & int[, 2] > 4)
sum(int[, 1] < 4 & int[, 2] > 4)/1000

int2 <- replicate(1000, expr = {
    x <- rnorm(n, mu, sigma)
    var(x) + 1.96 * (var(x) * sqrt(2/(n-1))) * c(-1,1)
})
int2 <- t(int2)

sum(int[, 1] < 4 & int[, 2] > 4)/1000
sum(int2[, 1] < 4 & int2[, 2] > 4)/1000

par(mfrow = c(1, 2))
hist(int)
hist(int2)
par(mfrow = c(1, 1))

vmc <- replicate(1000, expr = {
    x <- rnorm(n, mu, sigma)
    var(x)
})
hist(vmc)
quantile(vmc, probs = c(.275, .975))
mean(vmc)

plot(ecdf(vmc))
curve(pchisq(x, n - 1), add = TRUE, col = 2)

hist(((n - 1) * vmc)/4, freq = F)
curve(dchisq(x, n - 1), add = TRUE, col = 2)

sdmc <- replicate(10000, expr = {
    x <- rnorm(1000, mu, sigma)
    sd(x)
})
hist(sdmc)
quantile(sdmc, probs = c(.275, .975))
mean(sdmc)
sd(sdmc)
sd(sdmc)/sqrt(1000)

## Transformation invariance
sqrt(((n-1) * vx)/c(q2,q1))
sdx
sqrt(vx + 1.96 * epv * c(-1,1))



MASS::fitdistr(x, "normal")



## ----- geral ---------------------------------------------------------

## Função densidade da normal
fn <- function(x, mu = 0, sigma = 1){
    (1/(sqrt(2*pi)*sigma)) * exp(-(1/2) * ((x-mu)/sigma)^2)
}
## Testes
fn(1)
dnorm(1)
integrate(fn, 0, 1)
pnorm(1) - pnorm(0)
integrate(fn, -Inf, Inf)

## Função para o primeiro momento da normal (esperança)
fn.mu <- function(x, mu = 0, sigma = 1){
    x * ((1/(sqrt(2*pi)*sigma)) * exp(-(1/2) * ((x-mu)/sigma)^2))
}
## Função para o segundo momento central da normal (variancia)
fn.mu2 <- function(x, mu = 0, sigma = 1){
    (x-mu)^2 * ((1/(sqrt(2*pi)*sigma)) * exp(-(1/2) * ((x-mu)/sigma)^2))
}
## Função para o quarto momento central da normal
fn.mu4 <- function(x, mu = 0, sigma = 1){
    (x-mu)^4 * ((1/(sqrt(2*pi)*sigma)) * exp(-(1/2) * ((x-mu)/sigma)^2))
}

## Média de X pela integral
integrate(fn.mu, lower = -Inf, upper = Inf,
          mu = mean(x), sigma = sd(x))
mean(x)
## Variancia de X pela integral
integrate(fn.mu2, lower = -Inf, upper = Inf,
          mu = mean(x), sigma = sd(x))
var(x)
## Calcula quarto momento central
(mu4 <- integrate(fn.mu4, lower = -Inf, upper = Inf,
          mu = mean(x), sigma = sd(x))$value)
## Deve ser o mesmo que 3\sigma^4
3*sd(x)^4

## Variancia de s^2 na forma geral
(Var.s2 <- (1/n) * (mu4 - (((n-3)/(n-1)) * sd(x)^4)))
## Erro-padrão de s^2
(EP.s2 <- sqrt(Var.s2))

## Variancia de s pelo método delta
(Var.s <- (1/(2*sd(x)))^2 * Var.s2)
## Erro padrão de s
(EP.s <- 1/(2*sd(x)) * EP.s2) # ou
sqrt((1/(2*sd(x)))^2 * Var.s2)

## Conferindo pela fitdistr
sd(x)
MASS::fitdistr(x, "Normal")
