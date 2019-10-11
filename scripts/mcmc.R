##======================================================================
## Sobre MC e MCMC
##======================================================================

##======================================================================
## Integracao de MC em mais de uma dimensao

## Integral simples em uma dimensão
g <- function(x) x^2
## \int^0_3 g(x) dx
integrate(g, 0, 3)
ux <- runif(1000, 0, 3)
3 * mean(g(ux))

## Integral em duas dimensões, usando integrais iteradas
g <- function(x, y) x^2 * y
## \int^0_3 \int^1_2 g(x,y) dy dx
integrate( function(y) {
    sapply(y, function(y) {
        integrate(function(x) g(x,y), 0, 3)$value
    })
}, 1, 2)

## Mesma integração por MC
ux <- runif(1e4, 0, 3)
uy <- runif(1e4, 1, 2)
## Aqui multiplica pela área do retângulo formado pelos limites
((3 - 0) * (2 - 1)) * mean(g(ux, uy))

##----------------------------------------------------------------------
## Tentativa de integrar uma NMV

sigma <- matrix(c(1, .5, .5, 1), ncol = 2)
mu <- c(0, 0)
y <- mvtnorm::rmvnorm(1000, mu, sigma)
plot(y)
mean(y[,1] < 1 & y[,2] < 1)

ym <- MASS::mvrnorm(1000, mu, sigma)
plot(ym)
mean(ym[,1] < 1 & ym[,2] < 1)

mvtnorm::dmvnorm(c(1,1), mu, sigma)

pracma::integral2(function(x, y) mvtnorm::dmvnorm(c(x,y), mu, diag(sigma)),
                  -10, 1, -10, 1)

pracma::integral2(function(x, y) mvtnorm::dmvnorm(x, mu, sigma),
                  -10, 1, -10, 1)

integrate(function(x) dnorm(x, 0, 1), -1, 1)
integrate(function(x) dnorm(x, 10, 1), 9, 11)

integrate(function(y) y, 1, 2)
