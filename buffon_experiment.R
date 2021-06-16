##======================================================================
## Script com aplicações dos conceitos da aula 2. Estimativas de pi por
## simulação e simulação estocástica.
## Autor: Fernando Mayer
##======================================================================

##======================================================================
## Agulha de Buffon
##======================================================================

##----------------------------------------------------------------------
## Definindo as variaveis envolvidas
##----------------------------------------------------------------------

## l = comprimento da agulha
## a = distancia entre as paralelas
## D = distancia do centro da agulha até a paralela mais proxima. Fica
## no intevalo D \in [0, a/2]
## d = distancia do centro ate a ponta da agulha. Define se a agulha
## toca ou nao na paralela (se D <= d)
## \theta = angulo formado entre a linha a linha que passa pelo centro
## da agulha e sua posicao. Fica no intervalo \theta \in [0, pi]

## Por trigonometria,
## d = (l/2) * \sin(\theta)

##----------------------------------------------------------------------
## Visualizacao grafica
##----------------------------------------------------------------------

# define as variaveis
a <- 1
l <- 1
Dist <- seq(0, a/2, length = 100)       # Dist = D
theta <- seq(0, pi, length = 100)
# calcula o sub-espaco
d <- (l/2) * sin(theta)
# grafico - qualquer ponto dentro da area sombreada significa que a
# agulha tocou uma paralela
plot(theta, d, type = "l", xaxs = "i", yaxs = "i",
     xlab = expression(theta),
     ylab = expression(d == (l/2) %*% sin(theta)),
     axes = FALSE)
polygon(theta, d, density = 10)
axis(1, at = round(seq(0, pi, length = 10), 2),
     labels = round(seq(0, pi, length = 10), 2))
axis(2, at = round(seq(0, a/2, length = 6), 1),
     labels = round(seq(0, a/2, length = 6), 1))
box()

## Grafico 3D
# gera a matriz com todos os valores possiveis para a amplitude de D e
# \theta
mat <- matrix(Dist %*% t(sin(theta)), nrow = length(Dist),
              ncol = length(theta))
# para fazer o wireframe eh mais facil criar um dataframe para usar no
# formato de formula
df <- expand.grid(Dist=Dist, theta=theta)
# aloca os valores calculados de d da matriz
df$d <- as.vector(mat)
# wireframe
require(lattice)
wireframe(d ~ Dist + theta, data = df,
          scales = list(arrows = FALSE,
          x = list(tick.number = 5),
          y = list(tick.number = 5),
          z = list(tick.number = 5)),
          xlab = "D", ylab = expression(theta),
          zlab = list(expression(d == D %*% sin(theta)), rot=90))

##----------------------------------------------------------------------
## Simulacao do experimento de Buffon
##----------------------------------------------------------------------

## Numero de repeticoes
n <- 1000

## D ~ U[0, a/2]
D.random <- runif(n, 0, a/2)

## \theta ~ U[0, pi]
theta.random <- runif(n, 0, pi)

## d = (l/2) * \sin(\theta)
d <- (l/2) * sin(theta.random)

## var. aleatoria H, onde
## H = 1, se D <=d
## H = 0, c.c.
H <- numeric(n)
H[D.random <= d] <- 1

## h = numero de sucessos
h <- sum(H)

## \pi_{estimado} ~ (2ln)/(ah)
pi.est <- (2*l*n)/(a*h)

##----------------------------------------------------------------------
## Funcao
##----------------------------------------------------------------------

source("buffon.R")

## Testes
seq.teste1 <- 1:1e+5                     # approx. 30 min.
system.time(
            teste1 <- buffon(n = seq.teste1)
            )

seq.teste2 <- seq(0, 1e+6, 1000)[-1]    # approx. 3 min.
system.time(
            teste2 <- buffon(n = seq.teste2)
            )

## Funcao para plot
source("plot.buffon.R")

plot(teste1)
plot(teste2)

##----------------------------------------------------------------------

##======================================================================
## Estimativa de pi pelo metodo de circulo/quadrado
##======================================================================

## Define o circulo.
## Veja
browseURL("http://en.wikipedia.org/wiki/Circle")
browseURL("http://pt.wikipedia.org/wiki/Pseudo-aleatoriedade")
browseURL("http://pt.wikipedia.org/wiki/Pi")
browseURL("http://en.wikipedia.org/wiki/Approximations_of_%CF%80")

## Cria a representacao grafica do experimento
##----------------------------------------------------------------------

# eixo x = a + cos \theta * raio
circx <- cos(seq(0, 2*pi, .01)) * 1
# eixo y = b + sin \theta * raio
circy <- sin(seq(0, 2*pi, .01)) * 1
# (a,b) eh o ponto de origem, aqui (0,0)

# grafico
plot(circx, circy, type = "l", xlim = c(-1,1), ylim = c(-1,1),
     xaxs = "i", yaxs = "i", asp = 1, xlab = "Eixo X", ylab = "Eixo Y")
abline(v = c(-1,1), col = 2)
abline(v = c(0,0), col = 2)
segments(-1, -1, 1, -1, col = 2)
segments(-1, 1, 1, 1, col = 2)
segments(-1, 0, 1, 0, col = 2)
points(0, 0, pch = 19, col = 2)

# gera pontos aleatorios uniformes U[0,1]
N <- 1000
px <- runif(N, -1, 1)
py <- runif(N, -1, 1)
points(px, py, pch = 20)

r2 <- px^2 + py^2
r <- sqrt(r2)

points(px[r <= 1], py[r <= 1], pch = 20, col = 3)
points(px[r > 1], py[r > 1], pch = 20, col = 2)

## Estima o pi
# x^2 + y^2 = r^2 => r = sqrt(x^2 + y^2)
# com r = 1 basta verificar os pontos que estao a uma distancia < 1 para
# considerar inclusos dentro do circulo. A formula geral eh a divisao
# dos pontos que cairam dento do circulo (n) pelo total de pontos (N). A
# multiplicacao por 4 eh para considerar os 4 quadrados que compoem a
# figura: 4 * (n/N)
4*(sum(sqrt(px^2 + py^2) <= 1)/N)

## Cria uma funcao
##----------------------------------------------------------------------

circ.quad <- function(N, radius = 1){
    px <- runif(N, -radius, radius)
    py <- runif(N, -radius, radius)
    pi_est <- 4*(sum(sqrt(px^2 + py^2) <= radius)/N)
    return(pi_est)
}

## Teste de funcionamento
N <- c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7)
cq <- sapply(N, circ.quad)

## plot
plot(N, cq, type = "l")
abline(h = pi, col = "lightgrey")


##======================================================================
## Estimativa de pi pelo metodo de circulo/quadrado
##======================================================================

## Define o circulo.
## Veja
browseURL("http://en.wikipedia.org/wiki/Circle")
browseURL("http://pt.wikipedia.org/wiki/Pseudo-aleatoriedade")
browseURL("http://pt.wikipedia.org/wiki/Pi")
browseURL("http://en.wikipedia.org/wiki/Approximations_of_%CF%80")

## Cria a representacao grafica do experimento
##----------------------------------------------------------------------

# eixo x = a + cos \theta * raio
circx <- cos(seq(0, 2*pi, .01)) * 1
# eixo y = b + sin \theta * raio
circy <- sin(seq(0, 2*pi, .01)) * 1
# (a,b) eh o ponto de origem, aqui (0,0)

# grafico
plot(circx, circy, type = "l", xlim = c(-1,1), ylim = c(-1,1),
     xaxs = "i", yaxs = "i", asp = 1, xlab = "Eixo X", ylab = "Eixo Y")
abline(v = c(-1,1), col = 2)
abline(v = c(0,0), col = 2)
segments(-1, -1, 1, -1, col = 2)
segments(-1, 1, 1, 1, col = 2)
segments(-1, 0, 1, 0, col = 2)
points(0, 0, pch = 19, col = 2)

# gera pontos aleatorios uniformes U[0,1]
N <- 1000
px <- runif(N, -1, 1)
py <- runif(N, -1, 1)
points(px, py, pch = 20)

r2 <- px^2 + py^2
r <- sqrt(r2)

points(px[r <= 1], py[r <= 1], pch = 20, col = 3)
points(px[r > 1], py[r > 1], pch = 20, col = 2)

## Estima o pi
# x^2 + y^2 = r^2 => r = sqrt(x^2 + y^2)
# com r = 1 basta verificar os pontos que estao a uma distancia < 1 para
# considerar inclusos dentro do circulo. A formula geral eh a divisao
# dos pontos que cairam dento do circulo (n) pelo total de pontos (N). A
# multiplicacao por 4 eh para considerar os 4 quadrados que compoem a
# figura: 4 * (n/N)
4*(sum(sqrt(px^2 + py^2) <= 1)/N)

## Cria uma funcao
##----------------------------------------------------------------------

circ.quad <- function(N, radius = 1){
    px <- runif(N, -radius, radius)
    py <- runif(N, -radius, radius)
    pi_est <- 4*(sum(sqrt(px^2 + py^2) <= radius)/N)
    return(pi_est)
}

## Teste de funcionamento
N <- c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7)
cq <- sapply(N, circ.quad)

## plot
plot(N, cq, type = "l")
abline(h = pi, col = "lightgrey")

##======================================================================
## 2021

curve(1/2 * sin(x), 0, pi)
## Tem integral 1?
integrate(function(x) 1/2 * sin(x), lower = 0, upper = pi)

## D ~ U(0, 1/2)
## Theta ~ U(0 , pi)

## f(x) = 1/2 sin(Theta)
## g(x) ~ U(0, 1/2) ou direto g(x) ~ U(0, pi)

curve(1/2 * sin(x), 0, pi, col = 4)
curve(1/2 + 0 * x, add = TRUE, lty = 2, lwd = 2)
legend("right", legend = c("f(x)", "g(x)"),
       lty = c(1, 2), col = c(4, 1), lwd = c(1, 2), bty = "n")

## Criando os elementos necessários.
f <- function(x) 1/2 * sin(x)
g <- function(x) 1/2 + 0 * x

## Simula de uma única vez, com um valor fixo de simulações
Nsim <- 10000
set.seed(1)
## Amostra da proposta
y <- runif(Nsim, 0, pi)
## Amostra da U(0,1)
u <- runif(Nsim, 0, 1)
## Calcula a razão
r <- f(y)/(g(y))
## x será um vetor com os valores de y onde u < r
x <- y[u < r]
## Valores de u aceitos (apenas para o grafico)
ua <- u[u < r]
## Valores de u rejeitados (apenas para o grafico)
ur <- u[u >= r]

curve(f, 0, pi, col = 4)
curve(g, add = TRUE, lty = 2, lwd = 2)
points(x, ua * g(x), col = 3)
points(y[u >= r], ur * g(y[u >= r]), col = 2)

## Taxa de aceitacao
2/pi

## pi
(2 * Nsim)/length(ua)
