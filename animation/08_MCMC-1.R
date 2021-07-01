## ---- cache=FALSE, include=FALSE----------------------------------------------
source("setup_knitr.R")
opts_chunk$set(fig.path = "figures/08_MCMC-1/")


## ---- fig.show='hold'---------------------------------------------------------
## Simulação de um random walk
rw1 <- function(T, x1, seed) {
    x <- numeric(T)
    x[1] <- x1
    set.seed(seed)
    e <- rnorm(T)
    for(t in 1:(T - 1)) {
        x[t + 1] <- x[t] + e[t]
    }
    return(x)
}
par(mfrow = c(2, 1))
plot(rw1(T = 100, x1 = 10, seed = 1), type = "l",
     xlab = "Tempo", ylab = "x")
plot(rw1(T = 1000, x1 = 10, seed = 1), type = "l",
     xlab = "Tempo", ylab = "x")
par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
f <- function(x) dnorm(x, 0, 1)
delta <- 0.5
N <- 500
x <- numeric(N)
x[1] <- 0
set.seed(2019-10-11)
for(i in 2:N) {
    z <- runif(1, -delta, delta)
    y <- x[i - 1] + z
    alpha <- min(f(y)/f(x[i - 1]), 1)
    u <- runif(1)
    if(u <= alpha) {
        x[i] <- y
    } else {
        x[i] <- x[i - 1]
    }
}
plot(x, type = "l")


## -----------------------------------------------------------------------------
f <- function(x) dnorm(x, 0, 1)
delta <- 2
N <- 500
x2 <- numeric(N)
x2[1] <- 0
set.seed(2019-10-11)
for(i in 2:N) {
    z <- runif(1, -delta, delta)
    y <- x2[i - 1] + z
    alpha <- min(f(y)/f(x2[i - 1]), 1)
    u <- runif(1)
    if(u <= alpha) {
        x2[i] <- y
    } else {
        x2[i] <- x2[i - 1]
    }
}
plot(x2, type = "l")


## ---- fig.show='hold'---------------------------------------------------------
par(mfrow = c(1, 2))
plot(ecdf(x))
curve(pnorm(x), add = TRUE, col = 2)
plot(ecdf(x2))
curve(pnorm(x), add = TRUE, col = 2)
par(mfrow = c(1, 1))


## ---- fig.show='hold'---------------------------------------------------------
par(mfrow = c(2, 1))
plot(x, type = "l", main = expression(delta == 0.5))
plot(x2, type = "l", main = expression(delta == 2))
par(mfrow = c(1, 1))


## ---- fig.show='hold'---------------------------------------------------------
par(mfrow = c(1, 2))
acf(x, lag.max = 50)
acf(x2, lag.max = 50)
par(mfrow = c(1, 1))


## ---- include=FALSE-----------------------------------------------------------
## Ilustrando o procedimento (com um exemplo bem simples). Obter
## realizações de uma distribuição Normal(0, 1). Definir a distribuição
## candidata qX(x,.) como sendo a uniforme(-delta, delta) que é
## simétrica.

qX <- function(delta, xi){
    ## delta e xi: escalares parâmetros da distribuição candidata.
    ## A distribuição candidata é a uniforme.
    ## Retorna uma realização da distribuição candidata.
    runif(1, xi-delta, xi+delta)
}
rwsampler1 <- function(nsim, x1, delta, mu, sigma,
                       plot=FALSE, go=c("click","enter","none")){
    out <- vector(mode="numeric", length=nsim)
    out[1] <- x1
    for(i in 2:nsim){
        ## Realização da distribuição alvo.
        if(plot & go[1]=="click"){
            can <- locator(n=1)$x
        } else {
            can <- qX(delta, xi=out[i-1])
        }
        dn1 <- dnorm(can, mu, sigma)
        dn0 <- dnorm(out[i-1], mu, sigma)
        ratio <- dn1/dn0
        u <- runif(1)
        if(u<ratio) out[i] <- can else out[i] <- out[i-1]
        if(plot & nsim<=20){
            curve(dnorm(x, mu, sigma), mu-4*sigma, mu+4*sigma,
                  ylab="densidade")
            curve(dunif(x, out[i-1]-delta, out[i-1]+delta), add=TRUE, lty=2)
            du <- dunif(can, out[i-1]-delta, out[i-1]+delta)
            ## segments(can, du, can, 0, col=4)
            segments(can, dn1, can, 0, col=2);
            segments(out[i-1], dn0, out[i-1], 0, col=4);
            cex <- 2.5; col="yellow"
            points(can, dn1, pch=19, cex=cex, col="green");
            points(out[i-1], dn0, pch=19, cex=cex, col=col);
            ## points(can, dn1, pch="N");
            ## points(out[i-1], dn0, pch="n");
            text(can, dn1, expression(f[X]));
            text(out[i-1], dn0, expression(f[X]));
            ex <- substitute(frac(f[X](x[i]),
                                  f[X](x[i-1]))*" = "*
                             frac(dn1, dn0)==ratio,
                             list(dn1=dn1, dn0=dn0, ratio=ratio))
            r <- substitute("u = "~u<ratio,
                            list(ratio=ratio, u=u))
            mtext(ex, side=3, line=1, adj=0)
            mtext(r, side=3, line=2, adj=1)
            mtext(sprintf("então %s", ifelse(u<ratio, "aceita", "rejeita")),
                  side=3, line=1, adj=1)
            switch(go[1],
                   click=locator(n=1),
                   console=readline(prompt="Press [enter] to continue"),
                   none=Sys.sleep(0.5))
        }
    }
    return(out)
}


## ---- include=FALSE, eval=FALSE-----------------------------------------------
## mu <- 0; sigma <- 1
## x <- rwsampler1(nsim=10, x1=-1, delta= .5, mu, sigma, plot=TRUE,
##                 go="console")
## ## x <- rwsampler1(nsim=10, x1=-1, delta= .5, mu, sigma, plot=TRUE,
## ##                 go="click")
## x <- rwsampler1(nsim=10, x1=-1, delta=2, mu, sigma, plot=TRUE,
##                 go="console")
## ## x <- rwsampler1(nsim=10, x1=-1, delta=2, mu, sigma, plot=TRUE,
## ##                 go="click")


## ---- include=FALSE, results='hide', cache=TRUE-------------------------------
animation::saveHTML(
               rwsampler1(nsim = 20, x1 = -1, delta = 2, mu = 0,
                          sigma = 1, plot = TRUE, go = "none"),
               img.name = "rwsampler1-",
               imgdir = "figures/rwsampler1/",
               htmlfile = "rwsampler1.html",
               autobrowse = FALSE,
               verbose = FALSE,
               ani.width = 600,
               ani.height = 600)


## ---- echo=FALSE, results='asis', out.extra='style="display:block; margin: auto;" frameborder="0"'----
knitr::include_url("rwsampler1.html", height = "715px")


## ---- eval=FALSE, include=FALSE-----------------------------------------------
## ## Simular de uma mistura de normais. Normais com variância 1 e mistura
## ## 1:1.
## k <- 0.5
## curve(k*dnorm(x, 0, 1)+(1-k)*dnorm(x, 7, 1), -3, 10)
## curve(0.1*dunif(x), add=TRUE, col=2, n=1024)
## 
## rwsampler2 <- function(nsim, x1, delta,
##                        plot=FALSE, go=c("click","enter","none")){
##     out <- vector(mode="numeric", length=nsim)
##     out[1] <- x1
##     for(i in 2:nsim){
##         ## Realização da distribuição alvo.
##         if(plot & go[1]=="click"){
##             can <- locator(n=1)$x
##         } else {
##             can <- qX(delta, xi=out[i-1])
##         }
##         dn1 <- k*dnorm(can, 0, 1)+(1-k)*dnorm(can, 7, 1)
##         dn0 <- k*dnorm(out[i-1], 0, 1)+(1-k)*dnorm(out[i-1], 7, 1)
##         ratio <- dn1/dn0
##         u <- runif(1)
##         if(u<ratio) out[i] <- can else out[i] <- out[i-1]
##         if(plot & nsim<=20){
##             curve(k*dnorm(x, 0, 1)+(1-k)*dnorm(x, 7, 1), -3, 10,
##                   ylab="densidade")
##             curve(0.3*dunif(x, out[i-1]-delta, out[i-1]+delta),
##                   add=TRUE, lty=2)
##             du <- dunif(can, out[i-1]-delta, out[i-1]+delta)
##             ## segments(can, du, can, 0, col=4)
##             segments(can, dn1, can, 0, col=2);
##             segments(out[i-1], dn0, out[i-1], 0, col=4);
##             cex <- 2.5; col="yellow"
##             points(can, dn1, pch=19, cex=cex, col="green");
##             points(out[i-1], dn0, pch=19, cex=cex, col=col);
##             ## points(can, dn1, pch="N");
##             ## points(out[i-1], dn0, pch="n");
##             text(can, dn1, expression(f[X]));
##             text(out[i-1], dn0, expression(f[X]));
##             ex <- substitute(frac(f[X](x[i]),
##                                   f[X](x[i-1]))*" = "*
##                              frac(dn1, dn0)==ratio,
##                              list(dn1=dn1, dn0=dn0, ratio=ratio))
##             r <- substitute("u = "~u<ratio,
##                             list(ratio=ratio, u=u))
##             mtext(ex, side=3, line=1, adj=0)
##             mtext(r, side=3, line=2, adj=1)
##             mtext(sprintf("então %s", ifelse(u<ratio, "aceita", "rejeita")),
##                   side=3, line=1, adj=1)
##             switch(go[1],
##                    click=locator(n=1),
##                    console=readline(prompt="Press [enter] to continue"),
##                    none=Sys.sleep(0.5))
##         }
##     }
##     return(out)
## }
## 
## x <- rwsampler2(nsim=20, x1=1, delta=2, plot=TRUE, go="console")
## x <- rwsampler2(nsim=20, x1=1, delta=1, plot=TRUE, go="console")
## x <- rwsampler2(nsim=20, x1=1, delta=4, plot=TRUE, go="console")
## 
## ##----------------------------------------------------------------------
## ## Muitos valores.
## 
## ## Janela estreita -----------------------------------------------------
## set.seed(123)
## x <- rwsampler2(nsim=20000, x1=1, delta=1, plot=FALSE)
## 
## par(mfrow=c(2,2))
## plot(x, type="l")        ## Traço da cadeia completa.
## plot(x[1:100], type="l") ## Traço do começo da cadeia.
## acf(x)                   ## Mostra que a cadeia não é independente.
## plot(ecdf(x))            ## Acumulada teórica vs empírica.
## curve(k*pnorm(x, 0, 1)+(1-k)*pnorm(x, 7, 1), add=TRUE, col=2); layout(1)
## prop.table(table(x<3.5))
## 
## ## Janela larga --------------------------------------------------------
## set.seed(123)
## x <- rwsampler2(nsim=20000, x1=1, delta=4, plot=FALSE)
## 
## par(mfrow=c(2,2))
## plot(x, type="l")        ## Traço da cadeia completa.
## plot(x[1:100], type="l") ## Traço do começo da cadeia.
## acf(x)                   ## Mostra que a cadeia não é independente.
## plot(ecdf(x))            ## Acumulada teórica vs empírica.
## curve(k*pnorm(x, 0, 1)+(1-k)*pnorm(x, 7, 1), add=TRUE, col=2); layout(1)
## prop.table(table(x<3.5))


## -----------------------------------------------------------------------------
rw.Metropolis <- function(nu, sigma, x0, N) {
    f <- function(x, nu) dt(x, nu)
    x <- numeric(N)
    x[1] <- x0
    u <- runif(N)
    for(i in 2:N) {
        z <- rnorm(1, mean = 0, sd = sigma)
        y <- x[i - 1] + z
        alpha <- min(f(y, nu)/f(x[i - 1], nu), 1)
        u <- runif(1)
        if(u <= alpha) {
            x[i] <- y
        } else {
            x[i] <- x[i - 1]
        }
    }
    return(x)
}


## ---- fig.show='hold'---------------------------------------------------------
nu <- 4
N <- 2000
sigma <- c(.05, .5, 2,  16)
x0 <- 25
rw1 <- rw.Metropolis(n, sigma[1], x0, N)
rw2 <- rw.Metropolis(n, sigma[2], x0, N)
rw3 <- rw.Metropolis(n, sigma[3], x0, N)
rw4 <- rw.Metropolis(n, sigma[4], x0, N)
## Resultado das cadeias
par(mfrow = c(2, 2))
refline <- qt(c(.025, .975), df = n)
rw <- cbind(rw1, rw2, rw3,  rw4)
for (j in 1:4) {
    plot(rw[, j], type = "l",
         main = bquote(sigma == .(round(sigma[j], 3))),
         ylab = "X", ylim = range(rw[, j]))
    abline(h = refline)
}
par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
## Gerar números de uma distribuição Beta usando a distribuição Uniforme
## e/ou normal.

## Distribuição alvo: X ~ Beta(2, 3)
f <- function(x) dbeta(x, shape1 = 2, shape2 = 3)
curve(f, 0, 1)
## Distribuição candidata (proposal): X ~ Uniforme(0,1)
g <- function(x) dunif(x, 0, 1)

## Gráfico das densidados sobrepostas.
curve(f, 0, 1)
curve(g, add=TRUE, col=2)
legend("topright", legend=c("Alvo", "Candidata"), lty=1, col=1:2,
       bty="n")

N <- 500
x <- numeric(N)
x[1] <- 0.5
set.seed(2019-10-11)
for(i in 2:N) {
    y <- runif(1) # Distribuição proposta
    alpha <- min((f(y) * g(x[i - 1])) / (f(x[i - 1]) * g(y)), 1)
    u <- runif(1)
    if(u <= alpha) {
        x[i] <- y
    } else {
        x[i] <- x[i - 1]
    }
}

## Cadeia
plot(x, type = "l")

## Compara com teorica
plot(ecdf(x))
curve(pbeta(x, 2, 3), add = TRUE, col = 2)


## ---- include=FALSE-----------------------------------------------------------
## Alvo: Beta.
## Canditada: Uniforme.
iidsampler1 <- function(nsim, x1, plot=FALSE,
                        go=c("click","enter","none")){
    out <- vector(mode="numeric", length=nsim)
    ## Valor para iniciar a cadeia.
    out[1] <- x1
    for(i in 2:nsim){
        ## Realização da distribuição alvo.
        if(plot & go[1]=="click"){
            y <- locator(n=1)$x
        } else {
            y <- runif(1)
        }
        ## Cálculo da razão de aceitação.
        dg1 <- dbeta(y, 2, 3)
        dn1 <- dunif(y)
        dg0 <- dbeta(out[i-1], 2, 3)
        dn0 <- dunif(out[i-1])
        ratio <- (dg1/dg0)/(dn1/dn0)
        u <- runif(1)
        if(u<ratio){
            ## Se sim, cadeia ganha novo valor.
            out[i] <- y
        } else {
            ## Se não, cadeia recebe o último.
            out[i] <- out[i-1]
        }
        ## Parte de representação gráfica do método.
        if(plot & nsim<=20){
            ## Curvas.
            curve(dbeta(x, 2, 3), 0, 1, xlim=c(0, 1),
                  ylab="densidade");
            curve(dunif(x), add=TRUE, lty=2);
            ## Lengendas.
            legend("topright",
                   legend=c(expression(f[X]*" ~ Beta"),
                       expression(f[Y]*" ~ Unif")),
                   lty=c(1,2), bty="n")
            legend("right",
                   legend=c(expression("Candidato em"*~i),
                       expression("Valor em"*~i-1)),
                   lty=1, col=c(2,4), bty="n")
            ## Segmentos da base até os valores nas funções.
            segments(y, dg1, y, 0, col=2, lty=1);
            segments(y, dn1, y, 0, col=2, lty=1);
            segments(out[i-1], dg0, out[i-1], 0, col=4, lty=1);
            segments(out[i-1], dn0, out[i-1], 0, col=4, lty=1);
            ## Pontos sobre as funções.
            cex <- 2.5; col="yellow"
            points(y, dg1, pch=19, cex=cex, col="green");
            points(y, dn1, pch=19, cex=cex, col=col);
            points(out[i-1], dg0, pch=19, cex=cex, col="green");
            points(out[i-1], dn0, pch=19, cex=cex, col=col);
            ## Rótulos dos pontos.
            text(y, dg1, labels=expression(f[X]));
            text(y, dn1, labels=expression(f[Y]));
            text(out[i-1], dg0, expression(f[X]));
            text(out[i-1], dn0, expression(f[Y]));
            text(c(y, out[i-1]), 0,
                 labels=c(expression(x[i]), expression(x[i-1])),
                 pos=4)
            ## Anotações matemáticas.
            L <- list(dg1=dg1, dg0=dg0, dn1=dn1,
                      dn0=dn0, num=dg1/dg0, den=dn1/dn0,
                      ratio=ratio)
            L <- lapply(L, round, digits=3)
            ex <- substitute(frac(f[X](x[i]), f[X](x[i-1]))/
                             frac(f[Y](x[i]), f[Y](x[i-1]))*" = "*
                             frac(dg1, dg0)/frac(dn1, dn0)*" = "*
                             num/den==ratio, L)
            r <- substitute("u = "~u<ratio,
                            lapply(list(ratio=ratio, u=u),
                                   round, digits=3))
            mtext(ex, side=3, line=1, adj=0)
            mtext(r, side=3, line=2, adj=1)
            mtext(ifelse(u<ratio,
                         expression(Aceita~x[i]),
                         expression(Repete~x[i-1])),
                  side=3, line=1, adj=1)
            switch(go[1],
                   ## Avança por cliques do mouse.
                   click=locator(n=1),
                   ## Avança por enter no console.
                   console=readline(prompt="Press [enter] to continue"),
                   ## Avança com intervalo de tempo entre etapas.
                   none=Sys.sleep(0.5))
        }
    }
    return(out)
}


## ---- include=FALSE, eval=FALSE-----------------------------------------------
## n <- 10
## x <- iidsampler1(n, x1=0.5, plot=TRUE, go="console")
## ## Gerando muitos números pelo método.
## x <- iidsampler1(5000, x1=0.5)
## par(mfrow=c(2,2))
## plot(x, type="l")        ## Traço da cadeia completa.
## plot(x[1:100], type="l") ## Traço do começo da cadeia.
## acf(x)                   ## Mostra que a cadeia não é independente.
## plot(ecdf(x))            ## Acumulada teórica vs empírica.
## curve(pbeta(x, 2, 3), add=TRUE, col=2); layout(1)


## ---- include=FALSE, results='hide', cache=TRUE-------------------------------
animation::saveHTML(
               iidsampler1(n = 20, x1 = 0.5, plot = TRUE, go = "none"),
               img.name = "iidsampler1-",
               imgdir = "figures/iidsampler1/",
               htmlfile = "iidsampler1.html",
               autobrowse = FALSE,
               verbose = FALSE,
               ani.width = 600,
               ani.height = 600)


## ---- echo=FALSE, results='asis', out.extra='style="display:block; margin: auto;" frameborder="0"'----
knitr::include_url("iidsampler1.html", height = "715px")


## -----------------------------------------------------------------------------
## Distribuição alvo: X ~ Beta(2, 3)
f <- function(x) dbeta(x, shape1 = 2, shape2 = 3)
curve(f, 0, 1)
## Distribuição candidata (proposal): X ~ Normal(0.5, 0.25)
g <- function(x) dnorm(x, 0.5, 0.25)

## Gráfico das densidados sobrepostas.
curve(f, 0, 1)
curve(g, add=TRUE, col=2)
legend("topright", legend=c("Alvo", "Candidata"), lty=1, col=1:2,
       bty="n")

N <- 500
x2 <- numeric(N)
x2[1] <- 0.5
set.seed(2019-10-11)
for(i in 2:N) {
    y <- rnorm(1, 0.5, 0.25) # Distribuição proposta
    alpha <- min((f(y) * g(x2[i - 1])) / (f(x2[i - 1]) * g(y)), 1)
    u <- runif(1)
    if(u <= alpha) {
        x2[i] <- y
    } else {
        x2[i] <- x2[i - 1]
    }
}

## Cadeia
plot(x2, type = "l")

## Compara com teorica
plot(ecdf(x2))
curve(pbeta(x, 2, 3), add = TRUE, col = 2)


## ---- include=FALSE-----------------------------------------------------------
## Alvo: Beta.
## Canditada: Normal.
## curve(dbeta(x, 2, 3), 0, 1)
## curve(dnorm(x, 0.5, 0.25), add=TRUE, lty=2)
iidsampler2 <- function(nsim, x1, plot=FALSE,
                        go=c("click","console","none")){
    out <- vector(mode="numeric", length=nsim)
    out[1] <- x1
    for(i in 2:nsim){
        ## Realização da distribuição alvo.
        if(plot & go[1]=="click"){
            y <- locator(n=1)$x
        } else {
            y <- rnorm(1, 0.5, 0.25)
        }
        ## Cálculo da razão de aceitação.
        dg1 <- dbeta(y, 2, 3)
        dn1 <- dnorm(y, 0.5, 0.25)
        dg0 <- dbeta(out[i-1], 2, 3)
        dn0 <- dnorm(out[i-1], 0.5, 0.25)
        ratio <- (dg1/dg0)/(dn1/dn0)
        u <- runif(1)
        if(u<ratio){
            out[i] <- y
        } else {
            out[i] <- out[i-1]
        }
        ## Incluir contador da aceitação.
        if(plot & nsim<=20){
            ## Curvas.
            curve(dbeta(x, 2, 3), 0, 1, xlim=c(0, 1),
                  ylab="densidade");
            curve(dnorm(x, 0.5, 0.25), add=TRUE, lty=2);
            ## Lengendas.
            legend("topright",
                   legend=c(expression(f[X]*" ~ Beta"),
                       expression(f[Y]*" ~ Normal")),
                   lty=c(1,2), bty="n")
            legend("right",
                   legend=c(expression("Candidato em"*~i),
                       expression("Valor em"*~i-1)),
                   lty=1, col=c(2,4), bty="n")
            ## Segmentos da base até os valores nas funções.
            segments(y, dg1, y, 0, col=2, lty=1);
            segments(y, dn1, y, 0, col=2, lty=1);
            segments(out[i-1], dg0, out[i-1], 0, col=4, lty=1);
            segments(out[i-1], dn0, out[i-1], 0, col=4, lty=1);
            ## Pontos sobre as funções.
            cex <- 2.5; col="yellow"
            points(y, dg1, pch=19, cex=cex, col="green");
            points(y, dn1, pch=19, cex=cex, col=col);
            points(out[i-1], dg0, pch=19, cex=cex, col="green");
            points(out[i-1], dn0, pch=19, cex=cex, col=col);
            ## Rótulos dos pontos.
            text(y, dg1, labels=expression(f[X]));
            text(y, dn1, labels=expression(f[Y]));
            text(out[i-1], dg0, expression(f[X]));
            text(out[i-1], dn0, expression(f[Y]));
            text(c(y, out[i-1]), 0,
                 labels=c(expression(x[i]), expression(x[i-1])),
                 pos=4)
            ## Expressões matemáticas.
            L <- list(dg1=dg1, dg0=dg0, dn1=dn1,
                      dn0=dn0, num=dg1/dg0, den=dn1/dn0,
                      ratio=ratio)
            L <- lapply(L, round, digits=3)
            ex <- substitute(frac(f[X](x[i]), f[X](x[i-1]))/
                             frac(f[Y](x[i]), f[Y](x[i-1]))*" = "*
                             frac(dg1, dg0)/frac(dn1, dn0)*" = "*
                             num/den==ratio, L)
            r <- substitute("u = "~u<ratio,
                             lapply(list(ratio=ratio, u=u),
                                    round, digits=3))
            mtext(ex, side=3, line=1, adj=0)
            mtext(r, side=3, line=2, adj=1)
            mtext(ifelse(u<ratio,
                         expression(Aceita~x[i]),
                         expression(Repete~x[i-1])),
                  side=3, line=1, adj=1)
            switch(go[1],
                   click=locator(n=1),
                   console=readline(prompt="Press [enter] to continue"),
                   none=Sys.sleep(0.5))
        }
    }
    return(out)
}


## ---- eval=FALSE, include=FALSE-----------------------------------------------
## n <- 10
## x <- iidsampler2(n, x1=0.5, plot=TRUE, go="console")
## ## Gerando muitos números pelo método.
## x <- iidsampler2(5000, x1=0.5)
## par(mfrow=c(2,2))
## plot(x, type="l")        ## Traço da cadeia completa.
## plot(x[1:100], type="l") ## Traço do começo da cadeia.
## acf(x)                   ## Mostra que a cadeia não é independente.
## plot(ecdf(x))            ## Acumulada teórica vs empírica.
## curve(pbeta(x, 2, 3), add=TRUE, col=2); layout(1)


## ---- include=FALSE, results='hide', cache=TRUE-------------------------------
animation::saveHTML(
               iidsampler2(n = 20, x1 = 0.5, plot = TRUE, go = "none"),
               img.name = "iidsampler2-",
               imgdir = "figures/iidsampler2/",
               htmlfile = "iidsampler2.html",
               autobrowse = FALSE,
               verbose = FALSE,
               ani.width = 600,
               ani.height = 600)


## ---- echo=FALSE, results='asis', out.extra='style="display:block; margin: auto;" frameborder="0"'----
knitr::include_url("iidsampler2.html", height = "715px")


## ---- fig.show='hold'---------------------------------------------------------
par(mfrow = c(2, 2))
plot(x, type = "l")
plot(x2, type = "l")
acf(x)
acf(x2)
par(mfrow = c(1, 1))


## ---- eval=FALSE, include=FALSE-----------------------------------------------
## ## Alvo: Gama.
## ## Canditada: Normal.
## curve(dgamma(x, 2, 1), -1.5, 8)
## curve(dnorm(x, 2, sqrt(2)), add=TRUE, lty=2)
## 
## iidsampler3 <- function(nsim, x1, alpha=2, beta=1, mu=NULL, sig=NULL,
##                         plot=FALSE, go=c("click","enter","none")){
##     out <- vector(mode="numeric", length=nsim)
##     ## Esperança da distribuição proposta.
##     out[1] <- x1
##     ## Esperança da distribuição proposta.
##     if(is.null(mu)){
##         mu <- alpha/beta
##     }
##     ## Variância da distribuição proposta.
##     if(is.null(sig)){
##         sig <- sqrt(alpha/(beta^2))
##     }
##     for(i in 2:nsim){
##         ## Realização da distribuição alvo.
##         if(plot & go[1]=="click"){
##             y <- locator(n=1)$x
##         } else {
##             y <- rnorm(1, mu, sig)
##         }
##         ## Cálculo da razão de aceitação.
##         dg1 <- dgamma(y, alpha, beta)
##         dn1 <- dnorm(y, mu, sig)
##         dg0 <- dgamma(out[i-1], alpha, beta)
##         dn0 <- dnorm(out[i-1], mu, sig)
##         ratio <- (dg1/dg0)/(dn1/dn0)
##         u <- runif(1)
##         if(u<ratio){
##             out[i] <- y
##         } else {
##             out[i] <- out[i-1]
##         }
##         ## Incluir contador da aceitação.
##         if(plot & nsim<=20){
##             ## Curvas.
##             curve(dgamma(x, alpha, beta), 0, 8, xlim=c(-2, 8),
##                   ylab="densidade");
##             curve(dnorm(x, mu, sig), add=TRUE, lty=2);
##             ## Lengendas.
##             legend("topright",
##                    legend=c(expression(f[X]*" ~ Gama"),
##                        expression(f[Y]*" ~ Normal")),
##                    lty=c(1,2), bty="n")
##             legend("right",
##                    legend=c(expression("Candidato em"*~i),
##                        expression("Valor em"*~i-1)),
##                    lty=1, col=c(2,4), bty="n")
##             ## Segmentos da base até os valores nas funções.
##             segments(y, dg1, y, 0, col=2, lty=1);
##             segments(y, dn1, y, 0, col=2, lty=1);
##             segments(out[i-1], dg0, out[i-1], 0, col=4, lty=1);
##             segments(out[i-1], dn0, out[i-1], 0, col=4, lty=1);
##             ## Pontos sobre as funções.
##             cex <- 2.5; col="yellow"
##             points(y, dg1, pch=19, cex=cex, col="green");
##             points(y, dn1, pch=19, cex=cex, col=col);
##             points(out[i-1], dg0, pch=19, cex=cex, col="green");
##             points(out[i-1], dn0, pch=19, cex=cex, col=col);
##             ## Rótulos dos pontos.
##             text(y, dg1, labels=expression(f[X]));
##             text(y, dn1, labels=expression(f[Y]));
##             text(out[i-1], dg0, expression(f[X]));
##             text(out[i-1], dn0, expression(f[Y]));
##             text(c(y, out[i-1]), 0,
##                  labels=c(expression(x[i]), expression(x[i-1])),
##                  pos=4)
##             ## Expressões matemáticas.
##             L <- list(dg1=dg1, dg0=dg0, dn1=dn1,
##                       dn0=dn0, num=dg1/dg0, den=dn1/dn0,
##                       ratio=ratio)
##             L <- lapply(L, round, digits=3)
##             ex <- substitute(frac(f[X](x[i]), f[X](x[i-1]))/
##                              frac(f[Y](x[i]), f[Y](x[i-1]))*" = "*
##                              frac(dg1, dg0)/frac(dn1, dn0)*" = "*
##                              num/den==ratio, L)
##             r <- substitute("u = "~u<ratio,
##                              lapply(list(ratio=ratio, u=u),
##                                     round, digits=3))
##             mtext(ex, side=3, line=1, adj=0)
##             mtext(r, side=3, line=2, adj=1)
##             mtext(ifelse(u<ratio,
##                          expression(Aceita~x[i]),
##                          expression(Repete~x[i-1])),
##                   side=3, line=1, adj=1)
##             switch(go[1],
##                    click=locator(n=1),
##                    console=readline(prompt="Press [enter] to continue"),
##                    none=Sys.sleep(0.5))
##         }
##     }
##     return(out)
## }
## 
## n <- 10
## x <- iidsampler3(n, x1=0.5, plot=TRUE, go="console")
## 
## ## Gerando muitos números pelo método.
## x <- iidsampler3(5000, x1=0.5)
## par(mfrow=c(2,2))
## plot(x, type="l")        ## Traço da cadeia completa.
## plot(x[1:100], type="l") ## Traço do começo da cadeia.
## acf(x)                   ## Mostra que a cadeia não é independente.
## plot(ecdf(x))            ## Acumulada teórica vs empírica.
## curve(pgamma(x, 2, 1), add=TRUE, col=2); layout(1)
## 
## ##----------------------------------------------------------------------
## ## Início da cadeia mal escolhido. Não convergência.
## set.seed(123)
## x <- iidsampler3(5000, x1=9.5)
## par(mfrow=c(2,2))
## plot(x, type="l")        ## Traço da cadeia completa.
## plot(x[1:100], type="l") ## Traço do começo da cadeia.
## acf(x)                   ## Mostra que a cadeia não é independente.
## plot(ecdf(x))            ## Acumulada teórica vs empírica.
## curve(pgamma(x, 2, 1), add=TRUE, col=2); layout(1)
## 
## ##----------------------------------------------------------------------
## ## Densidade da candidata mal posicionada posicionada.
## n <- 5000; alpha <- 2; beta <- 1
## mu <- -2; sig <- 2
## curve(dgamma(x, alpha, beta), 0, 12, xlim=c(-8, 8), ylab="densidade");
## curve(dnorm(x, mu, sig), add=TRUE, lty=2);
## 
## set.seed(123)
## x <- iidsampler3(n, alpha, beta, mu=mu, sig=sig, x1=2)
## par(mfrow=c(2,2))
## plot(x, type="l")        ## Traço da cadeia completa.
## plot(x[1:100], type="l") ## Traço do começo da cadeia.
## acf(x)                   ## Mostra que a cadeia não é independente.
## plot(ecdf(x))            ## Acumulada teórica vs empírica.
## curve(pgamma(x, alpha, beta), add=TRUE, col=2); layout(1)


## ---- include=FALSE, eval=FALSE-----------------------------------------------
## ## Da aula passada
## ## Define funções
## g <- function(x) exp(-x) * (x >= 0)
## ## f <- function(x) dexp(x, 0.5)
## delta <- 0.5
## m <- 1e3
## x <- numeric(m)
## x[1] <- 1
## for(i in 2:m) {
##     eps <- runif(1, -delta, delta)
##     y <- x[i - 1] + eps
##     alpha <- min(g(y)/g(x[i - 1]), 1)
##     u <- runif(1)
##     if(u <= alpha) {
##         x[i] <- y
##     } else {
##         x[i] <- x[i - 1]
##     }
## }
## 
## hist(x)
## plot(x, type = "l")

