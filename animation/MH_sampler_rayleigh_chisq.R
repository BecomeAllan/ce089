## Nao esta OK

## Alvo: Rayleigh
## Canditada: Chi-quadrado
mhsampler1 <- function(nsim, x1, plot=FALSE,
                       go=c("click","enter","none")){
    out <- vector(mode="numeric", length=nsim)
    f <- function(x, sigma) {
        (x / sigma^2) * exp(-x^2 / (2 * sigma^2)) * (x >= 0) * (sigma > 0)
    }
    ## Valor para iniciar a cadeia.
    out[1] <- x1
    for(i in 2:nsim){
        ## Realização da distribuição alvo.
        if(plot & go[1]=="click"){
            y <- locator(n=1)$x
        } else {
            y <- rchisq(1, out[i - 1])
        }
        ## Cálculo da razão de aceitação.
        ## num <- f(y, sigma) * dchisq(x[i - 1], df = y)
        ## den <- f(x[i - 1], sigma) * dchisq(y, df = x[i - 1])
        dg1 <- f(y, 4)
        dn1 <- dchisq(out[i - 1], df = y)
        dg0 <- f(out[i-1], 4)
        dn0 <- dchisq(y, df = out[i - 1])
        ratio <- (dg1 * dn1)/(dg0 * dn0)
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
            curve(f(x, 4), 0, 20, ylim=c(0, .4),
                  ylab="densidade");
            curve(dchisq(x, df = out[i - 1]), add=TRUE, lty=2);
            curve(dchisq(x, df = y), add=TRUE, lty=3);
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
            ## segments(y, dg1, y, 0, col=2, lty=1);
            ## segments(y, dn1, y, 0, col=2, lty=1);
            ## segments(out[i-1], dg0, out[i-1], 0, col=4, lty=1);
            ## segments(out[i-1], dn0, out[i-1], 0, col=4, lty=1);
            segments(y, dg1, y, 0, col=2, lty=1);
            segments(out[i - 1], dn1, out[i - 1], 0, col=4, lty=1);
            segments(out[i-1], dg0, out[i-1], 0, col=4, lty=1);
            segments(y, dn0, y, 0, col=2, lty=1);
            ## Pontos sobre as funções.
            cex <- 2.5; col="yellow"
            points(y, dg1, pch=19, cex=cex, col="green");
            points(out[i - 1], dn1, pch=19, cex=cex, col=col);
            points(out[i-1], dg0, pch=19, cex=cex, col="green");
            points(y, dn0, pch=19, cex=cex, col=col);
            ## Rótulos dos pontos.
            text(y, dg1, labels=expression(f[X]));
            text(out[i - 1], dn1, labels=expression(f[Y]));
            text(out[i-1], dg0, expression(f[X]));
            text(y, dn0, expression(f[Y]));
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

n <- 10
x <- mhsampler1(n, x1 = 4, plot=TRUE, go="none")

## Gerando muitos números pelo método.
x <- mhsampler1(5000, x1=4)
par(mfrow=c(2,2))
plot(x, type="l")        ## Traço da cadeia completa.
plot(x[1:100], type="l") ## Traço do começo da cadeia.
acf(x)                   ## Mostra que a cadeia não é independente.
plot(ecdf(x))            ## Acumulada teórica vs empírica.
curve(Fx(x, 4), add = TRUE, col = 2, from = 0)
par(mfrow=c(1,1))

N <- 1e4
## Rayleigh(4)
sigma <- 4
x <- numeric(N)
x[1] <- 100
k <- 0 # para contar quantos foram aceitos
for (i in 2:N) {
    y <- rchisq(1, df = x[i - 1])
    num <- f(y, sigma) * g(x[i - 1], df = y)
    den <- f(x[i - 1], sigma) * g(y, df = x[i - 1])
    alpha <- num/den
    u <- runif(1)
    if (u <= alpha) {
        x[i] <- y
    } else {
        x[i] <- x[i - 1]
        k <- k + 1     # contagem dos aceitos
    }
}
plot.ts(x)
