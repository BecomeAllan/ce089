## Alvo: Gama.
## Canditada: Normal.
curve(dgamma(x, 2, 1), -1.5, 8)
curve(dnorm(x, 2, sqrt(2)), add=TRUE, lty=2)

iidsampler3 <- function(nsim, x1, alpha=2, beta=1, mu=NULL, sig=NULL,
                        plot=FALSE, go=c("click","enter","none")){
    out <- vector(mode="numeric", length=nsim)
    ## Esperança da distribuição proposta.
    out[1] <- x1
    ## Esperança da distribuição proposta.
    if(is.null(mu)){
        mu <- alpha/beta
    }
    ## Variância da distribuição proposta.
    if(is.null(sig)){
        sig <- sqrt(alpha/(beta^2))
    }
    for(i in 2:nsim){
        ## Realização da distribuição alvo.
        if(plot & go[1]=="click"){
            y <- locator(n=1)$x
        } else {
            y <- rnorm(1, mu, sig)
        }
        ## Cálculo da razão de aceitação.
        dg1 <- dgamma(y, alpha, beta)
        dn1 <- dnorm(y, mu, sig)
        dg0 <- dgamma(out[i-1], alpha, beta)
        dn0 <- dnorm(out[i-1], mu, sig)
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
            curve(dgamma(x, alpha, beta), 0, 8, xlim=c(-2, 8),
                  ylab="densidade");
            curve(dnorm(x, mu, sig), add=TRUE, lty=2);
            ## Lengendas.
            legend("topright",
                   legend=c(expression(f[X]*" ~ Gama"),
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

n <- 10
x <- iidsampler3(n, x1=0.5, plot=TRUE, go="console")

## Gerando muitos números pelo método.
x <- iidsampler3(5000, x1=0.5)
par(mfrow=c(2,2))
plot(x, type="l")        ## Traço da cadeia completa.
plot(x[1:100], type="l") ## Traço do começo da cadeia.
acf(x)                   ## Mostra que a cadeia não é independente.
plot(ecdf(x))            ## Acumulada teórica vs empírica.
curve(pgamma(x, 2, 1), add=TRUE, col=2); layout(1)

##----------------------------------------------------------------------
## Início da cadeia mal escolhido. Não convergência.
set.seed(123)
x <- iidsampler3(5000, x1=9.5)
par(mfrow=c(2,2))
plot(x, type="l")        ## Traço da cadeia completa.
plot(x[1:100], type="l") ## Traço do começo da cadeia.
acf(x)                   ## Mostra que a cadeia não é independente.
plot(ecdf(x))            ## Acumulada teórica vs empírica.
curve(pgamma(x, 2, 1), add=TRUE, col=2); layout(1)

##----------------------------------------------------------------------
## Densidade da candidata mal posicionada posicionada.
n <- 5000; alpha <- 2; beta <- 1
mu <- -2; sig <- 2
curve(dgamma(x, alpha, beta), 0, 12, xlim=c(-8, 8), ylab="densidade");
curve(dnorm(x, mu, sig), add=TRUE, lty=2);

set.seed(123)
x <- iidsampler3(n, alpha, beta, mu=mu, sig=sig, x1=2)
par(mfrow=c(2,2))
plot(x, type="l")        ## Traço da cadeia completa.
plot(x[1:100], type="l") ## Traço do começo da cadeia.
acf(x)                   ## Mostra que a cadeia não é independente.
plot(ecdf(x))            ## Acumulada teórica vs empírica.
curve(pgamma(x, alpha, beta), add=TRUE, col=2); layout(1)
