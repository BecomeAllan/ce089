## Simular de uma mistura de normais. Normais com variância 1 e mistura
## 1:1.
k <- 0.5
curve(k*dnorm(x, 0, 1)+(1-k)*dnorm(x, 7, 1), -3, 10)
curve(0.1*dunif(x), add=TRUE, col=2, n=1024)

rwsampler2 <- function(nsim, x1, delta,
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
        dn1 <- k*dnorm(can, 0, 1)+(1-k)*dnorm(can, 7, 1)
        dn0 <- k*dnorm(out[i-1], 0, 1)+(1-k)*dnorm(out[i-1], 7, 1)
        ratio <- dn1/dn0
        u <- runif(1)
        if(u<ratio) out[i] <- can else out[i] <- out[i-1]
        if(plot & nsim<=20){
            curve(k*dnorm(x, 0, 1)+(1-k)*dnorm(x, 7, 1), -3, 10,
                  ylab="densidade")
            curve(0.3*dunif(x, out[i-1]-delta, out[i-1]+delta),
                  add=TRUE, lty=2)
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

x <- rwsampler2(nsim=20, x1=1, delta=2, plot=TRUE, go="console")
x <- rwsampler2(nsim=20, x1=1, delta=1, plot=TRUE, go="console")
x <- rwsampler2(nsim=20, x1=1, delta=4, plot=TRUE, go="console")

##----------------------------------------------------------------------
## Muitos valores.

## Janela estreita -----------------------------------------------------
set.seed(123)
x <- rwsampler2(nsim=20000, x1=1, delta=1, plot=FALSE)

par(mfrow=c(2,2))
plot(x, type="l")        ## Traço da cadeia completa.
plot(x[1:100], type="l") ## Traço do começo da cadeia.
acf(x)                   ## Mostra que a cadeia não é independente.
plot(ecdf(x))            ## Acumulada teórica vs empírica.
curve(k*pnorm(x, 0, 1)+(1-k)*pnorm(x, 7, 1), add=TRUE, col=2); layout(1)
prop.table(table(x<3.5))

## Janela larga --------------------------------------------------------
set.seed(123)
x <- rwsampler2(nsim=20000, x1=1, delta=4, plot=FALSE)

par(mfrow=c(2,2))
plot(x, type="l")        ## Traço da cadeia completa.
plot(x[1:100], type="l") ## Traço do começo da cadeia.
acf(x)                   ## Mostra que a cadeia não é independente.
plot(ecdf(x))            ## Acumulada teórica vs empírica.
curve(k*pnorm(x, 0, 1)+(1-k)*pnorm(x, 7, 1), add=TRUE, col=2); layout(1)
prop.table(table(x<3.5))
