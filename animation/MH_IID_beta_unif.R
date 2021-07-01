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

n <- 10
x <- iidsampler1(n, x1=0.5, plot=TRUE, go="console")
## Gerando muitos números pelo método.
x <- iidsampler1(5000, x1=0.5)
par(mfrow=c(2,2))
plot(x, type="l")        ## Traço da cadeia completa.
plot(x[1:100], type="l") ## Traço do começo da cadeia.
acf(x)                   ## Mostra que a cadeia não é independente.
plot(ecdf(x))            ## Acumulada teórica vs empírica.
curve(pbeta(x, 2, 3), add=TRUE, col=2); layout(1)

animation::saveHTML(
               iidsampler1(n = 20, x1 = 0.5, plot = TRUE, go = "none"),
               img.name = "MH_IID_beta_unif",
               imgdir = "../figures/MH_IID_beta_unif",
               htmlfile = "MH_IID_beta_unif.html",
               autobrowse = FALSE,
               verbose = FALSE,
               ani.width = 600,
               ani.height = 600)
