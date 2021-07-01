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

mu <- 0; sigma <- 1
x <- rwsampler1(nsim=10, x1=-1, delta= .5, mu, sigma, plot=TRUE,
                go="console")
## x <- rwsampler1(nsim=10, x1=-1, delta= .5, mu, sigma, plot=TRUE,
##                 go="click")
x <- rwsampler1(nsim=10, x1=-1, delta=2, mu, sigma, plot=TRUE,
                go="console")
## x <- rwsampler1(nsim=10, x1=-1, delta=2, mu, sigma, plot=TRUE,
##                 go="click")

animation::saveHTML(
               rwsampler1(nsim = 20, x1 = -1, delta = 2, mu = 0,
                          sigma = 1, plot = TRUE, go = "none"),
               img.name = "MH_RW_normal_unif",
               imgdir = "../figures/MH_RW_normal_unif",
               htmlfile = "MH_RW_normal_unif.html",
               autobrowse = FALSE,
               verbose = FALSE,
               ani.width = 600,
               ani.height = 600)
