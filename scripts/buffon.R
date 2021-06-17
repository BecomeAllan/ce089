buffon <- function(n, a = 1, l = 1){
    buffon.calc <- function(n, ...){
        D.random <- runif(n, 0, a/2)
        theta.random <- runif(n, 0, pi)
        d <- (l/2) * sin(theta.random)
        H <- numeric(n)
        H[D.random <= d] <- 1
        h <- sum(H)
        pi.est <- (2*l*n)/(a*h)
        return(pi.est)
    }
    pi.res <- sapply(n, buffon.calc)
    saida <- data.frame(n = n,
                        pi.est = pi.res,
                        abs.error = abs(pi.res - pi))
    class(saida) <- c("buffon", "data.frame")
    return(saida)
}
