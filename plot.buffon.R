plot.buffon <- function(x, xlab = "Numero de jogadas da agulha",
                        ylab = expression(paste("Estimativa de ", pi)),
                        ...){
    plot(x$n, x$pi.est, type = "l", xlab = xlab, ylab = ylab,
         main = "Experimento de Buffon", ...)
    abline(h = pi, col = "lightgrey")
}
