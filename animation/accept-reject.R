library(animation)

loop <- function() {
    M <- 3
    ## M <- sqrt(2 * pi/exp(1))
    f <- function(x) 1.5 * x^2
    g <- function(x) 0.5 + 0 * x
    ## f <- function(x) dnorm(x, 0, 1)
    ## g <- function(x) dcauchy(x, 0, 1)
    n <- 0L
    l <- 1L
    N <- 20L
    q <- 0
    set.seed(123)
    x <- numeric(N)
    while (n < N) {
        curve(M * g(x), -1, 1,
              lty = 2, ylim = c(0, M * g(0)),
              xlim = c(-1.3, 1.3),
              ylab = NA,
              xlab = NA,
              n = 303)
        curve(f, add = TRUE, n = 303, from = -1, to = 1)
        legend("left",
               legend = c("f(x)", "M g(x)"),
               lty = c(1, 2),
               bty = "n")
        legend("right",
               title = "Decisão:",
               legend = c("Aceitar", "Rejeitar"),
               lty = 1,
               col = c(3, 2),
               bty = "n")
        legend("bottomright",
               title = "Valor:",
               legend = c("Aceito", "Rejeitado"),
               pch = c(1, 4),
               col = c(3, 2),
               bty = "n")
        y <- runif(n = 1, -1, 1)
        title(sub = substitute("Candidato: " * y == yy,
                               list(yy = round(y, 4))))
        rug(y, lwd = 2, col = 1)
        segments(y, 0, y, f(y), col = 3)
        segments(y, f(y), y, M * g(y), col = 2)
        segments(y - 0.1, 0, y + 0.1, 0, col = 1)
        segments(y - 0.1, f(y), y + 0.1, f(y), col = 3)
        segments(y - 0.1, M * g(y), y + 0.1, M * g(y), col = 2)
        u <- runif(n = 1)
        w <- f(y)/(M * g(y))
        d <- u < w
        if (d) {
            x[n] <- y
            n <- n + 1L
            l <- l + 1L
            q <- n/l
            points(y, u * M * g(y), pch = 1, col = 3)
        } else {
            l <- l + 1L
            q <- n/l
            points(y, u * M * g(y), pch = 4, col = 2)
        }
        mtext(side = 3,
              text = sprintf("u = %s <= r = %s = %s/%s",
                             round(u, 4), round(w, 4),
                             round(f(y), 4), round(M * g(y), 4)))
        ## text = substitute(
        ##     u * ' = ' * uu <= r * ' = ' * rr * ' = ' f,
        ##     list(uu = round(u, 4),
        ##          rr = round(w, 4))))
        msg <- sprintf("iterações: %d \t aceitos: %d \t taxa ac.: %0.2f",
                       l, n, q)
        title(main = list(msg, font = 1))
        ## Sys.sleep(0.5)
    } # END while(n < N)
} # END loop().

# Verifica.
# loop()

saveHTML(loop(),
         img.name = "accept-reject",
         imgdir = "accept-reject",
         htmlfile = "accept-reject.html",
         autobrowse = FALSE,
         verbose = FALSE,
         title = "Método da aceitação e rejeição",
         ani.width = 600,
         ani.height = 600)

##======================================================================

loop <- function() {
    M <- 2.669744
    ## M <- sqrt(2 * pi/exp(1))
    f <- function(x) dbeta(x, alfa, beta)
    g <- function(x) 1 + 0 * x
    alfa <- 2.7; beta <- 6.3
    ## f <- function(x) dnorm(x, 0, 1)
    ## g <- function(x) dcauchy(x, 0, 1)
    n <- 0L
    l <- 1L
    N <- 20L
    q <- 0
    set.seed(123)
    x <- numeric(N)
    while (n < N) {
        curve(M * g(x), 0, 1,
              lty = 2, ylim = c(0, M * g(0)),
              xlim = c(0, 1),
              ylab = NA,
              xlab = NA,
              n = 303)
        curve(f, add = TRUE, n = 303, from = 0, to = 1)
        legend("topleft",
               legend = c("f(x)", "M g(x)"),
               lty = c(1, 2),
               xjust = 1,
               yjust = 1,
               bty = "n")
        legend("right",
               title = "Decisão:",
               legend = c("Aceitar", "Rejeitar"),
               lty = 1,
               col = c(3, 2),
               bty = "n")
        legend("bottomright",
               title = "Valor:",
               legend = c("Aceito", "Rejeitado"),
               pch = c(1, 4),
               col = c(3, 2),
               bty = "n")
        y <- runif(n = 1, 0, 1)
        title(sub = substitute("Candidato: " * y == yy,
                               list(yy = round(y, 4))))
        rug(y, lwd = 2, col = 1)
        segments(y, 0, y, f(y), col = 3)
        segments(y, f(y), y, M * g(y), col = 2)
        segments(y - 0.05, 0, y + 0.05, 0, col = 1)
        segments(y - 0.05, f(y), y + 0.05, f(y), col = 3)
        segments(y - 0.05, M * g(y), y + 0.05, M * g(y), col = 2)
        u <- runif(n = 1)
        w <- f(y)/(M * g(y))
        d <- u < w
        if (d) {
            x[n] <- y
            n <- n + 1L
            l <- l + 1L
            q <- n/l
            points(y, u * M * g(y), pch = 1, col = 3)
        } else {
            l <- l + 1L
            q <- n/l
            points(y, u * M * g(y), pch = 4, col = 2)
        }
        mtext(side = 3,
              text = sprintf("u = %s <= r = %s = %s/%s",
                             round(u, 4), round(w, 4),
                             round(f(y), 4), round(M * g(y), 4)))
              ## text = substitute(
              ##     u * ' = ' * uu <= r * ' = ' * rr * ' = ' f,
              ##     list(uu = round(u, 4),
              ##          rr = round(w, 4))))
        msg <- sprintf("iterações: %d \t aceitos: %d \t taxa ac.: %0.2f",
                       l, n, q)
        title(main = list(msg, font = 1))
        # Sys.sleep(0.5)
    } # END while(n < N)
} # END loop().

# Verifica.
# loop()

saveHTML(loop(),
         img.name = "accept-reject2",
         imgdir = "accept-reject2",
         htmlfile = "accept-reject2.html",
         autobrowse = FALSE,
         verbose = FALSE,
         title = "Método da aceitação e rejeição",
         ani.width = 600,
         ani.height = 600)

##======================================================================

loop <- function() {
    M <- 1.671808
    ## M <- sqrt(2 * pi/exp(1))
    f <- function(x) dbeta(x, 2.7, 6.3)
    g <- function(x) dbeta(x, 2, 6)
    alfa <- 2.7; beta <- 6.3
    ## f <- function(x) dnorm(x, 0, 1)
    ## g <- function(x) dcauchy(x, 0, 1)
    n <- 0L
    l <- 1L
    N <- 20L
    q <- 0
    set.seed(123)
    x <- numeric(N)
    while (n < N) {
        curve(M * g(x), 0, 1,
              ## lty = 2, ylim = c(0, M * g(0)),
              lty = 2, ylim = c(0, 5),
              xlim = c(0, 1),
              ylab = NA,
              xlab = NA,
              n = 303)
        curve(f, add = TRUE, n = 303, from = 0, to = 1)
        legend("topleft",
               legend = c("f(x)", "M g(x)"),
               lty = c(1, 2),
               xjust = 1,
               yjust = 1,
               bty = "n")
        legend("right",
               title = "Decisão:",
               legend = c("Aceitar", "Rejeitar"),
               lty = 1,
               col = c(3, 2),
               bty = "n")
        legend("bottomright",
               title = "Valor:",
               legend = c("Aceito", "Rejeitado"),
               pch = c(1, 4),
               col = c(3, 2),
               bty = "n")
        y <- rbeta(n = 1, 2, 6)
        title(sub = substitute("Candidato: " * y == yy,
                               list(yy = round(y, 4))))
        rug(y, lwd = 2, col = 1)
        segments(y, 0, y, f(y), col = 3)
        segments(y, f(y), y, M * g(y), col = 2)
        segments(y - 0.05, 0, y + 0.05, 0, col = 1)
        segments(y - 0.05, f(y), y + 0.05, f(y), col = 3)
        segments(y - 0.05, M * g(y), y + 0.05, M * g(y), col = 2)
        u <- runif(n = 1)
        w <- f(y)/(M * g(y))
        d <- u < w
        if (d) {
            x[n] <- y
            n <- n + 1L
            l <- l + 1L
            q <- n/l
            points(y, u * M * g(y), pch = 1, col = 3)
        } else {
            l <- l + 1L
            q <- n/l
            points(y, u * M * g(y), pch = 4, col = 2)
        }
        mtext(side = 3,
              text = sprintf("u = %s <= r = %s = %s/%s",
                             round(u, 4), round(w, 4),
                             round(f(y), 4), round(M * g(y), 4)))
              ## text = substitute(
              ##     u * ' = ' * uu <= r * ' = ' * rr * ' = ' f,
              ##     list(uu = round(u, 4),
              ##          rr = round(w, 4))))
        msg <- sprintf("iterações: %d \t aceitos: %d \t taxa ac.: %0.2f",
                       l, n, q)
        title(main = list(msg, font = 1))
        # Sys.sleep(0.5)
    } # END while(n < N)
} # END loop().

# Verifica.
# loop()

saveHTML(loop(),
         img.name = "accept-reject3",
         imgdir = "accept-reject3",
         htmlfile = "accept-reject3.html",
         autobrowse = FALSE,
         verbose = FALSE,
         title = "Método da aceitação e rejeição",
         ani.width = 600,
         ani.height = 600)
