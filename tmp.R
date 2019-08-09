## Vetor com uma sequência de 1 a 1.000.000
x <- 1:1000000

## Cria um objeto de armazenamento com o mesmo tamanho do resultado
st1 <- system.time({
    out1 <- numeric(length(x))
    for(i in 1:length(x)){
        out1[i] <- x[i]^2
    }
})
st1

## Cria um objeto de tamanho "zero" e vai "crescendo" esse vetor
st2 <- system.time({
    out2 <- numeric(0)
    for(i in 1:length(x)){
        out2[i] <- x[i]^2
    }
})
st2

## Cria um objeto de tamanho "zero" e cresce o vetor usando a função c()
## NUNCA faça isso!!
st3 <- system.time({
    out3 <- numeric(0)
    for(i in 1:length(x)){
        out3 <- c(out3, x[i]^2)
    }
})
st3
identical(out1, out2, out3)
