desv.fun <- function(x) {
    N <- length(x)
    return(sd(x) * sqrt((N - 1)/N))
}
