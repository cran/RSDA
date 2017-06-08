sym.scale.interval <- function(sym.data, mean.var, desv.var) {
    data <- sym.data$data
    M <- sym.data$M
    for (i in 1:M) {
        indx <- (2 * i - 1):(2 * i)
        data[, indx] <- (data[, indx] - mean.var[i])/desv.var[i]
    }
    return(data)
}
