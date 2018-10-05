calc.k <- function(var.sym.X, var.sym.Y) {
  data.X.max <- var.sym.X$var.data.vector
  data.Y.max <- var.sym.Y$var.data.vector
  data.X.min <- calc.matrix.min(data.X.max)
  data.Y.min <- calc.matrix.min(data.Y.max)

  K.min <- t(as.matrix(data.X.min)) %*% as.matrix(data.Y.min)
  K.max <- t(as.matrix(data.X.max)) %*% as.matrix(data.Y.max)

  N <- dim(K.min)
  K <- as.data.frame(matrix(rep(0, 2 * N[1] * N[2]), nrow = N[1]))
  indx.min <- seq(from = 1, by = 2, length.out = N[2])
  indx.max <- seq(from = 2, by = 2, length.out = N[2])
  K[, indx.min] <- K.min
  K[, indx.max] <- K.max

  col.names.K <- colnames(K.min)
  row.names.K <- row.names(K.min)

  row.names(K) <- row.names.K
  colnames(K)[indx.min] <- col.names.K
  colnames(K)[indx.max] <- paste0(col.names.K, ".1")

  return(data.frame.to.RSDA.inteval.table(K))
}
