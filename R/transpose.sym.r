transpose.sym <- function(sym.data) {
  M <- sym.data$M
  N <- sym.data$N
  sym.obj.names <- sym.data$sym.obj.names
  sym.var.names <- sym.data$sym.var.names
  data <- sym.data$data

  data.sal <- as.data.frame(matrix(rep(0, 2 * M * N), nrow = M))

  seq.min <- seq(from = 1, by = 2, length.out = N)
  seq.max <- seq(from = 2, by = 2, length.out = N)

  for (i in 1:M)
  {
    indx.i <- (2 * i - 1):(2 * i)
    for (j in 1:N)
    {
      ind.j <- (2 * j - 1):(2 * j)
      data.sal[i, ind.j] <- data[j, indx.i]
    }
  }

  row.names(data.sal) <- sym.var.names
  colnames(data.sal)[seq.min] <- sym.obj.names
  colnames(data.sal)[seq.max] <- paste0(sym.obj.names, ".1")
  return(data.frame.to.RSDA.inteval.table(data.sal))
}
