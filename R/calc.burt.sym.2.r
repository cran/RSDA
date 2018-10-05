calc.burt.sym.2 <- function(sym.data, pos.var.x, pos.var.y) {
  num.vars.x <- length(pos.var.x)
  num.vars.y <- length(pos.var.y)

  num.rows <- sum(sym.data$sym.var.length[pos.var.x])
  num.cols <- sum(sym.data$sym.var.length[pos.var.y])

  burt.sym <- as.data.frame(matrix(rep(0, 2 * num.cols * num.rows), nrow = num.rows))

  pos.col <- pos.row <- 0

  sym.var.length <- sym.data$sym.var.length[pos.var.x]
  sym.var.length.2 <- sym.data$sym.var.length[pos.var.y] * 2

  cum.sym.var.length <- cumsum(sym.var.length)
  cum.sym.var.length.2 <- cumsum(sym.var.length.2)

  pos.col.ini <- 0
  pos.row.ini <- 0


  for (i in 1:num.vars.x)
  {
    var.act <- sym.var(sym.data, pos.var.x[i])

    pos.row.ini <- pos.row.ini + 1

    pos.row <- cum.sym.var.length[i]

    indx.row.i <- (pos.row.ini:pos.row)

    for (j in 1:num.vars.y)
    {
      pos.col.ini <- pos.col.ini + 1
      pos.col <- cum.sym.var.length.2[j]

      K <- calc.k(var.act, sym.var(sym.data, pos.var.y[j]))

      burt.sym[indx.row.i, (pos.col.ini:pos.col)] <- K$data
      colnames(burt.sym)[(pos.col.ini:pos.col)] <- colnames(K$data)

      pos.col.ini <- cum.sym.var.length.2[j]
    }

    row.names(burt.sym)[indx.row.i] <- rownames(K$data)
    pos.row.ini <- pos.row
    pos.col.ini <- 0
  }

  return(data.frame.to.RSDA.inteval.table(burt.sym))
}
