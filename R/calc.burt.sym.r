#' Burt Matrix
#'
#' @param sym.data ddd
#' @param pos.var ddd
#'
#' @export
#'
calc.burt.sym <- function(sym.data, pos.var) {
  num.vars <- length(pos.var)
  dim.matrix <- sum(sym.data$sym.var.length[pos.var])
  burt.sym <- as.data.frame(matrix(rep(0, 2 * dim.matrix^2), nrow = dim.matrix))

  pos.col <- pos.row <- 0

  sym.var.length <- sym.data$sym.var.length[pos.var]
  sym.var.length.2 <- sym.var.length * 2

  cum.sym.var.length <- cumsum(sym.var.length)
  cum.sym.var.length.2 <- cumsum(sym.var.length.2)

  pos.col.ini <- 0
  pos.row.ini <- 0

  for (i in 1:(num.vars - 1))
  {
    var.act <- sym.var(sym.data, pos.var[i])

    pos.col.ini <- pos.col.ini + 1
    pos.row.ini <- pos.row.ini + 1

    pos.row <- cum.sym.var.length[i]
    pos.col <- cum.sym.var.length.2[i]

    K <- calc.k(var.act, var.act)

    indx.col.i <- (pos.col.ini:pos.col)
    indx.row.i <- (pos.row.ini:pos.row)

    burt.sym[indx.row.i, indx.col.i] <- K$data
    colnames(burt.sym)[indx.col.i] <- colnames(K$data)
    row.names(burt.sym)[indx.row.i] <- row.names(K$data)

    for (j in (i + 1):num.vars)
    {
      pos.col.ini <- cum.sym.var.length.2[j - 1] + 1
      pos.col <- cum.sym.var.length.2[j]
      K <- calc.k(var.act, sym.var(sym.data, pos.var[j]))
      K.t <- transpose.sym(K)

      burt.sym[indx.row.i, (pos.col.ini:pos.col)] <- K$data
      colnames(burt.sym)[(pos.col.ini:pos.col)] <- colnames(K$data)


      burt.sym[(cum.sym.var.length[j - 1] + 1):(cum.sym.var.length[j]), indx.col.i] <- K.t$data
    }

    pos.row.ini <- pos.row
    pos.col.ini <- cum.sym.var.length.2[i]
  }

  i <- num.vars
  var.act <- sym.var(sym.data, pos.var[i])

  pos.row <- cum.sym.var.length[i]
  pos.col <- cum.sym.var.length.2[i]

  K <- calc.k(var.act, var.act)
  indx.col.i <- ((cum.sym.var.length.2[i - 1] + 1):pos.col)
  indx.row.i <- ((cum.sym.var.length[i - 1] + 1):pos.row)

  burt.sym[indx.row.i, indx.col.i] <- K$data
  colnames(burt.sym)[indx.col.i] <- colnames(K$data)
  row.names(burt.sym)[indx.row.i] <- row.names(K$data)

  return(data.frame.to.RSDA.inteval.table(burt.sym))
}
