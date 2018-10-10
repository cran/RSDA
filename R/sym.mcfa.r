#' sym.mcfa
#' @aliases sym.mcfa
#' @author Jorge Arce
#' @param sym.data A symbolic data table containing at least two set type variables.
#' @param pos.var Column numbers in the symbolic data table that contain the set type variables.
#' @description This function executes a Multiple Correspondence Factor Analysis for variables of set type.
#' @usage sym.mcfa(sym.data, pos.var)
#' @references  Arce J. and Rodriguez, O. (2018). Multiple Correspondence Analysis for Symbolic Multi–Valued Variables. On the Symbolic Data Analysis Workshop SDA 2018.
#'
#' Benzecri, J.P. (1973). L' Analyse des Données. Tomo 2: L'Analyse des Correspondances. Dunod, Paris.
#'
#' Castillo, W. and Rodriguez O. (1997). Algoritmo e implementacion del analisis factorial de correspondencias. Revista de Matematicas: Teoria y Aplicaciones, 24-31.
#'
#' Takagi I. and Yadosiha H. (2011). Correspondence Analysis for symbolic contingency tables base on interval algebra. Elsevier Procedia Computer Science, 6, 352-357.
#'
#' Rodriguez, O. (2007). Correspondence Analysis for Symbolic Multi--Valued Variables. CARME 2007 (Rotterdam, The Netherlands), http://www.carme-n.org/carme2007.
#'
#' @examples
#' data("ex_mcfa1")
#' sym.table <- classic.to.sym(ex_mcfa1, concept = "suspect",
#'                    variables.types = c(hair = type.set(),
#'                                        eyes = type.set(),
#'                                        region = type.set()))
#'
#' res <- sym.mcfa(sym.table, c(1,2))
#' mcfa.scatterplot(res[,1], res[,2], sym.data = sym.table, pos.var = c(1,2))
#'

sym.mcfa <- function (sym.data, pos.var)
{
  sym.data <- calc.burt.sym(sym.data, pos.var)
  idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
  if (idn == FALSE)
    stop("All variables have to be of the same type")
  res <- cfa.totals(sym.data)
  Z <- cfa.MatrixZ(sym.data, res$TotalRows, res$TotalColumns)
  svd <- eigen(Z)
  # se aplica cambio enviado por jorge
  svd$values <- svd$values*svd$values

  MVPRealz <- cfa.CVPRealz(sym.data, res$TotalRows, res$TotalColumns,
                           res$Total, svd$vectors)
  Mzz <- cfa.Czz(sym.data, res$TotalRows, res$TotalColumns,
                 MVPRealz, svd$values)
  CMM <- cfa.minmax.new(sym.data, res$TotalRows, res$TotalRowsMin,
                    res$TotalRowsMax, res$TotalColumns, res$TotalColumnsMin,
                    res$TotalColumnsMax, res$Total, MVPRealz, Mzz)
  n.sym.objects <- sym.data$N
  n.sym.var <- sym.data$M - 1
  sym.var.types <- list()
  sym.var.length <- rep(0, n.sym.var)
  sym.var.names <- list()
  sym.var.starts <- rep(0, n.sym.var)
  posd <- 1
  posdd <- 2
  for (ss in 1:n.sym.var) {
    sym.var.types[ss] <- "$I"
    sym.var.length[ss] <- 2
    sym.var.names[ss] <- paste("C", ss, sep = "")
    sym.var.starts[ss] <- posdd
    posd <- posd + 2
    posdd <- posdd + 3
  }
  sym.obj.names <- sym.data$sym.obj.names
  data.matrix <- matrix(0, n.sym.objects, 2 * n.sym.var)
  meta.data <- matrix(0, n.sym.objects, 2 * n.sym.var)
  for (i in 1:n.sym.objects) {
    posd <- 1
    for (j in 1:n.sym.var) {
      meta.data[i, posd] <- CMM$Min[i, j]
      meta.data[i, posd + 1] <- CMM$Max[i, j]
      data.matrix[i, posd] <- CMM$Min[i, j]
      data.matrix[i, posd + 1] <- CMM$Max[i, j]
      posd <- posd + 2
    }
  }
  col.types <- matrix(0, n.sym.objects, 1)
  for (ss in 1:n.sym.objects) {
    col.types[ss] <- "$I"
  }
  qq <- matrix(0, n.sym.objects, 3 * n.sym.var)
  qq <- cbind(col.types, meta.data[, 1:2])
  posd <- 3
  for (ss in 2:n.sym.var) {
    qq <- cbind(qq, col.types)
    qq <- cbind(qq, meta.data[, posd])
    qq <- cbind(qq, meta.data[, posd + 1])
    posd <- posd + 2
  }
  meta.data <- qq
  tt <- list()
  ttt <- list()
  posd <- 1
  posdd <- 1
  for (ss in 1:n.sym.var) {
    tt[posd] <- paste("C", ss, sep = "")
    tt[posd + 1] <- paste("C", ss, sep = "")
    posd <- posd + 2
    ttt[posdd] <- "$I"
    ttt[posdd + 1] <- paste("C", ss, sep = "")
    ttt[posdd + 2] <- paste("C", ss, sep = "")
    posdd <- posdd + 3
  }
  meta.data <- as.data.frame(meta.data)
  data.matrix <- as.data.frame(data.matrix)
  rownames(data.matrix) <- sym.obj.names
  colnames(data.matrix) <- tt
  rownames(meta.data) <- sym.obj.names
  colnames(meta.data) <- ttt
  posdd <- 1
  for (ss in 1:n.sym.var) {
    meta.data[, posdd + 1] <- as.numeric(as.vector(meta.data[,
                                                             posdd + 1]))
    meta.data[, posdd + 2] <- as.numeric(as.vector(meta.data[,
                                                             posdd + 2]))
    posdd <- posdd + 3
  }
  sym.data <- list(N = n.sym.objects, M = n.sym.var, sym.obj.names = sym.obj.names,
                   sym.var.names = unlist(sym.var.names), sym.var.types = unlist(sym.var.types),
                   sym.var.length = sym.var.length, sym.var.starts = unlist(sym.var.starts),
                   meta = meta.data, data = data.matrix)
  class(sym.data) <- "sym.data.table"
  return(sym.data)
}
