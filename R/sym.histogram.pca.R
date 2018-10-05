#' Histogram Principal Components Analysis
#' @name sym.histogram.pca
#' @aliases sym.histogram.pca
#' @author Oldemar Rodriguez Rojas
#' @description This functions allows us to execute a histogram principal components analysis from a
#' symbolic data table with continuos, interval or histogram variables that can be mixed.
#' @usage sym.histogram.pca(sym.data, method = c('histogram', 'classic'))
#' @param sym.data Symbolic data table.
#' @param method The method to be used.
#'
#' @return Return a symbolic data table.
#' @references Diday, E., Rodriguez O. and Winberg S. (2000).
#' Generalization of the Principal Components Analysis to Histogram
#' Data, 4th European Conference on Principles and Practice of Knowledge Discovery in Data
#' Bases, September 12-16, 2000, Lyon, France.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @seealso sym.interval.pca
#'
#' @examples
#' data(example7)
#' res<-sym.histogram.pca(example7)
#' class(res) <- c('sym.data.table')
#' sym.scatterplot(res[,1],res[,2], labels=TRUE,col='red',main='Histogram PCA')
#' sym.scatterplot3d(res[,1],res[,2],res[,3],color='blue',
#'                   main='Histogram PCA')
#' @keywords Histogram PCA
#' @export
#' @importFrom FactoMineR PCA
#'
sym.histogram.pca <- function(sym.data, method = c("histogram", "classic")) {
  method <- match.arg(method)
  if (method == "histogram") {
    dam <- downarrow.matrix(sym.data)
    ram <- rightarrow.matrix(sym.data)
    cpc <- PCA(ram, graph = FALSE)
    res <- sym.interval.pca(dam, "centers")
    q <- min(res$Sym.Components$M, dim(cpc$ind$coord)[2])
  }
  k <- max(sym.data$sym.var.length)
  if (k == 1) {
    class(res) <- "sym.data.table"
    return(res)
  } else {
    pos <- 1
    for (i in 1:sym.data$N) {
      for (s in 1:k) {
        colm <- 2
        for (j in 1:q) {
          res$Sym.Components$meta[pos, colm] <- res$Sym.Components$meta[
            pos,
            colm
          ] + cpc$ind$coord[i, j]
          res$Sym.Components$meta[pos, colm + 1] <- res$Sym.Components$meta[
            pos,
            colm + 1
          ] + cpc$ind$coord[i, j]
          colm <- colm + 3
        }
        pos <- pos + 1
      }
    }
    dam$meta <- res$Sym.Components$meta
    class(dam) <- "sym.data.table"
    return(dam)
  }
}
