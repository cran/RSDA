#' Symbolic Correlation
#' @name sym.cor
#' @aliases sym.cor
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic correlation
#' @usage sym.cor(sym.var.x, sym.var.y, method = c('centers', 'interval', 'billard', 'modal')
#' , na.rm = FALSE, ...)
#' @param sym.var.x First symbolic variables.
#' @param sym.var.y Second symbolic variables.
#' @param method The method to be use.
#' @param na.rm As in R cor function.
#' @param ... As in R cor function.
#'
#' @return Return a real number in [-1,1].
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @examples
#' data(example3)
#' sym.data<-example3
#' sym.cor(sym.var(sym.data,1),sym.var(sym.data,4),method='centers')
#' sym.cor(sym.var(sym.data,2),sym.var(sym.data,6),method='centers')
#' sym.cor(sym.var(sym.data,2),sym.var(sym.data,6),method='billard')
#' @keywords Symbolic Correlation
#' @export
#'
sym.cor <- function(sym.var.x, sym.var.y, method = c(
                      "centers", "interval", "billard",
                      "modal"
                    ), na.rm = FALSE, ...) {
  method <- match.arg(method)
  if (method == "centers") {
    if ((sym.var.x$var.type == "$C") && (sym.var.y$var.type == "$C")) {
      return(cor(sym.var.x$var.data.vector, sym.var.y$var.data.vector))
    }
    if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
      return(cor((sym.var.x$var.data.vector[, 1] + sym.var.x$var.data.vector[
        ,
        2
      ]) / 2, (sym.var.y$var.data.vector[, 1] + sym.var.y$var.data.vector[
        ,
        2
      ]) / 2))
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
  if (method == "billard") {
    if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
      return(sym.cov(sym.var.x, sym.var.y, method = "billard") / (sym.sd(sym.var.x,
        method = "billard"
      ) * sym.sd(sym.var.y, method = "billard")))
    }
  }
}
