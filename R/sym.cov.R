#' Symbolic Covariance
#' @name sym.cov
#' @aliases sym.cov
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic covariance.
#' @usage sym.cov(sym.var.x, sym.var.y, method = c('centers', 'interval', 'billard', 'modal'),
#' na.rm = FALSE, ...)
#' @param sym.var.x First symbolic variables.
#' @param sym.var.y Second symbolic variables.
#' @param method The method to be use.
#' @param na.rm As in R cov function.
#' @param ... As in R cov function.
#'
#' @return Return a real number.
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
#' sym.cov(sym.var(sym.data,1),sym.var(sym.data,4),method='centers')
#' sym.cov(sym.var(sym.data,2),sym.var(sym.data,6),method='centers')
#' sym.cov(sym.var(sym.data,2),sym.var(sym.data,6),method='billard')
#'
#' @keywords Symbolic Covariance
#'
#'
sym.cov <- function(sym.var.x, sym.var.y, method = c(
                      "centers", "interval", "billard",
                      "modal"
                    ), na.rm = FALSE, ...) {
  Gj <- function(a, b, vmean) {
    if ((a + b) / 2 <= vmean) {
      return(-1)
    } else {
      return(1)
    }
  }
  Qj <- function(a, b, vmean) {
    return((a - vmean)^2 + (a - vmean) * (b - vmean) + (b - vmean)^2)
  }
  method <- match.arg(method)
  if (method == "centers") {
    if ((sym.var.x$var.type == "$C") && (sym.var.y$var.type == "$C")) {
      return(cov(sym.var.x$var.data.vector, sym.var.y$var.data.vector))
    }
    if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
      return(cov((sym.var.x$var.data.vector[, 1] + sym.var.x$var.data.vector[
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
      ss <- 0
      vmean.x <- sym.mean(sym.var.x, method = "centers")
      vmean.y <- sym.mean(sym.var.y, method = "centers")
      for (i in 1:(sym.var.x$N)) {
        ss <- ss + Gj(sym.var.x$var.data.vector[i, 1], sym.var.x$var.data.vector[
          i,
          2
        ], vmean.x) * Gj(sym.var.y$var.data.vector[i, 1], sym.var.y$var.data.vector[
          i,
          2
        ], vmean.y) * sqrt(Qj(sym.var.x$var.data.vector[i, 1], sym.var.x$var.data.vector[
          i,
          2
        ], vmean.x) * Qj(sym.var.y$var.data.vector[i, 1], sym.var.y$var.data.vector[
          i,
          2
        ], vmean.y))
      }
      return((1 / (3 * sym.var.x$N)) * ss)
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
}
