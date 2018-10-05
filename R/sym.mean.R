#' Symbolic Mean
#' @name sym.mean
#' @aliases sym.mean
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic mean
#' @usage sym.mean(sym.var, method = c('centers', 'interval', 'modal'),
#' trim = 0, na.rm = FALSE, ...)
#' @param sym.var The symbolic variable.
#' @param method The method to be use.
#' @param trim As in R mean function.
#' @param na.rm As in R mean function.
#' @param ... As in R mean function.
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
#' sym.mean(sym.var(sym.data,1))
#' sym.mean(sym.var(sym.data,2))
#' sym.mean(sym.var(sym.data,2),method='interval')
#' sym.mean(sym.var(sym.data,3),method='modal')
#'
#' @keywords Symbolic Mean
#' @export
#'
#'
sym.mean <- function(sym.var, method = c("centers", "interval", "modal"), trim = 0,
                     na.rm = FALSE, ...) {
  method <- match.arg(method)
  if (method == "centers") {
    if (sym.var$var.type == "$C") {
      return(mean(sym.var$var.data.vector, trim, na.rm))
    }
    if (sym.var$var.type == "$I") {
      return(mean((sym.var$var.data.vector[, 1] + sym.var$var.data.vector[, 2]) / 2))
    } else {
      stop("Impossible to compute the mean for this type of variable with this method")
    }
  }
  if (method == "interval") {
    if (sym.var$var.type == "$I") {
      return(colMeans(sym.var$var.data.vector))
    } else {
      stop("Impossible to compute the mean for this type of variable with this method")
    }
  }
  if (method == "modal") {
    if (sym.var$var.type == "$M") {
      return(colMeans(sym.var$var.data.vector))
    } else {
      stop("Impossible to compute the mean for this type of variable with this method")
    }
  }
}
