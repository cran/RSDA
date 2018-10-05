#' Symbolic Mean
#' @name mean.sym.data.table
#' @aliases  mean.sym.data.table
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic mean
#' @param x The symbolic variable.
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
#' mean(sym.data[,1])
#' mean(sym.data[,2])
#' mean(sym.data[,2], method='interval')
#' mean(sym.data[,3], method='modal')
#'
#' @keywords Symbolic Mean
#' @export
#' @exportMethod
#'
mean.sym.data.table <- function(x, method = c("centers", "interval", "modal"), trim = 0,
                                na.rm = F, ...) {
  method <- match.arg(method)
  if (method == "centers") {
    if (x$sym.var.types == "$C") {
      return(mean(x$data[, 1], trim, na.rm, ...))
    }
    if (x$sym.var.types == "$I") {
      return(mean((x$data[, 1] + x$data[, 2]) / 2, ...))
    } else {
      stop("Impossible to compute the mean for this type of variable with this method")
    }
  }
  if (method == "interval") {
    if (x$sym.var.types == "$I") {
      return(colMeans(x$data))
    } else {
      stop("Impossible to compute the mean for this type of variable with this method")
    }
  }
  if (method == "modal") {
    if (x$sym.var.types == "$M") {
      return(colMeans(x$data))
    } else {
      stop("Impossible to compute the mean for this type of variable with this method")
    }
  }
}
