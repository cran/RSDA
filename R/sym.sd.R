#' Symbolic Standard Desviation
#' @name sym.sd
#' @aliases sym.sd
#' @author Oldemar Rodriguez Rojas
#' @description Compute the symbolic standard desviation.
#' @usage sym.sd(sym.var, method =
#' c('centers', 'interval', 'billard', 'modal'), na.rm = FALSE, ...)
#' @param sym.var The symbolic variable.
#' @param method The method to be use.
#' @param na.rm As in R sd function.
#' @param ... As in R sd function.
#'
#' @return return a real number.
#' @references Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @examples
#' data(example3)
#' sym.data<-example3
#' sym.sd(sym.var(sym.data,1))
#' sym.sd(sym.var(sym.data,2))
#' sym.sd(sym.var(sym.data,6))
#' sym.sd(sym.var(sym.data,6),method='interval')
#' sym.sd(sym.var(sym.data,6),method='billard')
#' sym.sd(sym.var(sym.data,3),method='modal')
#' @keywords Symbolic sd
#' @export
#'
sym.sd <- function(sym.var, method = c("centers", "interval", "billard", "modal"),
                   na.rm = FALSE, ...) {
  method <- match.arg(method)
  if (method == "centers") {
    if (sym.var$var.type == "$C") {
      return(sd(sym.var$var.data.vector))
    }
    if (sym.var$var.type == "$I") {
      return(sd(sym.var$var.data.vector[, 1] + sym.var$var.data.vector[, 2]) / 2)
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
  if (method == "interval") {
    if (sym.var$var.type == "$I") {
      return(sapply(sym.var$var.data.vector, sd))
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
  if (method == "billard") {
    if (sym.var$var.type == "$I") {
      return(sqrt((1 / (3 * sym.var$N)) * sum(sym.var$var.data.vector[, 1]^2 +
        (sym.var$var.data.vector[, 1] * sym.var$var.data.vector[, 2]) + sym.var$var.data.vector[
          ,
          2
        ]^2) - (1 / (4 * (sym.var$N)^2)) * sum(sym.var$var.data.vector[, 1] +
        sym.var$var.data.vector[, 2])^2))
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
  if (method == "modal") {
    if (sym.var$var.type == "$M") {
      return(sapply(sym.var$var.data.vector, sd))
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
}
