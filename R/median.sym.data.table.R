#' Symbolic Median
#' @name median.sym.data.table
#' @aliases median.sym.data.table
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic median.
#' @param x The symbolic variable.
#' @param method The method to be use.
#' @param na.rm As in R median function.
#' @param ... As in R median function.
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
#  median(sym.data[,1])
#' median(sym.data[,2])
#' median(sym.data[,6] ,method='interval')
#' median(sym.data[,3] ,method='modal')
#' @keywords Symbolic Median
#' @export
#'
median.sym.data.table <- function(x, na.rm = FALSE, method = c("centers",
    "interval", "modal"), ...) {
    method <- match.arg(method)
    if (method == "centers") {
        if (x$sym.var.types == "$C")
            return(median(x$data[, 1]))
        if (x$sym.var.types == "$I")
            return(median(x$data[, 1] + x$data[, 2])/2) else stop("Impossible to compute the median for this type of variable with this method")
    }
    if (method == "interval") {
        if (x$sym.var.types == "$I")
            return(sapply(x$data, median)) else stop("Impossible to compute the median for this type of variable with this method")
    }
    if (method == "modal") {
        if (x$sym.var.types == "$M")
            return(sapply(x$data, median)) else stop("Impossible to compute the median for this type of variable with this method")
    }
}
