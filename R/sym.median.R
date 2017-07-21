#' Symbolic Median
#' @name sym.median
#' @aliases sym.median
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic median.
#' @usage sym.median(sym.var, method = c('centers', 'interval', 'modal'), na.rm = FALSE, ...)
#' @param sym.var The symbolic variable.
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
#' sym.median(sym.var(sym.data,1))
#' sym.median(sym.var(sym.data,2))
#' sym.median(sym.var(sym.data,6),method='interval')
#' sym.median(sym.var(sym.data,3),method='modal')
#' @keywords Symbolic Median
#' @export
#'
sym.median <- function(sym.var, method = c("centers", "interval", "modal"), na.rm = FALSE, 
    ...) {
    method <- match.arg(method)
    if (method == "centers") {
        if (sym.var$var.type == "$C") 
            return(median(sym.var$var.data.vector, na.rm))
        if (sym.var$var.type == "$I") 
            return(median(sym.var$var.data.vector[, 1] + sym.var$var.data.vector[, 
                2])/2) else stop("Impossible to compute the median for this type of variable with this method")
    }
    if (method == "interval") {
        if (sym.var$var.type == "$I") 
            return(sapply(sym.var$var.data.vector, median)) else stop("Impossible to compute the median for this type of variable with this method")
    }
    if (method == "modal") {
        if (sym.var$var.type == "$M") 
            return(sapply(sym.var$var.data.vector, median)) else stop("Impossible to compute the median for this type of variable with this method")
    }
}
