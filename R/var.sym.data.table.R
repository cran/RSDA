#' Generic function for the Variance
#' @name var
#' @aliases var
#' @author Oldemar Rodriguez Rojas
#' @description Compute the symbolic variance.
#' @param x The symbolic variable.
#' @param y NULL (default) or a vector, matrix or data frame with
#' compatible dimensions to x. The default is equivalent to y = x (but more efficient).
#' @param use an optional character string giving a method for computing covariances
#' in the presence of missing values. This must be (an abbreviation of) one of the
#' strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#' or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param na.rm logical. Should missing values be removed?
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
#' var(sym.data[,1])
#' var(sym.data[,2])
#' var(sym.data[,6])
#' var(sym.data[,6], method='interval')
#' var(sym.data[,6], method='billard')
#' var(sym.data[,3], method='modal')
#' @keywords Symbolic Variance
#' @export
#'
var <- function(x, ...) {
    UseMethod("var", x)
}

#' @rdname var
#' @export
var.default <- function(x, y = NULL, na.rm = FALSE, use, ...) {
    stats::var(x, y, na.rm, use)
}

#' @rdname var
#' @export
var.sym.data.table <- function(x, method = c("centers", "interval", "billard", "modal"), 
    na.rm = FALSE, ...) {
    error.message <- "Impossible to compute the variance for this type of variable with this method"
    method <- match.arg(method)
    if (method == "centers") {
        if (x$sym.var.types == "$C") 
            return(var(x$data[, 1]))
        if (x$sym.var.types == "$I") {
            return(var(x$data[, 1] + x$data[, 2])/2)
        } else {
            stop(error.message)
        }
    }
    if (method == "interval") {
        if (x$sym.var.types == "$I") {
            return(sapply(x$data, var))
        } else {
            stop(error.message)
        }
    }
    if (method == "billard") {
        if (x$sym.var.types == "$I") {
            return((1/(3 * x$N)) * sum(x$data[, 1]^2 + (x$data[, 1] * x$data[, 2]) + 
                x$data[, 2]^2) - (1/(4 * (x$N)^2)) * sum(x$data[, 1] + x$data[, 2])^2)
        } else {
            stop(error.message)
        }
    }
    
    if (method == "modal") {
        if (x$sym.var.types == "$M") {
            return(sapply(x$data, var))
        } else {
            stop(error.message)
        }
    }
}
