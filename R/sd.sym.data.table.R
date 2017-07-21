#' Generic function for the standard desviation
#' @name sd
#' @aliases sd
#' @author Oldemar Rodriguez Rojas
#' @description Compute the symbolic standard desviation.
#' @param x A symbolic variable.
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
#' sd(sym.data[,1])
#' sd(sym.data[,2])
#' sd(sym.data[,6])
#' sd(sym.data[,6], method='interval')
#' sd(sym.data[,6], method='billard')
#' sd(sym.data[,3],method='modal')
#' @keywords Symbolic sd
#' @export
#'
sd <- function(x, ...) {
    UseMethod("sd", x)
}

#' @rdname sd
#' @export
sd.default <- function(x, na.rm = FALSE, ...) {
    stats::sd(x, na.rm)
}


#' @rdname sd
#' @export
sd.sym.data.table <- function(x, method = c("centers", "interval", "billard", "modal"), 
    na.rm = FALSE, ...) {
    method <- match.arg(method)
    if (method == "centers") {
        if (x$sym.var.types == "$C") 
            return(sd(x$data[, 1]))
        if (x$sym.var.types == "$I") 
            return(sd(x$data[, 1] + x$data[, 2])/2) else stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
    if (method == "interval") {
        if (x$sym.var.types == "$I") 
            return(sapply(x$data, sd)) else stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
    if (method == "billard") {
        if (x$sym.var.types == "$I") 
            return(sqrt((1/(3 * x$N)) * sum(x$data[, 1]^2 + (x$data[, 1] * x$data[, 
                2]) + x$data[, 2]^2) - (1/(4 * (x$N)^2)) * sum(x$data[, 1] + x$data[, 
                2])^2)) else stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
    if (method == "modal") {
        if (x$sym.var.types == "$M") 
            return(sapply(x$data, sd)) else stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
}
