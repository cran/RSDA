#' Generic function for the covariance
#' @name cov
#' @aliases cov
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic covariance.
#' @param x First symbolic variables.
#' @param y Second symbolic variables.
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
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
#' sym.data <- example3
#' cov(sym.data[,1], sym.data[,4], method='centers')
#' cov(sym.data[,2],sym.data[,6], method='centers')
#' cov(sym.data[,2],sym.data[,6], method='billard')
#'
#' @keywords Symbolic Covariance
#'
cov <- function(x, ...) {
  UseMethod("cov", x)
}

#' @rdname cov
#' @export
cov.default <- function(x, y = NULL, use = "everything", method = c(
                          "pearson", "kendall",
                          "spearman"
                        ), ...) {
  stats::cov(x, y, use, method)
}

#' @rdname cov
#' @export
cov.sym.data.table <- function(x, y, method = c("centers", "interval", "billard", "modal"),
                               na.rm = FALSE, ...) {
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
    if ((x$sym.var.types == "$C") && (y$sym.var.types == "$C")) {
      return(cov(x$data[, 1], y$data[, 1]))
    }
    if ((x$sym.var.types == "$I") && (y$sym.var.types == "$I")) {
      return(cov((x$data[, 1] + x$data[, 2]) / 2, (y$data[, 1] + y$data[, 2]) / 2))
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
  if (method == "billard") {
    if ((x$sym.var.types == "$I") && (y$sym.var.types == "$I")) {
      ss <- 0
      vmean.x <- mean(x, method = "centers")
      vmean.y <- mean(y, method = "centers")
      for (i in 1:(x$N)) {
        ss <- ss + Gj(x$data[i, 1], x$data[i, 2], vmean.x) * Gj(
          y$data[i, 1],
          y$data[i, 2], vmean.y
        ) * sqrt(Qj(x$data[i, 1], x$data[i, 2], vmean.x) *
          Qj(y$data[i, 1], y$data[i, 2], vmean.y))
      }
      return((1 / (3 * x$N)) * ss)
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
}
