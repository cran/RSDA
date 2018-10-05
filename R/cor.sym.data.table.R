#' Generic function for the correlation
#' @name cor
#' @aliases cor
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic correlation
#' @param x A symbolic variable.
#' @param y A symbolic variable.
#' @param use An optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
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
#' sym.data <- example3
#' cor(sym.data[,1], sym.data[,4], method='centers')
#' cor(sym.data[,2], sym.data[,6], method='centers')
#' cor(sym.data[,2], sym.data[,6], method='billard')
#' @keywords Symbolic Correlation
cor <- function(x, ...) {
  UseMethod("cor", x)
}

#' @rdname cor
#' @export
cor.default <- function(x, y = NULL, use = "everything", method = c(
                          "pearson", "kendall",
                          "spearman"
                        ), ...) {
  stats::cor(x, y, use, method)
}

#' @rdname cor
#' @export
cor.sym.data.table <- function(x, y, method = c("centers", "interval", "billard", "modal"),
                               ...) {
  method <- match.arg(method)
  if (method == "centers") {
    if ((x$sym.var.types == "$C") && (y$sym.var.types == "$C")) {
      return(cor(x$data[, 1], y$data[, 1]))
    }
    if ((x$sym.var.types == "$I") && (y$sym.var.types == "$I")) {
      return(cor((x$data[, 1] + x$data[, 2]) / 2, (y$data[, 1] + y$data[, 2]) / 2))
    } else {
      stop("Impossible to compute the Standard Deviation for this type of variable with this method")
    }
  }
  if (method == "billard") {
    if ((x$sym.var.types == "$I") && (y$sym.var.types == "$I")) {
      return(cov(x, y, method = "billard") / (sd(x, method = "billard") * sd(y,
        method = "billard"
      )))
    }
  }
}
