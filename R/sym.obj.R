#' Symbolic Object
#' @name sym.obj
#' @aliases sym.obj
#' @author Oldemar Rodriguez Rojas
#' @description This function get a symbolic object (row or a case) from a symbolic data table.
#' @usage sym.obj(sym.data, number.sym.obj)
#' @param sym.data Symboli data matrix.
#' @param number.sym.obj The number of the row for the symbolic object (case) that we want to get.
#'
#' @return
#' Return a symbolic object with the following internal format: \cr
#'
#' $M\cr
#'
#' [1] 5\cr
#'
#' $var.types\cr
#'
#' [1] '$C' '$H' '$I' '$H' '$C'\cr
#'
#' $var.length\cr
#'
#' [1] 1 5 2 3 1\cr
#'
#' $var.names\cr
#'
#' [1] 'F1' 'F2' 'F3' 'F4' 'F5'\cr
#'
#' $obj.data.vector\cr
#'
#' F1  M1  M2  M3  M4  M5 F3 F3.1 M1.1 M2.1 M3.1 F5\cr
#'
#' Case4 -2.1 0.4 0.1 0.1 0.1 0.3  0    2  0.9    0  0.1  0\cr
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#'
#' @examples
#' data(example7)
#' sym.obj(example7,4)
#' @keywords Symbolic Object
#' @export
#'
sym.obj <- function(sym.data, number.sym.obj) {
  if ((number.sym.obj > sym.data$N) || (number.sym.obj <= 0)) {
    stop("number.sym.obj out of range")
  }
  sym.obj <- list(
    M = sym.data$M, var.types = sym.data$sym.var.types, var.length = sym.data$sym.var.length,
    var.names = sym.data$sym.var.names, obj.data.vector = sym.data$data[number.sym.obj, ]
  )
  return(sym.obj)
}
