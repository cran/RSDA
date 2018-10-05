#' display.sym.table
#' @name display.sym.table
#' @aliases display.sym.table
#' @author Oldemar Rodriguez Rojas
#' @description This function display a symbolic data table tha have been read by read.sym.table(...)
#' @usage display.sym.table(sym.data)
#' @param sym.data Shoud be a Symbolic Data table that have been read with read.sym.table(...)
#' @details The output will be the symbolic data table in the screen.
#'
#' @return Not value.
#' @references Billard, L and  Diday, E. (2007). Symbolic Data Analysis: Conceptual Statistics and Data
#' Mining (Wiley Series in Computational Statistics).
#' Billard, L and  Diday, E. (2003). From the Statistics of Data to the Statistics of
#' Knowledge: Symbolic Data Analysis. Journal of the American of the Statistical Association, USA.
#' @examples
#' data(example3)
#' display.sym.table(example3)
#' @seealso read.sym.table
#' @keywords Display Symbolic Table
#' @export
#'
display.sym.table <- function(sym.data) {
  return(sym.data$meta)
}
