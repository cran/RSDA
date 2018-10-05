#' Generate a Symbolic Data Table
#' @name generate.sym.table
#' @aliases generate.sym.table
#' @author Oldemar Rodriguez Rojas
#' @description This function generates a symbolic data table from a CSV data file.
#' @usage generate.sym.table(sym.data, file, sep, dec, row.names = NULL, col.names = NULL)
#'
#' @param sym.data Symbolic data table.
#' @param file The name of the CSV file.
#' @param sep As in R function read.table.
#' @param dec As in R function read.table.
#' @param row.names As in R function read.table.
#' @param col.names As in R function read.table.
#'
#' @return Return a symbolic data table.
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#'
#' @examples
#' data(example1)
#' generate.sym.table(example1, file='temp4.csv', sep='|',dec='.', row.names=TRUE,
#'                    col.names=TRUE)
#' ex1 <- read.sym.table('temp4.csv', header=TRUE, sep='|',dec='.', row.names=1)
#' @keywords Symbolic Table
#' @export
#' @importFrom utils write.table
#'
generate.sym.table <- function(sym.data, file, sep, dec, row.names = NULL, col.names = NULL) {
  ncolumn <- rep(sym.data$sym.var.types[1], sym.data$N)
  temp.data <- cbind(temp = ncolumn, sym.data$data[, 1:ncol(sym.data$data)])
  colnames(temp.data)[1] <- sym.data$sym.var.types[1]
  pos <- 1 # pos = valid number of columns inserted
  if ((sym.data$sym.var.types[1] == "$C") || (sym.data$sym.var.types[1] == "$c")) {
    pos <- 2
  }
  if ((sym.data$sym.var.types[1] == "$I") || (sym.data$sym.var.types[1] == "$i")) {
    pos <- 3
  }
  if ((sym.data$sym.var.types[1] == "$H") || (sym.data$sym.var.types[1] == "$h") ||
    (sym.data$sym.var.types[1] == "$S") || (sym.data$sym.var.types[1] == "$s")) {
    ncolumn <- rep(sym.data$sym.var.length[1], sym.data$N)
    temp.data <- cbind(temp.data[, 1:pos], temp = ncolumn, temp.data[, (pos + 1):ncol(temp.data)])
    colnames(temp.data)[(pos + 1)] <- sym.data$sym.var.names[1]
    pos <- pos + 1 + sym.data$sym.var.length[1]
  }
  for (j in 2:sym.data$M) {
    ncolumn <- rep(sym.data$sym.var.types[j], sym.data$N)
    temp.data <- cbind(temp.data[, 1:pos], temp = ncolumn, temp.data[, (pos + 1):ncol(temp.data)])
    colnames(temp.data)[pos + 1] <- sym.data$sym.var.types[j]
    pos <- pos + 1
    if ((sym.data$sym.var.types[j] == "$C") || (sym.data$sym.var.types[j] == "$c")) {
      pos <- pos + 1
    }
    if ((sym.data$sym.var.types[j] == "$I") || (sym.data$sym.var.types[j] == "$i")) {
      pos <- pos + 2
    }
    if ((sym.data$sym.var.types[j] == "$H") || (sym.data$sym.var.types[j] == "$h") ||
      (sym.data$sym.var.types[j] == "$S") || (sym.data$sym.var.types[j] == "$s")) {
      ncolumn <- rep(sym.data$sym.var.length[j], sym.data$N)
      temp.data <- cbind(temp.data[, 1:pos], temp = ncolumn, temp.data[, (pos +
        1):ncol(temp.data)])
      colnames(temp.data)[(pos + 1)] <- sym.data$sym.var.names[j]
      pos <- pos + 1 + sym.data$sym.var.length[j]
    }
  }
  write.table(temp.data, file,
    sep = as.character(sep), dec = dec, quote = FALSE,
    row.names = c(row.names), col.names = c(col.names)
  )
}
