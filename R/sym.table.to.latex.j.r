#' sym.table.latex
#' @keywords internal
sym.table.latex <- function(sym.data) {
  return(xtable(generate.sym.table(sym.data)))
}
