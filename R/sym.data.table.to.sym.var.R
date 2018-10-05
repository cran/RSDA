#' sym.data.table.to.sym.var
#' @keywords internal
sym.data.table.to.sym.var <- function(sym.var) {
  new.sym.var <- list()
  new.sym.var$N <- sym.var$N
  new.sym.var$var.name <- sym.var$sym.var.names
  new.sym.var$var.type <- sym.var$sym.var.types
  new.sym.var$obj.names <- sym.var$sym.obj.names
  pos <- sym.var$sym.var.starts
  adv <- sym.var$sym.var.length
  new.sym.var$var.data.vector <- sym.var$meta[, (pos:(pos + adv - 1))]
  return(new.sym.var)
}
