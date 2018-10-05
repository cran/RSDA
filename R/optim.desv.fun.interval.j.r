#' optim.desv.fun.interval
#' @keywords internal
optim.desv.fun.interval <- function(sym.var.1) {
  res.min <- cobyla(sym.var.1$var.data.vector[, 1], desv.fun, lower = sym.var.1$var.data.vector[
    ,
    1
  ], upper = sym.var.1$var.data.vector[, 2], nl.info = FALSE, control = list(
    xtol_rel = 1e-08,
    maxeval = 20000
  ))

  res.max <- cobyla(sym.var.1$var.data.vector[, 1], neg.desv.fun, lower = sym.var.1$var.data.vector[
    ,
    1
  ], upper = sym.var.1$var.data.vector[, 2], nl.info = FALSE, control = list(
    xtol_rel = 1e-08,
    maxeval = 20000
  ))


  tmp.df <- data.frame(res.min$value, -res.max$value)
  colnames(tmp.df) <- c("sd", "sd.1")
  row.names(tmp.df) <- sym.var.1$var.name
  return(list(
    N = 1, var.name = "sd", var.type = "$I", obj.names = sym.var.1$var.name,
    var.data.vector = tmp.df
  ))
}
