#' principal.axis.i
#' @keywords internal
principal.axis.i <- function(rotation.matrix, comp.i, eje.x, eje.y) {
  pos <- c(eje.x, eje.y)
  x1 <- rotation.matrix[pos, comp.i]
  b <- x1[2] / x1[1]
  a <- 0
  return(data.frame(a = a, b = b))
}
