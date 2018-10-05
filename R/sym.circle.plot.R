#' Symbolic Circle of Correlations
#' @name sym.circle.plot
#' @aliases sym.circle.plot
#' @author Oldemar Rodriguez Rojas
#' @description Plot the symbolic circle of correlations.
#' @usage sym.circle.plot(prin.corre)
#' @param prin.corre A symbolic interval data matrix with correlations between the variables and the
#' principals componets, both of interval type.
#'
#' @return Plot the symbolic circle
#' @references
#' Rodriguez O. (2012). The Duality Problem in Interval Principal Components Analysis.
#' The 3rd Workshop in Symbolic Data Analysis, Madrid.
#'
#' @examples
#' data(oils)
#' res<-sym.interval.pca(oils,'centers')
#' sym.circle.plot(res$Sym.Prin.Correlations)
#'
#' @keywords Symbolic Circle
#' @export
#'
sym.circle.plot <- function(prin.corre) {
  v <- c(
    "green", "red", "blue", "cyan", "brown", "yellow", "pink", "purple", "orange",
    "gray"
  )
  msg <- paste("Correlation Circle")
  plot(-1.5:1.5, -1.5:1.5, type = "n", xlab = "C1", ylab = "C2", main = msg)
  abline(h = 0, lty = 3)
  abline(v = 0, lty = 3)
  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
  c1 <- 1
  c2 <- 2
  n <- dim(prin.corre)[1]
  f <- dim(prin.corre)[2]
  CRTI <- matrix(nrow = n, ncol = f)
  CRTI <- prin.corre
  vars <- rownames(prin.corre)
  for (k in 1:n) {
    x1 <- min(CRTI[k, c1], CRTI[k, c2])
    x2 <- max(CRTI[k, c1], CRTI[k, c2])
    y1 <- min(CRTI[k, c2 + 1], CRTI[k, c2 + 2])
    y2 <- max(CRTI[k, c2 + 1], CRTI[k, c2 + 2])
    if (((x1 > 0) && (x2 > 0) && (y1 > 0) && (y2 > 0)) || ((x1 < 0) && (x2 < 0) &&
      (y1 < 0) && (y2 < 0))) {
      plotX.slice(x1, y2, x2, y1, v, vars, k)
    }
    if (((x1 < 0) && (x2 < 0) && (y1 > 0) && (y2 > 0)) || ((x1 > 0) && (x2 > 0) &&
      (y1 < 0) && (y2 < 0))) {
      plotX.slice(x1, y1, x2, y2, v, vars, k)
    }
    if ((y1 > 0) && (y2 > 0) && (x1 < 0) && (x2 > 0)) {
      plotX.slice(x1, y1, x2, y1, v, vars, k)
    }
    if ((y1 < 0) && (y2 < 0) && (x1 < 0) && (x2 > 0)) {
      plotX.slice(x1, y2, x2, y2, v, vars, k)
    }
    if ((x1 > 0) && (x2 > 0) && (y1 < 0) && (y2 > 0)) {
      plotX.slice(x1, y1, x1, y2, v, vars, k)
    }
    if ((x1 < 0) && (x2 < 0) && (y1 < 0) && (y2 > 0)) {
      plotX.slice(x2, y1, x2, y2, v, vars, k)
    }
    if ((x1 < 0) && (x2 > 0) && (y1 < 0) && (y2 > 0)) {
      plotX.slice(x2, y1, x2, y2, v, vars, k)
    }
  }
}
