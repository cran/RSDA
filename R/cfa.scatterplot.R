#' CFA Symbolic Scatter Plot
#' @aliases cfa.scatterplot
#' @name cfa.scatterplot
#' @author Oldemar Rodriguez Rojas
#' @description This function could be use to plot two
#' symbolic variables in a X-Y plane to special case
#' of Symbolic Correspondance Analysis.
#'
#' @usage cfa.scatterplot(sym.var.x, sym.var.y, num.gr1=0, labels = TRUE, ...)
#' @param sym.var.x First symbolic variable
#' @param sym.var.y First symbolic variable
#' @param num.gr1 Number of modes of the first variable
#' @param labels As in R plot function.
#' @param ... As in R plot function.
#'
#' @references Rodriguez, O. (2011).Correspondence Analysis for Symbolic MultiValued Variables.
#'  Workshop in Symbolic Data AnalysisNamur, Belgium.
#' @return Return a graphics.
#' @seealso sym.cfa
#' @keywords CFA Plot
#' @export
#' @import graphics
#'
cfa.scatterplot <- function(sym.var.x, sym.var.y, num.gr1 = 0, labels = TRUE, ...) {
  if (((sym.var.x$sym.var.types != "$C") || (sym.var.y$sym.var.types != "$C")) && ((sym.var.x$sym.var.types !=
    "$I") || (sym.var.y$sym.var.types != "$I"))) {
    stop("Impossible to plot this type of variable")
  }
  if ((sym.var.x$sym.var.types == "$C") && (sym.var.y$sym.var.types == "$C")) {
    if (labels == FALSE) {
      plot(sym.var.x$data, sym.var.y$data,
        xlab = sym.var.x$sym.var.names,
        ylab = sym.var.y$sym.var.names, ...
      )
    } else {
      ltext <- sym.var.x$sym.obj.names
      plot(sym.var.x$data, sym.var.y$data,
        type = "n",
        xlab = sym.var.x$sym.var.names, ylab = sym.var.y$sym.var.names, ...
      )
      text(sym.var.x$data, sym.var.y$data, ltext)
    }
  }
  if ((sym.var.x$sym.var.types == "$I") && (sym.var.y$sym.var.types == "$I")) {
    xmin1 <- min(sym.var.x$data[, 1])
    xmin2 <- min(sym.var.x$data[, 2])
    xmin <- min(xmin1, xmin2)
    xmax1 <- max(sym.var.x$data[, 1])
    xmax2 <- max(sym.var.x$data[, 2])
    xmax <- max(xmax1, xmax2)
    ymin1 <- min(sym.var.y$data[, 1])
    ymin2 <- min(sym.var.y$data[, 2])
    ymin <- min(ymin1, ymin2)
    ymax1 <- max(sym.var.y$data[, 1])
    ymax2 <- max(sym.var.y$data[, 2])
    ymax <- max(ymax1, ymax2)
    plot(c(xmin, xmax), c(ymin, ymax),
      type = "n", xlab = sym.var.x$sym.var.names, ylab = sym.var.y$sym.var.names,
      ...
    )
    for (i in 1:sym.var.x$N) {
      x1 <- sym.var.x$data[i, 1]
      y1 <- sym.var.y$data[i, 1]
      x2 <- sym.var.x$data[i, 2]
      y2 <- sym.var.y$data[i, 2]
      if (i <= num.gr1) {
        rect(x1, y1, x2, y2, lwd = 2, border = "red")
      } else {
        rect(x1, y1, x2, y2, lwd = 2, border = "blue")
      }
    }
    if (labels == TRUE) {
      ltext <- sym.var.x$sym.obj.names
      text(jitter(sym.var.x$data[, 1]), jitter(sym.var.y$data[
        ,
        1
      ]), ltext)
    }
  }
}
