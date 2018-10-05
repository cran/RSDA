#' Symbolic Scatter Plot 3D
#' @name sym.scatterplot3d
#' @aliases sym.scatterplot3d
#' @author Oldemar Rodriguez Rojas
#' @description This function could be use to plot two symbolic variables in 3D i.e. in a X-Y-Z plane.
#' @usage sym.scatterplot3d(sym.var.x, sym.var.y, sym.var.z, labels = FALSE, ...)
#' @param sym.var.x First symbolic variable.
#' @param sym.var.y Second symbolic variable.
#' @param sym.var.z Third symbolic variable.
#' @param labels As in R plot function.
#' @param ... As in R plot function.
#'
#' @return 3D Plot graphic.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#'
#' @examples
#' data(lynne1)
#' class(lynne1) <- c('sym.data.table')
#' sym.scatterplot3d(lynne1[,1], lynne1[,2], lynne1[,3],
#'                  color='blue', main='Lynne Data')
#' @keywords Symbolic 3DPlot
#' @export
#' @importFrom scatterplot3d scatterplot3d
#'
sym.scatterplot3d <- function(sym.var.x, sym.var.y, sym.var.z, labels = FALSE, ...) {
  if (((sym.var.x$sym.var.types != "$C") || (sym.var.y$sym.var.types != "$C") ||
    (sym.var.z$sym.var.types != "$C")) && ((sym.var.x$sym.var.types != "$I") ||
    (sym.var.y$sym.var.types != "$I") || (sym.var.y$sym.var.types != "$I"))) {
    stop("Impossible to plot this type of variable")
  }
  if ((sym.var.x$sym.var.types == "$C") && (sym.var.y$sym.var.types == "$C") && (sym.var.z$sym.var.types ==
    "$C")) {
    if (labels == FALSE) {
      p <- scatterplot3d(sym.var.x$data[, 1], sym.var.y$data[, 1], sym.var.z$data[
        ,
        1
      ],
      xlab = sym.var.x$sym.var.names, ylab = sym.var.y$sym.var.names,
      zlab = sym.var.z$sym.var.names, ...
      )
    } else {
      p <- scatterplot3d(sym.var.x$data[, 1], sym.var.y$data[, 1], sym.var.z$data[
        ,
        1
      ],
      xlab = sym.var.x$sym.var.names, ylab = sym.var.y$sym.var.names,
      zlab = sym.var.z$sym.var.names, type = "n", ...
      )
      ltext <- sym.var.x$sym.obj.names
      text(p$xyz.convert(sym.var.x$data[, 1], sym.var.y$data[, 1], sym.var.z$data[
        ,
        1
      ]), labels = ltext)
    }
  }
  if ((sym.var.x$sym.var.types == "$I") && (sym.var.y$sym.var.types == "$I") && (sym.var.z$sym.var.types ==
    "$I")) {
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
    zmin1 <- min(sym.var.z$data[, 1])
    zmin2 <- min(sym.var.z$data[, 2])
    zmin <- min(zmin1, zmin2)
    zmax1 <- max(sym.var.z$data[, 1])
    zmax2 <- max(sym.var.z$data[, 2])
    zmax <- max(zmax1, zmax2)
    p <- scatterplot3d(c(xmin, xmax), c(ymin, ymax), c(zmin, zmax),
      xlab = sym.var.x$sym.var.names,
      ylab = sym.var.y$sym.var.names, zlab = sym.var.z$sym.var.names, type = "n",
      ...
    )
    cube <- rbind(c(1, 1, 1), c(2, 1, 1), c(2, 1, 2), c(1, 1, 2), c(1, 1, 1), c(
      1,
      2, 1
    ), c(1, 2, 2), c(2, 2, 2), c(2, 2, 1), c(1, 2, 1), c(1, 2, 2), c(
      1,
      1, 2
    ), c(2, 1, 2), c(2, 2, 2), c(2, 2, 1), c(2, 1, 1))
    for (i in 1:sym.var.x$N) {
      vec.x <- sym.var.x$data[i, cube[, 1]]
      vec.y <- sym.var.y$data[i, cube[, 2]]
      vec.z <- sym.var.z$data[i, cube[, 3]]

      p$points3d(vec.x, vec.y, vec.z, type = "l", lty = 1, col = i + 1)
    }
    if (labels == TRUE) {
      ltext <- sym.var.x$sym.obj.names
      textPoints <- cbind((sym.var.x$data[, 1] + sym.var.x$data[, 2]) / 2, (sym.var.y$data[
        ,
        1
      ] + sym.var.y$data[, 2]) / 2, (sym.var.z$data[, 1] + sym.var.z$data[
        ,
        2
      ]) / 2)
      text(p$xyz.convert(textPoints), labels = ltext)
    }
  }
}
