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
#' @examples
#' data(ex_cfa1)
#' res<-sym.cfa(ex_cfa1)
#' cfa.scatterplot(sym.var(res,1), sym.var(res,2), num.gr1=ex_cfa1$N,
#' labels=TRUE, col='red',main='CFA')
#' @keywords CFA Plot
#' @export
#' @import graphics
#'
cfa.scatterplot <- function(sym.var.x, sym.var.y, num.gr1 = 0, labels = TRUE,
    ...) {
    if (((sym.var.x$var.type != "$C") || (sym.var.y$var.type != "$C")) &&
        ((sym.var.x$var.type != "$I") || (sym.var.y$var.type != "$I")))
        stop("Impossible to plot this type of variable")
    if ((sym.var.x$var.type == "$C") && (sym.var.y$var.type == "$C")) {
        if (labels == FALSE)
            plot(sym.var.x$var.data.vector, sym.var.y$var.data.vector,
                xlab = sym.var.x$var.name, ylab = sym.var.y$var.name,
                ...) else {
            ltext <- sym.var.x$obj.names
            plot(sym.var.x$var.data.vector, sym.var.y$var.data.vector,
                type = "n", xlab = sym.var.x$var.name, ylab = sym.var.y$var.name,
                ...)
            text(sym.var.x$var.data.vector, sym.var.y$var.data.vector,
                ltext)
        }
    }
    if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
        xmin1 <- min(sym.var.x$var.data.vector[, 1])
        xmin2 <- min(sym.var.x$var.data.vector[, 2])
        xmin <- min(xmin1, xmin2)
        xmax1 <- max(sym.var.x$var.data.vector[, 1])
        xmax2 <- max(sym.var.x$var.data.vector[, 2])
        xmax <- max(xmax1, xmax2)
        ymin1 <- min(sym.var.y$var.data.vector[, 1])
        ymin2 <- min(sym.var.y$var.data.vector[, 2])
        ymin <- min(ymin1, ymin2)
        ymax1 <- max(sym.var.y$var.data.vector[, 1])
        ymax2 <- max(sym.var.y$var.data.vector[, 2])
        ymax <- max(ymax1, ymax2)
        plot(c(xmin, xmax), c(ymin, ymax), type = "n", xlab = sym.var.x$var.name,
            ylab = sym.var.y$var.name, ...)
        for (i in 1:sym.var.x$N) {
            x1 <- sym.var.x$var.data.vector[i, 1]
            y1 <- sym.var.y$var.data.vector[i, 1]
            x2 <- sym.var.x$var.data.vector[i, 2]
            y2 <- sym.var.y$var.data.vector[i, 2]
            if (i <= num.gr1)
                rect(x1, y1, x2, y2, lwd = 2, border = "red") else rect(x1, y1, x2, y2, lwd = 2, border = "blue")
        }
        if (labels == TRUE) {
            ltext <- sym.var.x$obj.names
            text(jitter(sym.var.x$var.data.vector[, 1]), jitter(sym.var.y$var.data.vector[,
                1]), ltext)
        }
    }
}
