#' Symbolic Scatter GGPlot
#' @name sym.scatterplot.ggplot
#' @aliases sym.scatterplot.ggplot
#' @author Oldemar Rodriguez Rojas
#' @description This function could be use to plot two symbolic variables in a X-Y plane using ggplot
#' R package.
#' @usage sym.scatterplot.ggplot(sym.var.x, sym.var.y, labels = FALSE, ...)
#' @param sym.var.x First symbolic variable.
#' @param sym.var.y Second symbolic variable.
#' @param labels As in ggplot.
#' @param ... As in ggplot.
#'
#' @return return a ggplot graphic.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @seealso sym.scatterplot
#' @examples
#' data(lynne1)
#' class(lynne1) <- c('sym.data.table')
#' sym.scatterplot.ggplot(lynne1[,1], lynne1[,3],labels=TRUE)
#' data(oils)
#' sym.scatterplot.ggplot(oils[,2], oils[,3],labels=TRUE)
#' @keywords Symbolic ggplot
#' @export
#' @import ggplot2
#'
sym.scatterplot.ggplot <- function(sym.var.x, sym.var.y, labels = FALSE, 
    ...) {
    x <- 0
    y <- 0
    xmin <- 0
    xmax <- 0
    ymin <- 0
    ymax <- 0
    if (((sym.var.x$sym.var.types != "$C") || (sym.var.y$sym.var.types != 
        "$C")) && ((sym.var.x$sym.var.types != "$I") || (sym.var.y$sym.var.types != 
        "$I"))) 
        stop("Impossible to plot this type of variable")
    if ((sym.var.x$sym.var.types == "$C") && (sym.var.y$sym.var.types == 
        "$C")) {
        df <- data.frame(sym.var.x$data, sym.var.y$data)
        names(df) <- c("x", "y")
        p <- ggplot(df, aes(x, y)) + labs(x = sym.var.x$sym.var.names, 
            y = sym.var.y$sym.var.names)
        if (labels == FALSE) {
            p <- p + geom_point()
        } else {
            ltext <- sym.var.x$sym.obj.names
            p <- p + geom_text(label = ltext)
        }
    }
    if ((sym.var.x$sym.var.types == "$I") && (sym.var.y$sym.var.types == 
        "$I")) {
        df <- data.frame(sym.var.x$data, sym.var.y$data)
        names(df) <- c("xmin", "xmax", "ymin", "ymax")
        p <- ggplot(df) + labs(x = sym.var.x$sym.var.names, y = sym.var.y$sym.var.names) + 
            geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                ymax = ymax), fill = alpha(1:sym.var.x$N, 2/3))
        
        if (labels == TRUE) {
            ltext <- sym.var.x$sym.obj.names
            p <- p + geom_text(aes((xmin + xmax)/2, (ymin + ymax)/2), 
                label = ltext)
        }
    }
    print(p)
    invisible(p)
}
