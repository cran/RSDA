#' Plot Interval Scatterplot
#'
#' @param x value
#' @param y value
#' @param sym.data value
#' @param pos.var value
#'
cfa.scatterplot.gg <- function(x,y, sym.data, pos.var) {
  var.names <- c()
  n.vars <- c()
  for (i in pos.var) {
    var.names <- c(var.names, sym.data[, i]$sym.var.names)
    n.vars <- c(n.vars, ncol(sym.data[, i]$data))
  }
  df <- cbind(x$data,y$data)
  colnames(df) <- c("C1","C1.1","C2","C2.1")
  df$var.name <- purrr::flatten_chr(purrr::map2(var.names, n.vars, ~rep(.x,.y)))
  df$cat.name <- row.names(df)

  ggplot(data = df) +
    geom_rect( mapping = aes(xmin = C1, xmax = C1.1, ymin = C2, ymax = C2.1, color = var.name), fill = NA, size = 1) +
    geom_text(aes(label = cat.name, x = C1, y = C2)) +
    theme_minimal() +
    labs(color = "Variables") +
    theme(legend.position = "bottom")
}
