#' @keywords Intervals
#' @export
#'
sym.tsne <- function(sym.data, initial_config = NULL, k = 2, initial_dims = 30, perplexity = 30,
                     max_iter = 1000, min_cost = 0, epoch_callback = NULL, whiten = TRUE,
                     epoch=100) {
  UseMethod("sym.tsne")
}

#' @keywords Intervals
#'
expand_rows <- function(df){
  l <- lapply(seq_len(ncol(df)), function(x) list(1,2))
  df_i <- expand.grid(l)
  funs <- list(min,max)
  funs <- lapply(df_i, function(i) funs[unlist(i)])

  out <- lapply(seq_len(nrow(df)), function(i){
    fila <- df[i,]
    out <- lapply(seq_along(funs), function(i) {
      unlist(lapply(funs[[i]], function(.f) .f(fila[[i]])))
    })
    out <- as.data.frame(do.call(cbind.data.frame, out))
    colnames(out) <- colnames(df)
    return(out)
  })
  out <- as.data.frame(do.call(rbind.data.frame, out))
  colnames(out) <- colnames(df)
  return(out)
}

#' TSNE for symbolic data tables
#' @rdname sym.tsne
#' @aliases sym.tsne
#' @param sym.data symbolic data table
#' @param initial_config an argument providing a matrix specifying the initial embedding for X.
#' @param k the dimension of the resulting embedding.
#' @param initial_dims The number of dimensions to use in reduction method.
#' @param perplexity Perplexity parameter. (optimal number of neighbors)
#' @param max_iter Maximum number of iterations to perform.
#' @param min_cost The minimum cost value (error) to halt iteration.
#' @param epoch_callback A callback function used after each epoch (an epoch here means a set number of iterations)
#' @param whiten  A boolean value indicating whether the matrix data should be whitened.
#' @param epoch The number of iterations in between update messages.

#' @export
#' @usage sym.tsne(sym.data, initial_config = NULL, k = 2, initial_dims = 30,
#'  perplexity = 30,max_iter = 1000, min_cost = 0, epoch_callback = NULL,
#'  whiten = TRUE,epoch=100)
#' @import tsne
#' @examples
#' \dontrun{
#' res <- sym.tsne(oils)
#' res
#' plot(res)
#'}
#'
sym.tsne.symbolic_tbl <- function(sym.data, initial_config = NULL, k = 2, initial_dims = 30, perplexity = 30,
                                  max_iter = 1000, min_cost = 0, epoch_callback = NULL, whiten = TRUE,
                                  epoch=100){
  ext <- expand_rows(sym.data)
  res_tsne <- tsne::tsne(ext, initial_config,k, initial_dims, perplexity,
               max_iter, min_cost, epoch_callback, whiten,
               epoch)
  res_tsne <- as.data.frame(res_tsne)

  class(res_tsne) <- c("sym_tsne",class(res_tsne))
  attr(res_tsne,"names_tsne") <- attr(sym.data,"row.names")
  return(res_tsne)
}

#' Plot TSNE for symbolic data tables
#' @param  x data frame
#' @param ... other graphical parameters (see par and section ‘Details’ below).
#' @export
#' @import ggplot2
#' @importFrom dplyr group_by summarise
#'
plot.sym_tsne <- function(x,...){
  l <- length(attr(x, "names_tsne"))
  x$group <- sort(rep(1:l,(nrow(x)/l)))

  x2 <- x %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(xmin = min(V1), xmax = max(V1),
                     ymin = min(V2), ymax = max(V2))
  x2$var <- attr(x, "names_tsne")

  ggplot(
    data = x2,
    mapping = aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      color = as.factor(var)
    )
  ) +
    geom_rect(fill = NA, alpha = 0.7,show.legend = FALSE, size = 1) +
    geom_text(aes(x=xmin+(xmax-xmin)/2, y=ymin+(ymax-ymin)/2, label=var), size=6,
              show.legend = FALSE) +
    theme_minimal() +
    labs(x = "dim.1",
         y = "dim.2")
}


