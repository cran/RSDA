#' Generic function for distance matrix computation
#' @name dist
#' @aliases dist
#' @author Oldemar Rodriguez Rojas
#' @param x An R object. Currently the are methods for numeric matrix, data.frame, dist object or symbolic data table
#'
#' @return dist returns an object of class 'dist'
#' @export
dist <- function(x, ...) {
  UseMethod("dist", x)
}

#' @param method the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". Any unambiguous substring can be given.
#' @param diag logical value indicating whether the diagonal of the distance matrix should be printed by print.dist.
#' @param upper logical value indicating whether the upper triangle of the distance matrix should be printed by print.dist.
#' @param p The power of the Minkowski distance.
#'
#' @rdname dist
#' @export
dist.default <- function(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2, ...) {
  stats::dist(x, method = method, diag = diag, upper = upper, p = p)
}


#' @param q q value for the Minkowski method
#' @param ... Further arguments passed to or from other methods
#'
#' @rdname dist
#' @export
dist.sym.data.table <- function(x, q = 2, ...) {
  if (length(unique(x$sym.var.types))) {
    if (x$sym.var.types[1] == "%I") {
      return(sym.dist.interval(x))
    } else if (x$sym.var.types[1] == "%S") {
      return(sym.dist.set(x))
    }
  }

  n <- x$N
  m <- x$M
  out <- matrix(0, nrow = n, ncol = n)
  colnames(out) <- x$sym.obj.names
  rownames(out) <- x$sym.obj.names
  dist.functions <- select.dist(x)
  for (i in seq_len(m)) {
    for (j in seq_len(n)) {
      for (k in seq_len(n)) {
        out[j, k] <- out[j, k] + dist.functions[[i]](x[j, i], x[k, i])
      }
    }
  }
  out <- out^(1 / q)
  out <- dist(out)
  return(out)
}

#' Select distance methods for a symbolic data table
#'
#' @param x A symbilic data table
#'
#' @keywords internal
select.dist <- function(x) {
  types <- x$sym.var.types
  dist.functions <- list()
  for (i in seq_along(types)) {
    switch(types[i],
      "$C" = {
        dist.functions[[i]] <- cont.distance
      },
      "$I" = {
        dist.functions[[i]] <- interval.distance
      },
      "$M" = {
        dist.functions[[i]] <- modal.distance
      },
      "$S" = {
        dist.functions[[i]] <- set.distance
      },
      "$H" = {
        dist.functions[[i]] <- hist.distance
      }
    )
  }
  return(dist.functions)
}

#' Distance between modal multivalued variables
#'
#' @param x A symbilic data table 1x1
#' @param y A symbilic data table 1x1
#'
#' @keywords internal
modal.distance <- function(x, y, q = 2) {
  1 - (sum(sqrt(x$data * y$data))^q)
}


#' Distance between set variables
#'
#' @param x A symbilic data table 1x1
#' @param y A symbilic data table 1x1
#' @param method Method to use
#' @param normalize A logical value indicating whether normalize the output
#'
#' @keywords internal
set.distance <- function(x, y, q = 2) {
  var <- rbind(x$data, y$data)
  sum.var <- colSums(var)
  union <- sum(sum.var > 0)
  intersect <- length(which(sum.var == 2))

  distance <- union - intersect + 0.5 * (2 * intersect - sum(x$data) - sum(y$data))
  distance <- distance / ncol(x$data)
  distances <- distance^q
  return(distance)
}

#' Distance between interval variables
#'
#' @param x A symbilic data table 1x1
#' @param y A symbilic data table 1x1
#'
#' @keywords internal
interval.distance <- function(x, y, q = 2) {
  max(c(abs(x$data[, 1] - y$data[, 1]), abs(x$data[, 2] - y$data[, 2])))^q
}


#' Distance between histogram variables
#'
#' @param x A symbilic data table 1x1
#' @param y A symbilic data table 1x1
#'
#' @keywords internal
hist.distance <- function(x, y, q = 2) {
  1 - (sum(sqrt(x$data * y$data))^q)
}

#' Distance between continuous variables
#'
#' @param x A symbilic data table 1x1
#' @param y A symbilic data table 1x1
#'
#' @keywords internal
cont.distance <- function(x, y, q = 2) {
  (x$data - y$data)^q
}
