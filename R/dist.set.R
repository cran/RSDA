#' Compute the distance between two rows
#' @keywords internal
sym.Multieval.distance <- function(sym.data, variable, w1, w2, gamma = 0.5, method = "Minkowski",
                                   normalize = TRUE) {
  if (method == "Gowda.Diday" | method == "Ichino" | method == "Minkowski") {
    result <- sym.data
    if (result$sym.var.types[variable] == "$S") {
      lenght <- result$sym.var.length[variable]
      ini <- result$sym.var.starts[variable]
      var <- result$meta[, ini:(ini + lenght - 1)]
      Var <- colSums(rbind(var[w1, ], var[w2, ]))
      Union <- sum(Var > 0)
      Intersect <- length(which(Var == 2))
      if (method == "Gowda.Diday") {
        D1 <- abs(sum(var[w1, ]) - sum(var[w2, ])) / Union
        D2 <- (sum(var[w1, ]) + sum(var[w2, ]) - 2 * Intersect) / Union
        Distance <- D1 + D2
      } else {
        if (gamma > 0.5) {
          gamma <- 0.5
        }
        if (gamma < 0) {
          gamma <- 0
        }
        Distance <- Union - Intersect + gamma * (2 * Intersect - sum(var[w1, ]) - sum(var[w2, ]))
      }
      if (normalize == TRUE) {
        Distance <- Distance / result$sym.var.length[variable]
      }
    } else {
      Distance <- NA
    }
    return(Distance)
  }
  return("Invalid method")
}
#' Distance for Symbolic Set Variables.
#' @name sym.dist.set
#' @description This function computes and returns the distance matrix by using the specified
#' distance measure to compute distance between symbolic interval variables.
#'
#' @param sym.data A symbolic object
#' @param variables Numeric vector with the number of the variables to use.
#' @param gamma gamma value for the methods ichino and minkowski.
#' @param method Method to use (Gowda.Diday, Ichino, Minkowski, Hausdorff)
#' @param normalize A logical value indicating whether normalize the data in the ichino or hausdorff method.
#' @param q q value for the Minkowski method.
#' @param pond A numeric vector
#'
#' @return An object of class 'dist'
#' @export

sym.dist.set <- function(sym.data, gamma = 0.5, method = "Minkowski", normalize = TRUE,
                         q = 1, pond = rep(1, length(variables))) {
  variables <- (1:(sym.data$M))
  if (sum(pond) != length(variables) & sum(pond) > 1) {
    pond <- rep(1 / length(variables), length(variables))
  }
  for (med in 1:length(method)) {
    if (method[med] == "Gowda.Diday" | method[med] == "Ichino" | method[med] ==
      "Minkowski") {
      result <- sym.data
      h <- 1
      Dissimilarity.matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
      if (method[med] == "Minkowski") {
        for (var in variables) {
          Matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          Matrix_pond <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          for (j in 1:nrow(result$data)) {
            for (i in j:nrow(result$data)) {
              Matrix[i, j] <- sym.Multieval.distance(
                sym.data, var, i, j, gamma,
                method[med], normalize
              )
            }
          }
          if (q == 1) {
            Matrix_pond <- Matrix_pond + (Matrix * pond[h])
          } else {
            Matrix_pond <- Matrix_pond + (Matrix)^q
          }
          Dissimilarity.matrix <- Dissimilarity.matrix + Matrix_pond
          h <- h + 1
        }
        Dissimilarity.matrix <- as.dist(Dissimilarity.matrix^(1 / q))
      } else {
        for (var in variables) {
          Matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          Matrix_pond <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          for (j in 1:nrow(result$data)) {
            for (i in j:nrow(result$data)) {
              Matrix[i, j] <- sym.Multieval.distance(
                sym.data, var, i, j, gamma,
                method[med], normalize
              )
            }
          }
          Matrix_pond <- Matrix_pond + (Matrix * pond[h])
          Dissimilarity.matrix <- Dissimilarity.matrix + Matrix_pond
          h <- h + 1
        }
        Dissimilarity.matrix <- as.dist(Dissimilarity.matrix)
      }
    } else {
      return("Invalid method")
    }
    if (med == 1) {
      res <- list(Dissimilarity.matrix)
      names <- matrix(c(method[med]))
    } else {
      names <- rbind(names, method[med])
      res[[length(res) + 1]] <- Dissimilarity.matrix
    }
    names(res) <- names
  }
  return(res)
}
