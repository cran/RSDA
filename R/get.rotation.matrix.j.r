#' get.rotation.matrix
#' @keywords internal
get.rotation.matrix <- function(pca) {
  return(t(apply(pca$var$coord, 1, function(x) {
    x / sqrt(pca$eig[, 1])
  })))
}
