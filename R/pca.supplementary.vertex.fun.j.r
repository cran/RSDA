pca.supplementary.vertex.fun.j <- function(x, N, M, sym.var.names, sym.data.vertex.matrix,
                                           tot.individuals) {
  M.x <- matrix(x, nrow = N)
  colnames(M.x) <- sym.var.names
  M.x <- scale(M.x)
  mean.var <- attr(M.x, "scaled:center")
  desv.var <- attr(M.x, "scaled:scale")
  sym.data.vertex.matrix.cent <- sym.data.vertex.matrix
  for (i in 1:M) {
    sym.data.vertex.matrix.cent[, i] <- (sym.data.vertex.matrix.cent[, i] - mean.var[i]) / desv.var[i]
  }

  M.x <- rbind(M.x, sym.data.vertex.matrix.cent)

  pca.min <- PCA(
    X = M.x, scale.unit = FALSE, ind.sup = (N + 1):tot.individuals,
    ncp = M, graph = FALSE
  )

  min.dist.pca <- pca.min$ind.sup$dist * pca.min$ind.sup$dist
  return(sum(min.dist.pca))
}
