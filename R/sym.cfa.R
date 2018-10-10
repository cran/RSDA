#' @keywords internal
#'
sym.cfa <- function(x = NULL, y = NULL) {

  sym.data <- transform.set(x, y)
  idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
  if (idn == FALSE) {
    stop("All variables have to be of the same type")
  }
  res <- cfa.totals(sym.data)
  Z <- cfa.MatrixZ(sym.data, res$TotalRows, res$TotalColumns)
  svd <- eigen(Z)
  MVPRealz <- cfa.CVPRealz(
    sym.data, res$TotalRows, res$TotalColumns, res$Total,
    svd$vectors
  )
  Mzz <- cfa.Czz(sym.data, res$TotalRows, res$TotalColumns, MVPRealz, svd$values)
  CMM <- cfa.minmax(
    sym.data, res$TotalRows, res$TotalRowsMin, res$TotalRowsMax,
    res$TotalColumns, res$TotalColumnsMin, res$TotalColumnsMax, res$Total, MVPRealz,
    Mzz
  )
  names. <- make.names(c(sym.data$sym.var.names, sym.data$sym.obj.names),unique = T)
  df <- cbind(concept = names., data.frame(rbind(CMM$Min, CMM$Max)))
  df <- cbind(concept = names., data.frame(rbind(CMM$Min, CMM$Max)))
  colnames(df) <- c("concept", paste0("C", seq_len(ncol(df) - 1)))
  out <- classic.to.sym(df, concept = "concept")
  class(out) <- "sym.data.table"
  meta <- do.call("cbind",lapply(seq_len(ncol(CMM$Min)), function(x){data.frame("$I" = rep("I",nrow(CMM$Max)), CMM$Min[,x], CMM$Max[,x] ,check.names = F)}))
  data <- do.call("cbind",lapply(seq_len(ncol(CMM$Min)), function(x){data.frame(CMM$Min[,x], CMM$Max[,x] ,check.names = F)}))
  colnames(data) <-colnames(out$data)
  colnames(meta) <-colnames(out$meta)
  rownames(meta) <- make.names(names.,unique = T)
  rownames(data) <- make.names(names.,unique = T)

  out2 <- list(N = sum(sym.data$N,sym.data$M),
               M = ncol(CMM$Min),
               sym.obj.names = names.,
               sym.var.names = out$sym.var.names,
               sym.var.types = out$sym.var.types,
               sym.var.length = out$sym.var.length,
               sym.var.starts = out$sym.var.starts,
               data = data,
               meta = meta)
  class(out2) <- "sym.data.table"
  return(out2)
}

transform.set <- function(x, y) {
  concept <- colnames(x$data)
  x <- as.matrix(x$data)
  y <- as.matrix(y$data)

  x.min <- x
  p <- apply(x, 1, function(x) sum(x) > 1)
  x.min[p, ] <- rep(0, ncol(x))
  x.max <- x

  y.max <- y
  y.min <- y
  p <- apply(y, 1, function(x) sum(x) > 1)
  y.min[p, ] <- rep(0, ncol(y))

  df1 <- data.frame(t(x.min) %*% y.min)
  df2 <- data.frame(t(x.max) %*% y.max)
  df1$concept <- concept
  df2$concept <- concept
  df.out <- rbind(df1, df2)

  classic.to.sym(df.out, concept = "concept")
}
