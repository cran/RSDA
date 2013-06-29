Sconcept <- 
function(...) {
  l <- list(...)
  v <- l[1]
  colname <- sapply(match.call()[1+1:length(v)], as.character)
  tname <- colname[2,1]
  t <- mget(tname, envir=.GlobalEnv) 
  t <- t[[1]]
  stable <- split(t,l)    
  return(stable)
}