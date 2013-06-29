Sobject <-
function(...) {
  sobject=list(...)
  na<-length(sobject)
  if (na==1)
    r<-sobject[[1]]
  else { 
    r<-sobject[[1]]
    for(i in 2: na)
      r<-abind(r,sobject[[i]])
  }
  return(as.data.frame(r))
}
