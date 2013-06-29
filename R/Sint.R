Sint <-
function(v=c()) {  
  colname <- sapply(match.call()[1+1:length(v)], as.character)
  tname<-colname[[1]][2]
  st<-mget(tname, envir=.GlobalEnv); st<-st[[1]]
  scol<-colname[[1]][3]
  tc<-length(st)
  m<-matrix(nrow=tc,ncol=2)  
  mnames<-matrix(nrow=tc,ncol=1)
  colnames(mnames) <- colnames(mnames, do.NULL = FALSE, prefix = "var.")
  colnames(mnames)[1]<-"$I"
  colnames(m) <- colnames(m, do.NULL = FALSE, prefix = "var.")
  colnames(m)[1] <-scol
  colnames(m)[2] <-scol
  rownames(m) <- rownames(m, do.NULL = FALSE, prefix = "con.")
  for(i in 1: tc) {  
    if (nrow(st[[i]])>0)  {  
      rownames(m)[i] <-names(st[i])
      m[i,1]<-round(min(st[[i]][scol]),2)
      m[i,2]<-round(max(st[[i]][scol]),2)
    }               
  } 
  mnames[is.na(mnames)] <- "$I"
  rm=abind(mnames,m)  
  return(rm)
}
