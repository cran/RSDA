Scon <-
function(v=c()) {   
  colname <- sapply(match.call()[1+1:length(v)], as.character)
  tname<-colname[[1]][2]
  st<-mget(tname, envir=.GlobalEnv)
  st<-st[[1]]
  scol<-colname[[1]][3]
  lst<-length(st)
  m<-matrix(nrow=lst,ncol=1)  
  mnames<-matrix(nrow=lst,ncol=1)
  colnames(mnames) <- colnames(mnames, do.NULL = FALSE, prefix = "var.")
  colnames(mnames)[1]<-"$C"
  colnames(m) <- colnames(m, do.NULL = FALSE, prefix = "var.")
  colnames(m)[1] <-scol
  rownames(m) <- rownames(m, do.NULL = FALSE, prefix = "con.")    
  for(i in 1: lst) {  
    if (nrow(st[[i]])>0)  {   
      m[i,1]<-round(mean(st[[i]][,scol]),2)
      rownames(m)[i] <-names(st[i])
    }
  }
  mnames[is.na(mnames)] <- "$C"
  rm=abind(mnames,m)    
  return(rm)
}