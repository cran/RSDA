Sset <-
function(v=c()) {  
  colname <- sapply(match.call()[1+1:length(v)], as.character)
  tname<-colname[[1]][2]
  st<-mget(tname, envir=.GlobalEnv); st<-st[[1]]
  scol<-colname[[1]][3]
  tc<-length(st)
  tk=c()
  p=1
  for (i in 1: tc) {  
    if (nrow(st[[i]])>0)  {  
      xv<-st[[i]][scol] 
      tv<-nrow(xv)
      for (j in 1: tv)  {   
        tk[p]<-xv[j,1]
        p<-p+1
      }
    }
  }
  nn <- as.vector(sort(unique(tk)))
  ncols <- length(nn)
  m<-matrix(nrow=tc,ncol=ncols)
  colnames(m) <-nn   
  for(i in 1:tc) 
    for (j in 1:ncols)
      m[i,j]<-0;    
  rownames(m) <- rownames(m, do.NULL = FALSE, prefix = "Case")
  for(i in 1: tc) {  
    if (nrow(st[[i]])>0) {  
      rownames(m)[i] <-names(st[i])
      v<-table(st[[i]][scol])
      nv<-length(v)
      vv<-as.vector(names(v))      
      for (j in 1:nv) {  
        pcol<-1;
        for(k in 1:ncols)
          if(nn[k]== vv[j])  
            pcol<-k 
        m[i,pcol]<-v[j]/v[j]  
      }             
    }
  }   
  mnames<-matrix(nrow=tc,ncol=2)
  colnames(mnames) <- colnames(mnames, do.NULL = FALSE, prefix = "")
  colnames(mnames)[1]<-"$S"
  colnames(mnames)[2]<-scol
  mnames[,2]<-ncols   
  mnames[ is.na(mnames) ] <- "$S"
  rm=abind(mnames,m)  
  return(rm) 
}
