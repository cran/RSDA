interval.dist <-
function(sym.data,distance=c('hausdorff','centers'),p=2) {
  distance<-match.arg(distance)
  if((distance=='hausdorff')||(distance=='centers')) {
    idn<-all(sym.data$sym.var.types=='$I')
    if(idn==FALSE)
      stop("The two variables have to be interval type")         
    nn<-sym.data$N
    mm<-sym.data$M
    dist.matrix<-matrix(0,nn,nn)
    for(i in 1:nn) {
      for(j in 1:i) {
        dist.matrix[i,j]<-interval.dist.tobj(sym.obj(sym.data,i),sym.obj(sym.data,j),
                                             distance)
        dist.matrix[j,i]<-dist.matrix[i,j]
      }      
    }
    return(as.dist(dist.matrix))
  }  
}
