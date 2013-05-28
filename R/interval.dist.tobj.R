interval.dist.tobj <-
function(sym.obj.x,sym.obj.y,distance=c('hausdorff','centers'),
                             p=2) {
    distance<-match.arg(distance)
    idn1<-all(sym.obj.x$var.types=='$I')
    idn2<-all(sym.obj.y$var.types=='$I')
    if((idn1==FALSE)||(idn2==FALSE))
      stop("The two variables have to be interval type")         
    if(distance=='hausdorff') {
      pos<-1
      sum<-0
      for(j in 1:(sym.obj.x$M)) {
        a<-abs(sym.obj.x$obj.data.vector[pos]-sym.obj.y$obj.data.vector[pos])
        b<-abs(sym.obj.x$obj.data.vector[pos+1]-sym.obj.y$obj.data.vector[pos+1])
        m<-max(a,b)
        pos<-pos+2
        sum<-sum+m^p
      }
      return(sum^(1/p))
    }
    else {
      if(distance=='centers') {
        pos<-1
        sum<-0
        for(j in 1:(sym.obj.x$M)) {
          a<-(sym.obj.x$obj.data.vector[pos]+sym.obj.x$obj.data.vector[pos+1])/2
          b<-(sym.obj.y$obj.data.vector[pos]+sym.obj.y$obj.data.vector[pos+1])/2
          m<-abs(a-b)
          pos<-pos+2
          sum<-sum+m^p
        }
        return(sum^(1/p))
      }    
      else       
        stop("Impossible to compute the Hausdorff distance for this type of variable")       
    }
}
