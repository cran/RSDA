calc.matrix.min<-function(data.max)
{
  dim.data.max<-dim(data.max)
  data.min<-data.max
  zeros.rep<-rep(0,dim.data.max[2])
  indx.mult<-which(apply(data.max, 1, sum) > 1)
  data.min[indx.mult,]<-zeros.rep
  return(data.min)
}