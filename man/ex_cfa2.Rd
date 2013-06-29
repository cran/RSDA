\name{ex_cfa2}
\alias{ex_cfa2}
\docType{data}
\title{
Correspondence Analysis Example
}
\description{
Correspondence Analysis for Symbolic MultiValued Variables example.
}
\usage{data(ex_cfa2)}

\references{
Rodriguez, O. (2011).
Correspondence Analysis for Symbolic MultiValued Variables. Workshop in Symbolic 
Data Analysis Namur, Belgium
}
\examples{
data(ex_cfa2)
res<-sym.cfa(ex_cfa2)
cfa.scatterplot(sym.var(res,1),sym.var(res,2),num.gr1=ex_cfa2$N,labels=TRUE,col='red',
                main='CFA')
}
\keyword{datasets}
