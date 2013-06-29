\name{ex_cfa1}
\alias{ex_cfa1}
\docType{data}
\title{
Correspondence Analysis Example
}
\description{
Correspondence Analysis for Symbolic MultiValued Variables example.
}
\usage{data(ex_cfa1)}

\references{
Rodriguez, O. (2011).
Correspondence Analysis for Symbolic MultiValued Variables. Workshop in Symbolic 
Data Analysis Namur, Belgium
}
\examples{
data(ex_cfa1)
res<-sym.cfa(ex_cfa1)
cfa.scatterplot(sym.var(res,1),sym.var(res,2),num.gr1=ex_cfa1$N,labels=TRUE,col='red',
                main='CFA')
}
\keyword{datasets}
