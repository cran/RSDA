\name{cfa.scatterplot}
\alias{cfa.scatterplot}
\title{
CFA Symbolic Scatter Plot
}
\description{
This function could be use to plot two symbolic variables in a X-Y plane to special case of
Symbolic Correspondance Analysis.
}
\usage{
cfa.scatterplot(sym.var.x, sym.var.y, num.gr1=0, labels = TRUE, ...)
}
\arguments{
  \item{sym.var.x}{
First symbolic variable
}
  \item{sym.var.y}{
Second symbolic variable.
}
  \item{num.gr1}{
Number of modes of the first variable
}
  \item{labels}{
As in R plot function.
}
  \item{\dots}{
As in R plot function.
}
}
\value{
Return a graphics. 
}
\references{
Rodriguez, O. (2011).
Correspondence Analysis for Symbolic MultiValued Variables. Workshop in Symbolic Data Analysis
Namur, Belgium.
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
\code{\link{sym.cfa}}
}
\examples{
data(ex_cfa1) 
res<-sym.cfa(ex_cfa1)
cfa.scatterplot(sym.var(res,1),sym.var(res,2),num.gr1=ex_cfa1$N,labels=TRUE,col='red',
                        main='CFA')
}
\keyword{ CFA }
\keyword{ Plot }
