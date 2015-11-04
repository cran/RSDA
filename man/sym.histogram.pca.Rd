\name{sym.histogram.pca}
\alias{sym.histogram.pca}
\title{
Histogram Principal Components Analysis
}
\description{
This functions allows us to execute a histogram principal components analysis from a 
symbolic data table with continuos, interval or histogram variables that can be mixed.
}
\usage{
sym.histogram.pca(sym.data, method = c("histogram", "classic"))
}
\arguments{
  \item{sym.data}{
Symbolic data table.
}
  \item{method}{
The method to be used.
}
}
\value{
Return a symbolic data table.
}
\references{
Diday, E., Rodriguez O. and Winberg S. (2000).
Generalization of the Principal Components Analysis to Histogram
Data, 4th European Conference on Principles and Practice of Knowledge Discovery in Data 
Bases, September 12-16, 2000, Lyon, France.

Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University.
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
\code{\link{sym.interval.pca}}
}
\examples{
data(example7)
res<-sym.histogram.pca(example7)
sym.scatterplot(sym.var(res,1),sym.var(res,2),labels=TRUE,col='red',main='Histogram PCA')
sym.scatterplot3d(sym.var(res,1),sym.var(res,2),sym.var(res,3),color='blue',
                  main='Histogram PCA')
}
\keyword{ Histogram }
\keyword{ PCA }% __ONLY ONE__ keyword per line
