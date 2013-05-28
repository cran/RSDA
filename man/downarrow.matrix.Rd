\name{downarrow.matrix}
\alias{downarrow.matrix}
\title{
Down Arrow Matrix
}
\description{
Transform a symbolic matrix in an interval matrix.
}
\usage{
downarrow.matrix(sym.data)
}
\arguments{
  \item{sym.data}{
  	This is symbolic data matrix.
}
}
\value{
sym.data: Return the original symbolic data matrix transformed in a symbolic matrix in 
which all the entries are intervals.
}
\references{
Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University.
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
\code{\link{sym.histogram.pca}}
}
\examples{
data(example7)
sym.data<-example7
imatrix<-downarrow.matrix(example7)
imatrix
}
\keyword{ Histogram }
\keyword{ PCA }% __ONLY ONE__ keyword per line
