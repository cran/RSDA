\name{rightarrow.matrix}
\alias{rightarrow.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Right Arrow Matrix
}
\description{
Transform a symbolic matrix in an classic data matrix.
}
\usage{
rightarrow.matrix(sym.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym.data}{
This is symbolic data matrix.
}
}
\value{
sym.data: Return the original symbolic data matrix transformed in a classic data matrix 
in which all the entries are real numbers.
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
cmatrix<-rightarrow.matrix(example7)
cmatrix
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Histogram }
\keyword{ PCA }% __ONLY ONE__ keyword per line
