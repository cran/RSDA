\name{interscal.B}
\alias{interscal.B}

\title{
Matrix B in interscal algorithm
}
\description{
Compute the matrix B in interscal algorithm.
}
\usage{
interscal.B(sym.data)
}

\arguments{
  \item{sym.data}{
Should be a symbolic data table read with the function read.sym.table(...).
}
}

\value{
The matrix B.
}
\references{
Groenen, P.J.F., Winsberg, S., Rodriguez, O., Diday, E. (2006). I-Scal: Multidimensional 
scaling of interval dissimilarities. Computational Statistics and Data Analysis, 51, 
360-378. 	
	
Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University.
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{interscal}}
}
\examples{
data(ex_cfa1)
sdn<-sym.normalize(ex_cfa1)
BM<-interscal.B(sdn)
ds<-eigen(BM)
ValP<-ds$values
VecP<-ds$vector
}
\keyword{ interscal }

