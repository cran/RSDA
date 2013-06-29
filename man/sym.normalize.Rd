\name{sym.normalize}
\alias{sym.normalize}

\title{
Normalize a interval data table
}
\description{
Normalize a interval data table.
}
\usage{
sym.normalize(sym.data)
}

\arguments{
  \item{sym.data}{
Should be a symbolic data table read with the function read.sym.table(...).
}
}

\value{
The symbolic data table normalize with respect the centers.
}
\references{
Groenen, P.J.F., Winsberg, S., Rodriguez, O., Diday, E. (2006). I-Scal: Multidimensional 
scaling of interval dissimilarities. Computational Statistics and Data Analysis, 51, 
360-378. 	
	
Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{sym.interval.pca}}
}
\examples{
data(ex_cfa1)
sdn<-sym.normalize(ex_cfa1)
MDis<-interscal.delta(ex_cfa1)
}
\keyword{ interscal }
