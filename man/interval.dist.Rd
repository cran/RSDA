\name{interval.dist}
\alias{interval.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interval Distance Matrix
}
\description{
Compute a distance matrix from a symbolic interval data matrix.
}
\usage{
interval.dist(sym.data, distance = c("hausdorff", "centers", "interscal"), p = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym.data}{
Symbolic data matrix with the variables of interval type.
}
  \item{distance}{
The distance to be use.
}
  \item{p}{The p in the Hausdorff distance
  	
      \deqn{d(w_{u_1},w_{u_2}) = \left( \sum_{j=1}^m \Phi_j(w_{u_1},w_{u_2})^p  \right)^{1/p}}
}
}
\value{
Return a R distance triangular matrix
}
\references{
Groenen, P.J.F., Winsberg, S., Rodriguez, O., Diday, E. (2006). I-Scal: Multidimensional 
scaling of interval dissimilarities. Computational Statistics and Data Analysis, 51, 
360-378. 	
	
Billard L. and  Diday E. (2006).
Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
}
\author{
Oldemar Rodriguez Rojas
}
\examples{
data(VeterinaryData)
VD<-VeterinaryData
interval.dist(VD)
interval.dist(VD,distance='centers')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Distance }% __ONLY ONE__ keyword per line
