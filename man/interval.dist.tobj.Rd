\name{interval.dist.tobj}
\alias{interval.dist.tobj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Objects Distance
}
\description{
Compute a distance between two symbolic objects.
}
\usage{
interval.dist.tobj(sym.obj.x, sym.obj.y, distance = c("hausdorff", "centers"), p = 2)
}
\arguments{
  \item{sym.obj.x}{
First Symbolic Object
}
  \item{sym.obj.y}{
Second Symbolic Object
}
  \item{distance}{
Dsitance to be use
}
  \item{p}{The p in the Hausdorff distance
  	
      \deqn{d(w_{u_1},w_{u_2}) = \left( \sum_{j=1}^m \Phi_j(w_{u_1},w_{u_2})^p  \right)^{1/p}}
}
}
\value{
Return a real number that is the distance between sym.obj.x and sym.obj.y
}
\references{
Billard L. and  Diday E. (2006).
Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
\code{\link{interval.dist}}
}
\examples{
data(VeterinaryData)
VD<-VeterinaryData
interval.dist.tobj(sym.obj(VD,1),sym.obj(VD,2))
interval.dist.tobj(sym.obj(VD,1),sym.obj(VD,2),distance="centers")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Distance }% __ONLY ONE__ keyword per line
