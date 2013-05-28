\name{sym.circle.plot}
\alias{sym.circle.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Circle of Correlations
}
\description{
Plot the symbolic circle of correlations
}
\usage{
sym.circle.plot(prin.corre)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prin.corre}{
A symbolic interval data matrix with correlations between the variables and the 
principals componets, both of interval type.
}
}
\value{
Plot the symbolic circle
}
\references{
Rodriguez O. (2012). The Duality Problem in Interval Principal Components Analysis. 
The 3rd Workshop in Symbolic Data Analysis, Madrid.
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
\code{\link{plotX.slice}}
}
\examples{
data(oils)  
res<-sym.interval.pca(oils,'centers')
sym.circle.plot(res$Sym.Prin.Correlations)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Circle }% __ONLY ONE__ keyword per line
