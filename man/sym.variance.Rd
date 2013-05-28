\name{sym.variance}
\alias{sym.variance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Variance
}
\description{
Compute the symbolic variance
}
\usage{
sym.variance(sym.var, method = 
                   c("centers", "interval", "billard", "histogram"), na.rm = FALSE, ...)
}
\arguments{
  \item{sym.var}{
The symbolic variable.
}
  \item{method}{
The method to be use.
}
  \item{na.rm}{
As in R median function.
}
  \item{\dots}{
As in R median function.
}
}
\value{
Return a real number.
}
\references{
Billard L. and  Diday E. (2006).
Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.

Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University.
}
\author{
Oldemar Rodriguez Rojas
}
\examples{
data(example3)
sym.data<-example3
sym.variance(sym.var(sym.data,1))
sym.variance(sym.var(sym.data,2))
sym.variance(sym.var(sym.data,6))
sym.variance(sym.var(sym.data,6),method='interval')
sym.variance(sym.var(sym.data,6),method='billard')
sym.variance(sym.var(sym.data,3),method='histogram')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Variance}% __ONLY ONE__ keyword per line
