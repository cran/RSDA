\name{sym.mean}
\alias{sym.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Mean
}
\description{
This function compute the symbolic mean
}
\usage{
sym.mean(sym.var, method = c("centers", "interval", "histogram"), 
         trim = 0, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym.var}{
The symbolic variable.
}
  \item{method}{
The method to be use.
}
  \item{trim}{
As in R mean function.
}
  \item{na.rm}{
As in R mean function.
}
  \item{\dots}{
As in R mean function.
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
sym.mean(sym.var(sym.data,1))
sym.mean(sym.var(sym.data,2))
sym.mean(sym.var(sym.data,2),method='interval')
sym.mean(sym.var(sym.data,3),method='histogram')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Mean }% __ONLY ONE__ keyword per line
