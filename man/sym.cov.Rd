\name{sym.cov}
\alias{sym.cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Covariance
}
\description{
This function compute the symbolic covariance
}
\usage{
sym.cov(sym.var.x, sym.var.y, 
        method = c("centers", "interval", "billard", "histogram"), na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{sym.var.x}{
First symbolic variables.
}
  \item{sym.var.y}{
Second symbolic variables.
}
  \item{method}{
The method to be use.
}
  \item{na.rm}{
As in R cov function.
}
  \item{\dots}{
As in R cov function.
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
sym.cov(sym.var(sym.data,1),sym.var(sym.data,4),method='centers')
sym.cov(sym.var(sym.data,2),sym.var(sym.data,6),method='centers')
sym.cov(sym.var(sym.data,2),sym.var(sym.data,6),method='billard')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Covariance }% __ONLY ONE__ keyword per line
