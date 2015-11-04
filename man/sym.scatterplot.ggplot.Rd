\name{sym.scatterplot.ggplot}
\alias{sym.scatterplot.ggplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Scatter GGPlot
}
\description{
This function could be use to plot two symbolic variables in a X-Y plane using ggplot 
R package.
}
\usage{
sym.scatterplot.ggplot(sym.var.x, sym.var.y, labels = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym.var.x}{
First symbolic variable.
}
  \item{sym.var.y}{
Second symbolic variable.
}
  \item{labels}{
As in ggplot.
}
  \item{\dots}{
As in ggplot.
}
}
\value{
return a ggplot graphic.
}
\references{
Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information 
 from complex data. Springer, Germany.

Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University.
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
\code{\link{sym.scatterplot}}
}
\examples{
data(lynne1)
sym.scatterplot.ggplot(sym.var(lynne1,1),sym.var(lynne1,3),labels=TRUE)
data(oils)
sym.scatterplot.ggplot(sym.var(oils,2),sym.var(oils,3),labels=TRUE)
}
\keyword{ Symbolic }
\keyword{ ggplot }% __ONLY ONE__ keyword per line
