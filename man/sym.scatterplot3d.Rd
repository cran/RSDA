\name{sym.scatterplot3d}
\alias{sym.scatterplot3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Scatter Plot 3D
}
\description{
This function could be use to plot two symbolic variables in 3D i.e. in a X-Y-Z plane.
}
\usage{
sym.scatterplot3d(sym.var.x, sym.var.y, sym.var.z, labels = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym.var.x}{
First symbolic variable.
}
  \item{sym.var.y}{
Second symbolic variable.
}
  \item{sym.var.z}{
Third symbolic variable.
}
  \item{labels}{
As in R plot function.
}
  \item{\dots}{
As in R plot function.
}
}
\value{
3D Plot graphic.
}
\references{

Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information 
 from complex data. Springer, Germany.
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
\code{\link{sym.scatterplot}}
}
\examples{
data(lynne1)
sym.scatterplot3d(sym.var(lynne1,1),sym.var(lynne1,2),sym.var(lynne1,3),
                             color='blue', main='Lynne Data')
}
\keyword{ Symbolic }
\keyword{ 3DPlot }% __ONLY ONE__ keyword per line
