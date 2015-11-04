\name{sym.scatterplot}
\alias{sym.scatterplot}
\title{
Symbolic Scatter Plot
}
\description{
This function could be use to plot two symbolic variables in a X-Y plane.
}
\usage{
sym.scatterplot(sym.var.x, sym.var.y, labels = FALSE, ...)
}
\arguments{
  \item{sym.var.x}{
First symbolic variable
}
  \item{sym.var.y}{
Second symbolic variable.
}
  \item{labels}{
As in R plot function.
}
  \item{\dots}{
As in R plot function.
}
}
\value{
Return a graphics. 
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
 \code{\link{sym.scatterplot3d}}
}
\examples{
data(example3)
sym.data<-example3
sym.scatterplot(sym.var(sym.data,1),sym.var(sym.data,4),col='blue',
                main='Main Title')
sym.scatterplot(sym.var(sym.data,1),sym.var(sym.data,4),labels=TRUE,col='blue',
                main='Main Title')
sym.scatterplot(sym.var(sym.data,2),sym.var(sym.data,6),labels=TRUE,
                col='red',main='Main Title',lwd=3)
                
data(oils)
sym.scatterplot(sym.var(oils,2),sym.var(oils,3),labels=TRUE,
                col='red',main='Oils Data')
data(lynne1)

sym.scatterplot(sym.var(lynne1,2),sym.var(lynne1,1),labels=TRUE,
                col='red',main='Lynne Data')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Plot }% __ONLY ONE__ keyword per line
