\name{sym.interval.pc}
\alias{sym.interval.pc}

\title{
Compute a symbolic interval principal components curves
}
\description{
Compute a symbolic interval principal components curves
}
\usage{
sym.interval.pc(sym.data, method = c("vertex", "centers"), maxit, plot, scale, center)
}

\arguments{
  \item{sym.data}{
Shoud be a symbolic data table read with the function read.sym.table(...)
}
  \item{method}{
It should be "vertex" or "centers".
}
  \item{maxit}{
Maximum number of iterations.
}
  \item{plot}{
TRUE to plot immediately, FALSE if you do not want to plot.
}
  \item{scale}{
TRUE to standardize the data.
}
  \item{center}{
TRUE to center the data.
}
}


\value{
prin.curve: This a symbolic data table with the interval principal components. As 
this is a symbolic data table we can apply over this table any other symbolic data 
analysis method (symbolic propagation).

cor.ps: This is the interval correlations between the original interval 
variables and the interval principal components, it can be use to plot the symbolic 
circle of correlations.
}

\references{
Arce J. and Rodriguez O. (2015) "Principal Curves and Surfaces 
to Interval Valued Variables". The 5th Workshop on Symbolic 
Data Analysis, SDA2015, Orleans, France, November.

Hastie,T. (1984).
Principal Curves and Surface. Ph.D Thesis Stanford University.

Hastie,T. \& Weingessel,A. (2014).
princurve - Fits a Principal Curve in Arbitrary Dimension.R package version 1.1--12
http://cran.r-project.org/web/packages/princurve/index.html.

Hastie,T. \& Stuetzle, W. (1989). Principal Curves.
Journal of the American Statistical Association, Vol. 84-406, 502--516.

Hastie, T., Tibshirani, R. \& Friedman, J. (2008).
The Elements of Statistical Learning; Data Mining, Inference and Prediction. Springer, New York.
}

\author{
Jorge Arce.
}

\seealso{
\code{\link{sym.interval.pca}}
}

\examples{
data(oils) 
res.vertex.ps<-sym.interval.pc(oils,"vertex",150,FALSE,FALSE,TRUE)
sym.scatterplot(sym.var(res.vertex.ps$sym.prin.curve,1),sym.var(res.vertex.ps$sym.prin.curve,2),
                labels=TRUE,col='red',main='PSC Oils Data')

data(facedata) 
res.vertex.ps<-sym.interval.pc(facedata,"vertex",150,FALSE,FALSE,TRUE)
sym.scatterplot(sym.var(res.vertex.ps$sym.prin.curve,1),sym.var(res.vertex.ps$sym.prin.curve,2),
                labels=TRUE,col='red',main='PSC Face Data')
}

\keyword{ Principal }
\keyword{ Curve }
