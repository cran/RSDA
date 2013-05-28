\name{plotX.slice}
\alias{plotX.slice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Slice
}
\description{
Plot a variable in the symboli circle of correlations.
}
\usage{
plotX.slice(xx1, yy1, xx2, yy2, vv, vvars, kk)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx1}{
Botton left corner of the rectangle that generates the slice.
}
  \item{yy1}{
Top left corner of the rectangle that generates the slice.
}
  \item{xx2}{
Botton right corner of the rectangle that generates the slice.
}
  \item{yy2}{
Top right corner of the rectangle that generates the slice.
}
  \item{vv}{
Color inside
}
  \item{vvars}{
Label fot yhe variables
}
  \item{kk}{
Color line
}
}

\value{
Plot a slice in the correlation circle
}
\references{
Rodriguez O. (2012). The Duality Problem in Interval Principal Components Analysis. The 
3rd Workshop in Symbolic Data Analysis, Madrid
}
\author{
Oldemar Rodriguez Rojas
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sym.interval.pca}}
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
