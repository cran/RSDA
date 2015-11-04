\name{sym.mds}
\alias{sym.mds}
\title{
Symbolic Multidemensional Scaling
}
\description{
This function execute a multidimensional scaling from a interval symbolic data matrix.
}
\usage{
sym.mds(sym.data, distance = c("hausdorff", "centers"), p = 2, 
        method = c("classic", "INTERSCAL"))
}
\arguments{
  \item{sym.data}{
The symbolic data matrix.
}
  \item{distance}{
The distance to be use.
}
  \item{p}{The p in the Hausdorff distance
  	
      \deqn{d(w_{u_1},w_{u_2}) = \left( \sum_{j=1}^m \Phi_j(w_{u_1},w_{u_2})^p  \right)^{1/p}}
}
  \item{method}{
The method to be used.
}
}
\value{
	Return the coordanates to plot the graphic.

}
\references{
Groenen, P.J.F., Winsberg, S., Rodriguez, O., Diday, E. (2006). I-Scal: Multidimensional 
scaling of interval dissimilarities. Computational Statistics and Data Analysis, 51, 
360-378. 	
	
Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University.
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{sym.interval.pca}}
}
\examples{
data(oils)
res<-sym.mds(oils)
plot(res,pch = 23, bg = "red", xlab = "Score 1", ylab = "Score 2")
res<-sym.mds(oils,distance="centers")
plot(res,pch = 23, bg = "red", xlab = "Score 1", ylab = "Score 2")
}
\keyword{ Symbolic }
\keyword{ MDS }% __ONLY ONE__ keyword per line
