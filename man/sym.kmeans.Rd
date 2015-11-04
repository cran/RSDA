\name{sym.kmeans}
\alias{sym.kmeans}
\title{
Symbolic k-Means
}
\description{
This is a function is to carry out a k-means overs a interval symbolic data matrix.
}
\usage{
sym.kmeans(sym.data, k = 3, iter.max = 10, nstart = 1, 
           algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
}
\arguments{
  \item{sym.data}{
Symbolic data table.
}
  \item{k}{
The number of clusters.
}
  \item{iter.max}{
Maximun number of iterations.
}
  \item{nstart}{
As in R kmeans function.
}
  \item{algorithm}{
The method to be use, as in kmeans R function.
}
}
\value{
This function return the following information:

K-means clustering with 3 clusters of sizes 2, 2, 4

Cluster means:

      GRA     FRE     IOD    SAP
      
1 0.93300 -13.500 193.500 174.75

2 0.86300  30.500  54.500 195.25

3 0.91825  -6.375  95.375 191.50


Clustering vector:

 L  P Co  S Ca  O  B  H 
 
 1  1  3  3  3  3  2  2 

Within cluster sum of squares by cluster:

[1] 876.625 246.125 941.875

 (between_SS / total_SS =  92.0 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"  
 
[7] "size"        
}
\references{
Carvalho F., Souza R.,Chavent M., and Lechevallier Y. (2006)
Adaptive Hausdorff distances and dynamic clustering of symbolic interval data. Pattern 
Recognition Letters Volume 27, Issue 3, February 2006, Pages 167-179
}
\author{
Oldemar Rodriguez Rojas
}
\seealso{
 \code{\link{sym.hclust}}
}
\examples{
data(oils)
sk<-sym.kmeans(oils,k=3)
sk$cluster
}
\keyword{ Symbolic }
\keyword{ Kmeans }% __ONLY ONE__ keyword per line
