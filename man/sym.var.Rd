\name{sym.var}
\alias{sym.var}
\title{
Symbolic Variable (Feature)
}
\description{
This function get a symbolic variable from a symbolic data table.
}
\usage{
sym.var(sym.data, number.sym.var)
}
\arguments{
  \item{sym.data}{
The symbolic data table
}
  \item{number.sym.var}{
The number of the column for the variable (feature) that we want to get.
}
}
\value{
Return a symbolic data variable with the following structure:

$N

[1] 7

$var.name

[1] "F6"

$var.type

[1] "$I"

$obj.names

[1] "Case1" "Case2" "Case3" "Case4" "Case5" "Case6" "Case7"

$var.data.vector

          F6  F6.1
          
Case1   0.00 90.00

Case2 -90.00 98.00

Case3  65.00 90.00

Case4  45.00 89.00

Case5  20.00 40.00

Case6   5.00  8.00

Case7   3.14  6.76
}
\references{
Billard L. and  Diday E. (2006).
Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.

Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information 
 from complex data. Springer, Germany.
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{sym.obj}}
}
\examples{
data(example3)
sym.data<-example3
sym.var(sym.data,4)
sym.var(sym.data,6)
}
\keyword{ Symbolic }
\keyword{ Variable }% __ONLY ONE__ keyword per line
