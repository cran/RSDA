\name{sym.obj}
\alias{sym.obj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symbolic Object
}
\description{
This function get a symbolic object (row or a case) from a symbolic data table.
}
\usage{
sym.obj(sym.data, number.sym.obj)
}
\arguments{
  \item{sym.data}{
Symboli data matrix.
}
  \item{number.sym.obj}{
The number of the row for the symbolic object (case) that we want to get.
}
}
\value{
Return a symbolic object with the following internal format:

$M

[1] 5

$var.types

[1] "$C" "$H" "$I" "$H" "$C"

$var.length

[1] 1 5 2 3 1

$var.names

[1] "F1" "F2" "F3" "F4" "F5"

$obj.data.vector

        F1  M1  M2  M3  M4  M5 F3 F3.1 M1.1 M2.1 M3.1 F5
        
Case4 -2.1 0.4 0.1 0.1 0.1 0.3  0    2  0.9    0  0.1  0
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
\code{\link{sym.var}}
}
\examples{
data(example7)
sym.obj(example7,4)
}
\keyword{ Symbolic }
\keyword{ Object }% __ONLY ONE__ keyword per line
