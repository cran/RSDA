\name{VeterinaryData}
\alias{VeterinaryData}
\docType{data}
\title{
Symbolic interval data example
}
\description{
Symbolic data matrix with all the variables of interval type
}
\usage{data(VeterinaryData)}
\format{
   $I Height Height $I Weight Weight
   
1  $I  120.0  180.0 $I  222.2  354.0

2  $I  158.0  160.0 $I  322.0  355.0

3  $I  175.0  185.0 $I  117.2  152.0

4  $I   37.9   62.9 $I   22.2   35.0

5  $I   25.8   39.6 $I   15.0   36.2

6  $I   22.8   58.6 $I   15.0   51.8

7  $I   22.0   45.0 $I    0.8   11.0

8  $I   18.0   53.0 $I    0.4    2.5

9  $I   40.3   55.8 $I    2.1    4.5

10 $I   38.4   72.4 $I    2.5    6.1	
}
\references{
Billard L. and  Diday E. (2006).
Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
}
\examples{
data(VeterinaryData)
display.sym.table(VeterinaryData)
}
\keyword{datasets}
