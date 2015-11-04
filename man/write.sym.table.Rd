\name{write.sym.table}
\alias{write.sym.table}
\title{
Write Symbolic Data Table
}
\description{
This function write (save) a symbolic data table from a CSV data file.
}
\usage{
write.sym.table(sym.data, file, sep, dec, row.names = NULL, col.names = NULL)
}
\arguments{ 
\item{sym.data}{
Symbolic data table
}
  \item{file}{
The name of the CSV file.
}
  \item{sep}{
As in R function read.table
}
  \item{dec}{
As in R function read.table
}
  \item{row.names}{
As in R function read.table
}
  \item{col.names}{
As in R function read.table
}
}
\value{
Write in CSV file the symbolic data table.
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
\code{\link{read.sym.table}}
}
\examples{
data(example1)
write.sym.table(example1, file='temp4.csv', sep='|',dec='.', row.names=TRUE, 
                col.names=TRUE)
ex1<-read.sym.table('temp4.csv', header=TRUE, sep='|',dec='.', row.names=1)
}
\keyword{ Symbolic }
\keyword{ Table }% __ONLY ONE__ keyword per line
