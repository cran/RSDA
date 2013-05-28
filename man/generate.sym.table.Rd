\name{generate.sym.table}
\alias{generate.sym.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a Symbolic Data Table
}
\description{
This function generates a symbolic data table from a CSV data file.
}
\usage{
generate.sym.table(sym.data, file, sep, dec, row.names = NULL, col.names = NULL)
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
Return a symbolic data table.
}
\references{
Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
complex data. Springer, Germany.
}
\author{
Oldemar Rodriguez Rojas
}

\examples{
data(example1)
generate.sym.table(example1, file='temp4.csv', sep='|',dec='.', row.names=TRUE, 
                   col.names=TRUE)
ex1<-read.sym.table('temp4.csv', header=TRUE, sep='|',dec='.', row.names=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Symbolic }
\keyword{ Table}% __ONLY ONE__ keyword per line
