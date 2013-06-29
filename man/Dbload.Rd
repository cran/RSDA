\name{Dbload}
\alias{Dbload}

\title{
Read a data table
}
\description{
Read a data table from any data base.
}
\usage{
Dbload(dsn, dbtable)
}

\arguments{
  \item{dsn}{
Connection argument to database.
}
  \item{dbtable}{
Data base table to be read.
}
}
\value{
Return the data table.
}

\references{
Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
complex data. Springer, Germany.
}
\author{
Johnny Villalobos Murillo.
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
\keyword{ CFA }
