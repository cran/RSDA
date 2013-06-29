\name{Sobject}
\alias{Sobject}

\title{
Generate a symbolic data table
}
\description{
Generate a symbolic data table from a classic data table.
}
\usage{
Sobject(...)
}

\arguments{
  \item{\dots}{
There is no arguments.
}
}
\value{
The symbolic data table.
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
data(ex1_db2so)
ex1<-ex1_db2so
concept<-Sconcept(ex1$state,ex1$sex)
sym.table<-Sobject(Scon(concept$county),Sint(concept$group),Shis(concept$age))
sym.table
}

\keyword{ symbolic }
\keyword{ data }
\keyword{ table }
