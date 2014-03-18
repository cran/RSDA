\name{classic.to.sym}
\alias{classic.to.sym}

\title{
Generate a symbolic data table
}
\description{
Generate a symbolic data table from a classic data table.
}
\usage{
classic.to.sym(dataTable, concept, variables, variables.types)
}
\arguments{
  \item{dataTable}{
 This is the classic data table.
}
  \item{concept}{
   These are the variable that we are going to use a concepts.
}
  \item{variables}{
   These are the variables that we want to inlude in the symbolic data table.
}
  \item{variables.types}{
  These are the variables symbolic types (continuos, interval, set or histograma) of the variables that we want to inlude in the symbolic data table.
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
Olger Calderon and Roberto Zuniga.
}

\seealso{
\code{\link{read.sym.table}}
}
\examples{
data(ex1_db2so)
ex1 <- ex1_db2so
result <- classic.to.sym(ex1, concept=c("state", "sex"),
                    variables=c("county", "group", "age","age"),
                    variables.types=c("$C", "$I", "$H", "$S"))
result
}
\keyword{ symbolic }
\keyword{ data }
\keyword{ table }
