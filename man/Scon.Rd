\name{Scon}
\alias{Scon}
\title{
Generate a continuos variable
}
\description{
It is use to generate a continuos variable.
}
\usage{
Scon(v = c())
}

\arguments{
  \item{v}{
v is the variable to be transform into a continuos variable.
}
}

\value{
The continuos variable.
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
\keyword{ db2so }
\keyword{ interval }
\keyword{ variable }