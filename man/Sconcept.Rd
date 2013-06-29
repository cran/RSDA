\name{Sconcept}
\alias{Sconcept}
\title{
Generate a concept
}
\description{
It is use to generate a symbolic concept or variable.
}
\usage{
Sconcept(...)
}
\arguments{
  \item{\dots}{
No arguments
}
}
\value{
The symbolic variable or concept.
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