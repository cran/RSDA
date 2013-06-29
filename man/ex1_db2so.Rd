\name{ex1_db2so}
\alias{ex1_db2so}
\docType{data}
\title{
Data example to generate symbolic objets
}
\description{
This is a small data example to generate symbolic objets.
}
\usage{data(ex1_db2so)}
\references{
Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
complex data. Springer, Germany.
}
\examples{
data(ex1_db2so)
ex1<-ex1_db2so
concept<-Sconcept(ex1$state,ex1$sex)
sym.table<-Sobject(Sint(concept$county),Sint(concept$group),Sint(concept$age))
sym.table
}
\keyword{datasets}
