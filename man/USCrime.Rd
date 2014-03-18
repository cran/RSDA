\name{USCrime}
\alias{USCrime}
\docType{data}
\title{
Us crime classic data table
}
\description{
Us crime classic data table that can be used to generate symbolic data tables.
}
\usage{data(USCrime)}

\source{
http://archive.ics.uci.edu/ml/
}
\references{
HASTIE, T., TIBSHIRANI, R. and FRIEDMAN, J. (2008). The Elements of Statistical Learning: 
Data Mining, Inference and Prediction. New York: Springer.
}
\examples{
data(USCrime)
us.crime<-USCrime
dim(us.crime)
head(us.crime)
summary(us.crime)
names(us.crime)
nrow(us.crime)
result  <- classic.to.sym(us.crime, concept="state", 
                          variables=c("NumInShelters", "NumImmig"), 
                          variables.types=c("$H", "$H"))
result
}
\keyword{datasets}
