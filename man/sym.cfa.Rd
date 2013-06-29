\name{sym.cfa}
\alias{sym.cfa}

\title{
Symbolic correspondence analysis
}
\description{
Correspondence Analysis for Symbolic MultiValued Variables.
}
\usage{
sym.cfa(sym.data)
}

\arguments{
  \item{sym.data}{
Should be a symbolic data table read with the function read.sym.table(...).
}
}

\value{
Return the interval principal components.
}
\references{
Rodriguez, O. (2011).
Correspondence Analysis for Symbolic MultiValued Variables. Workshop in Symbolic Data Analysis
Namur, Belgium.
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{cfa.Czz}}
}
\examples{
data(ex_cfa1) 
res<-sym.cfa(ex_cfa1)
cfa.scatterplot(sym.var(res,1),sym.var(res,2),num.gr1=ex_cfa1$N,
                        labels=TRUE,col='red',main='CFA')
}

\keyword{ CFA }

