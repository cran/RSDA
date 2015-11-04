\name{deter.coefficient}
\alias{deter.coefficient}

\title{
Compute the determination cosfficient
}
\description{
The determination coefficient represents a goodness-of-fit measure commonly used in 
regression analysis to capture the adjustment quality of a model.
}
\usage{
deter.coefficient(sym.var, prediction)
}

\arguments{
  \item{sym.var}{
Variable that was predicted.
}
  \item{prediction}{
The prediction given by the model.
}
}
\value{
Return the determination cosfficient.
}
\references{
LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
to fitting a linear regression model on symbolic interval data. Computational
Statistics and Data Analysis 52, 1500-1515.

LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models 
for symbolic interval-valued variables. Computational Statistics and
Data Analysis 54, 333-347.
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{sym.glm}}
}
\examples{
data(int_prost_train)
data(int_prost_test)
res.cm<-sym.lm(lpsa~.,sym.data=int_prost_train,method='cm')
pred.cm<-predictsym.lm(res.cm,int_prost_test,method='cm')
deter.coefficient(sym.var(int_prost_test,9),pred.cm$Fitted)

res.cm.lasso<-sym.glm(sym.data=int_prost_train,response=9,method='cm',
                      alpha=1,nfolds=10,grouped=TRUE)
pred.cm.lasso<-predictsym.glm(res.cm.lasso,response=9,int_prost_test,method='cm')
deter.coefficient(sym.var(int_prost_test,9),pred.cm.lasso)
}
\keyword{ Symbolic }
\keyword{ Lasso }
\keyword{ Ridge }
\keyword{ Elastic }
\keyword{ Net }
