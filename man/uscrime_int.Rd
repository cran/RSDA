\name{uscrime_int}
\alias{uscrime_int}
\docType{data}
\title{
Us crime interval data table
}
\description{
Us crime classic data table genetated from uscrime data.
}
\usage{data(uscrime_int)}

\references{
Rodriguez O. (2013). A generalization of Centre and Range method for fitting a linear 
regression model to symbolic interval data using Ridge Regression, Lasso
and Elastic Net methods. The IFCS2013 conference of the International Federation of 
Classification Societies, Tilburg University Holland.
}
\examples{
data(uscrime_int)
car.data<-uscrime_int
res.cm.lasso<-sym.glm(sym.data=car.data,response=102,method='cm',alpha=1,
                                      nfolds=10,grouped=TRUE)
plot(res.cm.lasso)
plot(res.cm.lasso$glmnet.fit, "norm", label=TRUE)
plot(res.cm.lasso$glmnet.fit, "lambda", label=TRUE)

pred.cm.lasso<-predictsym.glm(res.cm.lasso,response=102,car.data,method='cm')
RMSE.L(sym.var(car.data,102),pred.cm.lasso)
RMSE.U(sym.var(car.data,102),pred.cm.lasso)
R2.L(sym.var(car.data,102),pred.cm.lasso)
R2.U(sym.var(car.data,102),pred.cm.lasso)
deter.coefficient(sym.var(car.data,102),pred.cm.lasso)
}
\keyword{datasets}
