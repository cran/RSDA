\name{Cardiological}
\alias{Cardiological}
\docType{data}
\title{
Cardiological data example
}
\description{
Cardiological interval data example.
}
\usage{data(Cardiological)}

\references{
Billard L. and  Diday E. (2006).
Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
}
\examples{
data(Cardiological)
car.data<-Cardiological
res.cm<-sym.lm(Pulse~Syst+Diast,sym.data=car.data,method='cm')
pred.cm<-predictsym.lm(res.cm,car.data,method='cm')
RMSE.L(sym.var(car.data,1),pred.cm$Fitted)
RMSE.U(sym.var(car.data,1),pred.cm$Fitted)
R2.L(sym.var(car.data,1),pred.cm$Fitted)
R2.U(sym.var(car.data,1),pred.cm$Fitted)
deter.coefficient(sym.var(car.data,1),pred.cm$Fitted)
}
\keyword{datasets}
