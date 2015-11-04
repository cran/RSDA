\name{interscal}
\alias{interscal}

\title{
Interscal Method
}
\description{
Execute interscal Method.
}
\usage{
interscal(sym.data)
}

\arguments{
  \item{sym.data}{
The symbolic data matrix.
}
}

\value{
The symbolic interval components.
}
\references{
Groenen, P.J.F., Winsberg, S., Rodriguez, O., Diday, E. (2006). I-Scal: Multidimensional 
scaling of interval dissimilarities. Computational Statistics and Data Analysis, 51, 
360-378. 	
	
Rodriguez, O. (2000).
Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
Paris IX-Dauphine University
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{sym.interval.pca}}
}
\examples{
data(ex_cfa1)
res<-interscal(ex_cfa1)
sym.scatterplot(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
                labels=TRUE,col='red',main='Interscal CFA Data')
sym.scatterplot3d(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
                  sym.var(res$Sym.Components,3),color='blue',
                  labels=TRUE,main='Interscal CFA Data')
sym.scatterplot.ggplot(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
                                    labels=TRUE)
}

\keyword{ Interscal }
