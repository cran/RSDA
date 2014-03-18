\name{abalone}
\alias{abalone}
\docType{data}
\title{
SODAS XML data file.
}
\description{
Example of SODAS XML data file converted in a CSV file in RSDA format.
}
\usage{data(abalone)}
\format{
abalone<-SODAS.to.RSDA("C:/Program Files (x86)/DECISIA/SODAS version 2.0/bases/abalone.xml)
}
\source{
http://www.info.fundp.ac.be/asso/sodaslink.htm
}
\references{
Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
complex data. Springer, Germany.
}
\examples{
# We can read the file directly from the SODAS XML file as follows:
# abalone<-SODAS.to.RSDA("C:/Program Files (x86)/DECISIA/SODAS version 2.0/bases/abalone.xml)
# We can save the file in CSV to RSDA format as follows:
# write.sym.table(sodas.ex1, file='abalone.csv', sep=';',dec='.', row.names=TRUE, 
#               col.names=TRUE)
# We read the file from the CSV file, 
# this is not necessary if the file is read directly from 
# XML using SODAS.to.RSDA as in the first statement in this example.
data(abalone)
res<-sym.interval.pca(abalone,'centers')
sym.scatterplot(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
                labels=TRUE,col='red',main='PCA Oils Data')
sym.scatterplot3d(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
                  sym.var(res$Sym.Components,3),color='blue',main='PCA Oils Data')
sym.scatterplot.ggplot(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
                       labels=TRUE)
sym.circle.plot(res$Sym.Prin.Correlations)
}
\keyword{datasets}
