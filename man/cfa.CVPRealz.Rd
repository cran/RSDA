\name{cfa.CVPRealz}
\alias{cfa.CVPRealz}

\title{
Compute the eigenvectors
}
\description{
Compute the eigenvectors to the matrix of column profiles.
}
\usage{
cfa.CVPRealz(sym.data, TFilas, TColumnas, TT, z)
}

\arguments{
  \item{sym.data}{
Should be a symbolic data table read with the function read.sym.table(...).
}
  \item{TFilas}{
Value of the total sum of the rows of the matrix.
}
  \item{TColumnas}{
Value of the total sum of the columns of the matrix.
}
  \item{TT}{
Value of the total sum of the matrix.
}
  \item{z}{
Eigenvectors of the correlation matrix.
}
}

\value{
Return the eigenvectors.
}
\references{
Rodriguez, O. (2011).
Correspondence Analysis for Symbolic Multivalued Variables. Workshop in Symbolic Data Analysis
Namur, Belgium.
}
\author{
Oldemar Rodriguez Rojas
}

\seealso{
\code{\link{sym.cfa}}
}
\examples{
data(ex_cfa1) 
res<-cfa.totals(ex_cfa1)
Z<-cfa.MatrixZ(ex_cfa1,res$TotalRows,res$TotalColumns) 
svd<-eigen(Z)
MVPRealz<-cfa.CVPRealz(ex_cfa1,res$TotalRows,res$TotalColumns,res$Total,svd$vectors)
Mzz<-cfa.Czz(ex_cfa1,res$TotalRows,res$TotalColumns,MVPRealz,svd$values)
CMM<-cfa.minmax(ex_cfa1,res$TotalRows,res$TotalRowsMin,res$TotalRowsMax,
            res$TotalColumns,res$TotalColumnsMin,res$TotalColumnsMax,
            res$Total,MVPRealz,Mzz)
}
\keyword{ Symbolic }
\keyword{ CFA }
