\name{cfa.Czz}
\alias{cfa.Czz}

\title{
Compute the eigenvectors
}

\description{
Compute the eigenvectors to the matrix of row profiles.
}
\usage{
cfa.Czz(sym.data, TFilas, TColumnas, VPRealz, d)
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
  \item{VPRealz}{
The output of the function cfa.CVPRealz.
}
  \item{d}{
Eigenvalues.
}
}

\value{
Return the eigenvectors.
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
