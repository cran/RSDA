\name{oils}
\alias{oils}
\docType{data}
\title{
Ichino Oils example data
}
\description{
Symbolic data matrix with all the variables of interval type
}
\usage{data(oils)}
\format{
$I   GRA   GRA $I FRE FRE $I IOD IOD $I SAP SAP

L  $I 0.930 0.935 $I -27 -18 $I 170 204 $I 118 196

P  $I 0.930 0.937 $I  -5  -4 $I 192 208 $I 188 197

Co $I 0.916 0.918 $I  -6  -1 $I  99 113 $I 189 198

S  $I 0.920 0.926 $I  -6  -4 $I 104 116 $I 187 193

Ca $I 0.916 0.917 $I -25 -15 $I  80  82 $I 189 193

O  $I 0.914 0.919 $I   0   6 $I  79  90 $I 187 196

B  $I 0.860 0.870 $I  30  38 $I  40  48 $I 190 199

H  $I 0.858 0.864 $I  22  32 $I  53  77 $I 190 202	
}
\references{
Cazes P., Chouakria A., Diday E. et Schektman Y. (1997).  Extension de l'analyse en 
composantes principales a des donnees de type intervalle, Rev. Statistique Appliquee, 
Vol. XLV Num. 3 pag. 5-24, France.
}
\examples{
data(oils)
display.sym.table(oils)
}
\keyword{datasets}
