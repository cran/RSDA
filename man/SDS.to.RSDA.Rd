\name{SDS.to.RSDA}
\alias{SDS.to.RSDA}

\title{
SDS SODAS files to RSDA files.
}
\description{
To convert SDS SODAS files to RSDA files.
}
\usage{
SDS.to.RSDA(file.path, labels = FALSE)
}

\arguments{
  \item{file.path}{
Disk path where the SODAS *.SDA file is.
}
  \item{labels}{
 If we want to include SODAS SDA files lebels in RSDA file.
}
}

\value{
A RSDA symbolic data file.
}
\references{
Bock H-H. and Diday E. (eds.) (2000).
 Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
complex data. Springer, Germany.
}
\author{
Olger Calderon and Roberto Zuniga.
}

\seealso{
\code{\link{SODAS.to.RSDA}}
}
\examples{
# We can read the file directly from the SODAS SDA file as follows:
# We can save the file in CSV to RSDA format as follows:
# setwd("C:/Program Files (x86)/DECISIA/SODAS version 2.0/bases/")
# result <- SDS.to.RSDA(file.path="hani3101.sds")
# We can save the file in CSV to RSDA format as follows:
# write.sym.table(result, file='hani3101.csv', sep=';',dec='.', row.names=TRUE,
#                 col.names=TRUE)
}

\keyword{ symbolic }
\keyword{ data }
\keyword{ table }

