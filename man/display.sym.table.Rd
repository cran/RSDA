\name{display.sym.table}
\alias{display.sym.table}
\title{
display.sym.table
}
\description{
This function display a symbolic data table tha have been read by read.sym.table(...)
}
\usage{
display.sym.table(sym.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym.data}{ 
  	Shoud be a Symbolic Data table that have been read with read.sym.table(...)
}
}
\details{
The output will be the symbolic data table in the screen.
}
\value{
Not value.
}
\references{
 Billard, L and  Diday, E. (2007). Symbolic Data Analysis: Conceptual Statistics and Data
 Mining (Wiley Series in Computational Statistics).	
	
Billard, L and  Diday, E. (2003). From the Statistics of Data to the Statistics of 
Knowledge: Symbolic Data Analysis. Journal of the American of the Statistical Association, USA.
}
\author{
Oldemar Rodriguez Rojas
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	 \code{\link{read.sym.table}}
}
\examples{
data(example3)
display.sym.table(example3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Display}
\keyword{ Symbolic}% __ONLY ONE__ keyword per line
\keyword{ Table}