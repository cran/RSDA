% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sym_regression.R
\name{sym.rt}
\alias{sym.rt}
\title{Symbolic Regression Trees}
\usage{
sym.rt(
  formula,
  sym.data,
  method = c("cm", "crm"),
  minsplit = 20,
  maxdepth = 10
)
}
\arguments{
\item{formula}{a formula, with a response but no interaction terms. If this a a data frame, that is taken as the model frame (see model.frame).}

\item{sym.data}{a symbolic data table}

\item{method}{cm crm}

\item{minsplit}{the minimum number of observations that must exist in a node in order for a split to be attempted.}

\item{maxdepth}{Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines.}
}
\description{
Symbolic Regression Trees
}
\references{
Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515


Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347


Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y


Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38
}
