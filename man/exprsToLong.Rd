% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/AllMethods.R
\name{exprsToLong}
\alias{exprsToLong}
\alias{exprsToLong-matrix-method}
\alias{exprsToLong-eSet-method}
\alias{exprsToLong,matrix-method}
\alias{exprsToLong,eSet-method}
\title{Transform an expression matrix to long table}
\usage{
exprsToLong(x, ...)

\S4method{exprsToLong}{matrix}(
  x,
  idvar = "illID",
  timevar = "hybridID",
  valuevar = "value",
  ids = rownames(x),
  valueType = "raw"
)

\S4method{exprsToLong}{eSet}(x)
}
\arguments{
\item{x}{A matrix or an ExpressionSet object}

\item{...}{Other parameters}

\item{idvar}{Variable name of the feature identifier, passed to \code{\link{reshape}}}

\item{timevar}{The time variable, passed to \code{\link{reshape}}}

\item{valuevar}{The value variable}

\item{ids}{Feature identifiers}

\item{valueType}{Character string, value type}
}
\value{
A \code{data.frame}
}
\description{
Transform an expression matrix to long table
}
\section{Methods (by class)}{
\itemize{
\item \code{exprsToLong(matrix)}: The method for matrix as input

\item \code{exprsToLong(eSet)}: The method for eSet as input

}}
