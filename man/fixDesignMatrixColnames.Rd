% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixDesignMatrixColnames.R
\name{fixDesignMatrixColnames}
\alias{fixDesignMatrixColnames}
\title{Fix design matrix colnames so that they are legal variable names}
\usage{
fixDesignMatrixColnames(
  designMatrix,
  interceptChar = "_",
  removeContrastNames = TRUE
)
}
\arguments{
\item{designMatrix}{A design matrix, produced by \code{model.matrix}}

\item{interceptChar}{Character string, the value the interaction symbol (:) should be replaced with}

\item{removeContrastNames}{Logical, whether the contrast variable name should be removed.}
}
\value{
The matrix with fixed colum names.
}
\description{
Fix design matrix colnames so that they are legal variable names
}
\examples{
myFac1 <- gl(6,2, labels=sprintf("Fac1_\%d", 1:6))
myFac2 <- gl(2,6, labels=c("Ctrl", "Dis"))
myVar <- rnorm(12)
myDesign <- model.matrix(~myFac1 * myFac2 + myVar)
head(myDesign)
head(fixDesignMatrixColnames(myDesign))
}
