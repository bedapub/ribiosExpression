#' @include DesignContrast.R
NULL

#' Remove all-zero variables from design matrix and the corresponding contrast matrix
#' @param obj Either a design matrix, rows are samples, columns are 
#'   independent variables, and values are coefficients. Or a 
#'   \code{DesignContrast} object that contains both design and contrast 
#'   matrices. The design matrix may contain columns of all zeros, the 
#'   independent variable corresponding to which can not be estimated. 
#' @param contrasts Either \code{NULL}, in case \code{designMatrix} is a 
#'   \code{DesignContrast} object, or a contrast matrix matching the design 
#'   matrix, i.e. the number of rows must equal the number of column of the design matrix.
#'    Rows are independent variables, columns are names of the contrasts to be 
#'    estimated, the values are operations applied to the coefficients.
#'
#' @return Either a list of two matrices (\code{design} and 
#'   \code{contrasts}), or a \code{DesignContrast} object, 
#'   depending on the input parameter type. The design matrix and contrast 
#'   matrix have an attribute each, \code{notEstCoefs} and 
#'   \code{notEstContrasts}, that keep track of filtered variables and contrasts.
#' @examples 
#' myTestDesign <- matrix(c(1,1,1,1, 1,1,0,0,0,0,1,1,0,0,0,0), 
#'   byrow=FALSE, nrow=4L, 
#'   dimnames=list(sprintf("S%d", 1:4), c("Baseline", "Trt1", "Trt2", "Trt3")))
#' myTestContrast <- matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1), nrow=4L, byrow=FALSE,
#'   dimnames=list(colnames(myTestDesign), c("Trt1", "Trt2", "Trt3")))
#' removeAllZeroVar(myTestDesign, myTestContrast)
#' removeAllZeroVar(DesignContrast(myTestDesign, myTestContrast))
#' @export
removeAllZeroVar <- function(obj, contrasts) UseMethod("removeAllZeroVar")

#' @describeIn removeAllZeroVar S3 function for matrix as input
#' @export
removeAllZeroVar.matrix <- function(obj, contrasts) {
  isNotEst <- apply(obj, 2, function(x) all(x==0))
  if(any(isNotEst)) {
    notEstCoefs <- colnames(obj)[isNotEst]
    obj <- obj[, !isNotEst, drop=FALSE]
    attr(obj, "notEstCoefs") <- notEstCoefs
    indInvalidContrast <- unlist(apply(contrasts[isNotEst,,drop=FALSE],
                                1, function(x) which(x!=0)))
    if(length(indInvalidContrast)==0) {
      keepContrast <- rep(TRUE, ncol(contrasts))
    } else {
      keepContrast <- setdiff(1:ncol(contrasts), indInvalidContrast)
    }
    notEstContrasts <- colnames(contrasts)[indInvalidContrast]
    contrasts <- contrasts[!isNotEst, keepContrast]
    attr(contrasts, "notEstContrasts") <- notEstContrasts
  }
  res <- list(design=obj, contrasts=contrasts)
  return(res)
}

#' @describeIn removeAllZeroVar S3 function for matrix as input
#' @export
removeAllZeroVar.DesignContrast <- function(obj, contrasts=NULL) {
  reslist <- removeAllZeroVar.matrix(designMatrix(obj),
                                     contrastMatrix(obj))
  designMatrix(obj) <- reslist$design
  contrastMatrix(obj) <- reslist$contrasts
  return(obj)
}
