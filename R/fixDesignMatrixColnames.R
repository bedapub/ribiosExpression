#' Fix design matrix colnames so that they are legal variable names
#' @param designMatrix A design matrix, produced by \code{model.matrix}
#' @param interceptChar Character string, the value the interaction symbol (:) should be replaced with
#' @param removeContrastNames Logical, whether the contrast variable name should be removed.
#' 
#' @return The matrix with fixed colum names.
#' @importFrom stats model.matrix
#' @examples 
#' myFac1 <- gl(6,2, labels=sprintf("Fac1_%d", 1:6))
#' myFac2 <- gl(2,6, labels=c("Ctrl", "Dis"))
#' myVar <- rnorm(12)
#' myDesign <- model.matrix(~myFac1 * myFac2 + myVar)
#' head(myDesign)
#' head(fixDesignMatrixColnames(myDesign))
#' @export
fixDesignMatrixColnames <- function(designMatrix,
                            interceptChar="_",
                            removeContrastNames=TRUE) {
  contrasts <- attr(designMatrix, "contrasts")
  stopifnot(length(removeContrastNames)==1 |
              length(removeContrastNames)==length(contrasts))
  removeContrastNames <- rep_len(removeContrastNames, 
                                 length.out=length(contrasts))
  cnames <- colnames(designMatrix)
  varnames <- names(contrasts)
  for (i in seq(along=varnames)) {
    if(removeContrastNames[i]) {
      vn <- make.names(varnames[i])
      cnames <- sub(vn, "", cnames)
    }
  }
  cnames <- gsub(":", interceptChar, cnames)
  cnames[1] <- "Baseline"
  cnames <- make.names(cnames)
  colnames(designMatrix) <- cnames
  return(designMatrix)
}
