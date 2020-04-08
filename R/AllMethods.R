#' @include AllClasses.R AllGenerics.R
NULL

#' Return contrast names of a DesignContrast object
#' @param object A DesignContrast object
#' @export
setMethod("contrastNames", "DesignContrast", function(object)
          return(colnames(contrastMatrix(object))))

#' @describeIn exprsToLong The method for matrix as input
setMethod("exprsToLong", "matrix", function(x, idvar="illID",timevar="hybridID", valuevar="value", 
                                            ids=rownames(x), valueType="raw") {
  x <- as.data.frame(x)
  colnames(x) <- paste(valuevar, colnames(x), sep=".")
  va <- 1:ncol(x)
  x[,idvar] <- ids
  xLong <- reshape(x, idvar=idvar, varying=va, timevar=timevar, direction="long")
  rownames(xLong) <- NULL
  xLong$type <- valueType
  xLong <- xLong[,c(idvar, timevar, "type", valuevar)]
  return(xLong)
})

#' @describeIn exprsToLong The method for eSet as input
#' @export
setMethod("exprsToLong", "eSet", function(x,...) {
  exprsToLong(exprs(x),...)
})

#' Perform row-wise scaling to an ExpressionSet object
#' @param x An ExpressionSet object.
#' @param center Logical, whether the mean values of rows should be set to zero.
#' @param scale Logical, whether the standard deviations of rows should be normalised to one.
#' 
#' @importFrom ribiosUtils rowscale
#' @export
rowscale.ExpressionSet <- function(x, center=TRUE, scale=TRUE) {
  if(storageMode(x)!="lockedEnvironment")
    warning("The storageMode of the input object is not 'lockedEnvironment': exprs is replaced by row-scaled values\n",
            "To prevent damaging the data integrity, set the storageMode to 'lockedEnvironment'")
  
  exprs(x) <- t(scale(t(exprs(x)),
                      center=center, scale=scale))
  return(x)
}

#' Extract design matrix from MArrayLM
#'
#' @param object A MArrayLM object from the limma package
#' 
#' @importClassesFrom limma MArrayLM
#' @return Design matrix
setMethod("designMatrix", "MArrayLM", function(object) {
    return(object$design)
})

#' Extract contrast matrix from MArrayLM
#'
#' @param object A MArrayLM object from the limma package
#' @return contrast matrix
setMethod("contrastMatrix", "MArrayLM", function(object) {
    return(object$contrast)
})

#' Extract contrast names from MArrayLM
#'
#' @param object A MArrayLM object from the limma package
#' @return Character vector of contrast names
setMethod("contrastNames", "MArrayLM", function(object) {
    return(colnames(contrastMatrix(object)))
})
