#' @include AllClasses.R AllGenerics.R
NULL

##-----------------------##
## DesignContrast methods
##-----------------------##

#' @describeIn groups Return the raw sample groups from a DesignContrast object
#' @export
setMethod("groups", "DesignContrast", function(object) {
return(object@groups)
})

#' @describeIn dispGroups Return the sample groups from a DesignContrast object
#'     , suing display labels
#' @export
setMethod("dispGroups", "DesignContrast", function(object) {
  groups <- object@groups
  levels(groups) <- object@dispLevels
  return(groups)
})

#' @describeIn designMatrix Return the design matrix from a DesignContrast
#'     object
#' @export
setMethod("designMatrix", "DesignContrast", function(object) {
  return(object@design)
})

#' @describeIn designMatrix Assign a design matrix to a \code{DesignContrast}
#'     object
#' @param value A design matrix
#' @export
setReplaceMethod("designMatrix", "DesignContrast", function(object, value) {
  object@design <- value
  return(object)
})

#' @describeIn contrastMatrix Return the contrast matrix from a DesignContrast
#'     object
#' @export
setMethod("contrastMatrix", "DesignContrast", function(object) {
  return(object@contrasts)
})

#' @describeIn contrastMatrix Assign a contrast matrix to a \code{contrastContrast}
#'     object
#' @param value A contrast matrix
#' @export
setReplaceMethod("contrastMatrix", "DesignContrast", function(object, value) {
  object@contrasts <- value
  return(object)
})

#' @describeIn contrastAnnotation Return the contrast annotation data.frame
#'   from a DesignContrast object
#' @export
setMethod("contrastAnnotation", "DesignContrast", function(object) {
  return(object@contrastAnnotation)
})

#' @describeIn contrastAnnotation Assign a contrast annotation data.frame
#'   to a \code{contrastContrast} object
#' @param value A contrast annotation data.frame
#' @export
setReplaceMethod("contrastAnnotation", "DesignContrast", function(object, value) {
  object@contrastAnnotation <- value
  return(object)
})

#' @describeIn nContrast Return the number of contrast in a DesignContras
#'     object
#' @export
setMethod("nContrast", "DesignContrast", function(object) {
  return(ncol(object@contrasts))
})

#' @describeIn designVariables Return the names of variables (column names) 
#'    in the design matrix of a DesignContrast object
#' @export
setMethod("designVariables", "DesignContrast", function(object) {
  return(colnames(designMatrix(object)))
})

#' @describeIn contrastNames Return contrast names, i.e., column names of the
#'   contrast matrix
#' @export
setMethod("contrastNames", "DesignContrast", function(object)
          return(colnames(contrastMatrix(object))))

##---------------##
## exprsToLong
##---------------##
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
setMethod("exprsToLong", "eSet", function(x) {
  exprsToLong(exprs(x))
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

##----------------------------------------------##
## design/contrast matrix from MArrayLM objects
##----------------------------------------------##

#' @describeIn designMatrix Extract design matrix from an object of MArrayLM
#' @importClassesFrom limma MArrayLM
#' @export
setMethod("designMatrix", "MArrayLM", function(object) {
    return(object$design)
})


#' @describeIn contrastMatrix Extract contrast matrix from an object of MArrayLM
#' @export
setMethod("contrastMatrix", "MArrayLM", function(object) {
    return(object$contrast)
})

#' @describeIn contrastNames Extract contrast names from an object of MArrayLM
#' @export
setMethod("contrastNames", "MArrayLM", function(object) {
    return(colnames(contrastMatrix(object)))
})

##--------------------##
## formatGmt methods
##--------------------##

#' @describeIn formatGmt title, comment, and genes are one character string
#' @export
setMethod("formatGmt",
          c("character", "character", "character"),
          function(title, comment,genes) {
            if(length(title)!=1L)
              stop("'title' must be a character string")
            if(length(comment)!=1L)
              stop("'comment' must be a character string")
            genes <- unique(genes)
            genes.collapse <- paste(genes, collapse="\t")
            paste(title, comment, genes.collapse,sep="\t")
          })

#' @describeIn formatGmt title and genes are both one character string, comments are missing
#' @export
setMethod("formatGmt",
          c("character", "missing", "character"),
          function(title, genes) {
            formatGmt(title, "", genes)
          })

#' @describeIn formatGmt title and comments are both vectors of character
#'     strings, genes are a list of the same length
#' @export
setMethod("formatGmt",
          c("character", "character", "list"),
          function(title, comment,genes) {
            if(!identical(length(title), length(genes))) {
              stop("'genes' must be a list of the same length as the character vector 'titles'")
            }
            if(length(comment)==1)
              comment <- rep(comment, length(title))
            stopifnot(identical(length(title), length(comment)))
            sapply(1:length(genes),
                   function(x) formatGmt(title[x], comment[x], genes[[x]])
            )
          })

#' @describeIn formatGmt title is vectors of character strings, comments are
#'     missing, genes are a list of the same length as the title
#' @export
setMethod("formatGmt",
          c("character", "missing", "list"),
          function(title, genes) {
            formatGmt(title, "", genes)
          })
