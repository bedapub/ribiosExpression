#' Study design and contrast information
#'
#' The DesignContrast class represents key information in a designed experiment
#'
#' @slot design A numeric matrix. The number of rows equals the
#'     sample size. The columns corresponds to the variables of design
#' @slot contrasts A numeric matrix. The number of rows equals the number of
#'     columns in the design matrix. The columns corresponds to the comparisons
#'     one wishes to make.
#' @slot groups A factor vector, giving sample groups.
#'     The length equals the number of samples.
#' @slot dispLevels A character vector, used for displaying sample groups.
#'     The length equals the number of levels of the \code{groups} factor.
#' @slot contrastAnnotation A data.frame, used to annotate the contrasts.
#'
#' @param object An object of \code{DesignContrast}.
#'
#' @name DesignContrast-class
#' @aliases show,DesignContrast-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the
#' function \code{DesignContrast}. However, the users should not directly call this
#' function. Instead, \code{\link{parseDesignContrast}} should be called.
#' @keywords classes
#' @exportClass DesignContrast
setClass("DesignContrast",
         representation=list(design="matrix",
           contrasts="matrix",
           groups="factor",
           dispLevels="character",
           contrastAnnotation="data.frame"),
         validity=function(object) {
           ribiosUtils::haltifnot(valid.gd <- length(object@groups)==nrow(object@design),
                                  msg=sprintf("object groups length (%d) does not equal design matrix nrow (%d)\n",
                                    length(object@groups), nrow(object@design)))

           ribiosUtils::haltifnot(valid.gr <- length(object@dispLevels)==nlevels(object@groups),
                                  msg=sprintf("Length of displayed levels (%d) does not match the levels of groups (%d)\n",
                                    length(object@dispLevels),
                                    nlevels(object@groups)))
           ribiosUtils::haltifnot(valid.dc <- ncol(object@design)==nrow(object@contrasts),
                                  msg=sprintf("Ncol of design matrix (%d) does not match the nrow of the contrast matrix (%d)\n",
                                    ncol(object@design),nrow(object@contrasts)))
           ribiosUtils::haltifnot(valid.ca <- nrow(object@contrastAnnotation)==ncol(object@contrasts),
                                  msg=sprintf("Nrow of contrast annotation (%d) does not match the ncol of the contrast matrix (%d)\n",
                                              nrow(object@contrastAnnotation), ncol(object@contrasts)))
           return(valid.gd & valid.gr & valid.dc & valid.ca)
         })

#' @describeIn DesignContrast-class The show method
#' @importMethodsFrom methods show
#' @importFrom ribiosUtils headtail
#' @export
setMethod("show", "DesignContrast", function(object) {
  cat("DesignContrast object:\n")
  grps <- object@groups
  des <- object@design
  con <- object@contrasts
  cat(sprintf("- %d samples in %d groups\n",
              length(grps), nlevels(grps)))
  cat(sprintf("    Levels: %s\n",
             headtail(levels(grps))))
  cat(sprintf("- Design matrix (%d samples x %d variables)\n",
              nrow(des), ncol(des)))
  cat(sprintf("    Variables: %s\n",
              headtail(colnames(des))))
  cat("  Call 'designMatrix(object)' to get the design matrix.\n")
  cat(sprintf("- Contrast matrix (%d variables x %d contrasts)\n",
              nrow(con), ncol(con)))
  cat(sprintf("    Contrasts: %s\n",
              headtail(colnames(con))))
  cat("  Call 'contrastMatrix(object)' to get the contrast matrix.\n")
  cat("  Call 'contrastAnnotation(object) to get the contrast annotation.\n")
})
