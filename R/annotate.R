#' Annotate an object with the target
#' @exportMethod annotate
setGeneric("annotate",
           function(object, target, check.target, ...) standardGeneric("annotate"))

annChar <- function(object, target, check.target=FALSE) {
  if(!require(ribiosAnnotation))
    stop("Missing 'ribiosAnnotation' package: the annotation functionality is not available.")
  if(check.target) {
    has.target <- target %in% gtiChiptypes()
    if(!has.target)
      stop("Chip type ", target, " is not supported by GTI.\n",
           "Call 'gtiChiptypes()' to see supported types")
  }
  return(annotateProbesets(object, target))
}

#' Annotate an ExpressionSet object with a chip type
#' @param object An ExpressionSet
#' @param target An chiptype in \code{ribiosAnnotation::gtiChiptypes()}
#' @param check.target Logical, whether checking the target exists or not
#' @export
setMethod("annotate",
          c("ExpressionSet", "character", "logical"),
          function(object, target, check.target) {
            ann <- annChar(featureNames(object),
                            target,
                            check.target=check.target)
            fData(object) <- ann
            annotation(object) <- target
            return(object)
          })

#' Annotate an ExpressionSet object with a chip type, do not perform checking
#' @param object An ExpressionSet
#' @param target An chiptype in \code{ribiosAnnotation::gtiChiptypes()}
#' @export
setMethod("annotate",
          c("ExpressionSet", "character","missing"),
          function(object, target) {
            annotate(object, target, check.target=FALSE)
          })

#' Annotate probesets with a chip type
#' @param object A vector of chacater strings
#' @param target An chiptype in \code{ribiosAnnotation::gtiChiptypes()}
#' @param check.target Logical, whether checking the target exists or not
#' @export
setMethod("annotate",
          c("character", "character", "logical"),
          function(object, target, check.target) {
            annChar(object, target, check.target)
          })

#' Annotate probesets with a chip type without checking
#' @param object A vector of chacater strings
#' @param target An chiptype in \code{ribiosAnnotation::gtiChiptypes()}
#' @export
setMethod("annotate",
          c("character", "character", "missing"),
          function(object, target) {
            annChar(object, target, check.target=FALSE)
          })

#' Re-annotate an object
#' @exportMethod reannotate
setGeneric("reannotate",
           function(object, check.target,...) standardGeneric("reannotate"))

#' Re-annotate an ExpressionSet with Roche Bioinformatics Infrastructure
#' 
#' Annotation type is inferred from the annotation label of Bioconductor
#' 
#' @param object An ExpressionSet object
#' @param check.target Logical, checking whether the annotation is supported
#' @export
setMethod("reannotate",
          c("ExpressionSet", "logical"),
          function(object, check.target) {
            old.ann <- annotation(object)
            new.ann <- bioc2gti(old.ann)
            if(check.target && is.na(new.ann)) {
              stop("Chip type '", old.ann, "' of Bioconductor ",
                   "has no corresponding annotation information ",
                   "in GTI.\n",
                   "Call 'gtiChiptypes()' to see supported types")
            }
            annotate(object, new.ann)
          })

#' Re-annotate an ExpressionSet with Roche Bioinformatics Infrastructure without checking
#' @param object An ExpressionSet object
#' @export
setMethod("reannotate",
          c("ExpressionSet", "missing"),
          function(object) {
            reannotate(object, check.target=FALSE)
          })
