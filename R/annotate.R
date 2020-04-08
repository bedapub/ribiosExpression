#' @include AllGenerics.R

annChar <- function(object, target, check.target=FALSE) {
  if(check.target) {
    has.target <- target %in% gtiChiptypes()
    if(!has.target)
      stop("Chip type ", target, " is not supported by GTI.\n",
           "Call 'gtiChiptypes()' to see supported types")
  }
  return(annotateProbesets(object, target))
}

#' Annotate an eSet object with a chip type
#' @param object An eSet
#' @param target An chiptype in \code{ribiosAnnotation::gtiChiptypes()}
#' @param check.target Logical, whether checking the target exists or not.
#' 
#' Annotate an \code{eSet} with the chip type defined by the value of
#' \code{target}. \code{check.target} determines whether the chip type should
#' be first checked. 
#' 
#' @export
setMethod("annotate",
          c("eSet", "character", "logical"),
          function(object, target, check.target) {
            ann <- annChar(featureNames(object),
                            target,
                            check.target=check.target)
            fData(object) <- ann
            annotation(object) <- target
            return(object)
          })

#' Annotate an eSet object with a chip type, do not perform checking
#' @param object An eSet
#' @param target An chiptype in \code{ribiosAnnotation::gtiChiptypes()}
#' @export
setMethod("annotate",
          c("eSet", "character","missing"),
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


#' @describeIn reannotate Method for eSet and logical
#' @export
setMethod("reannotate",
          c("eSet", "logical"),
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

#' @describeIn reannotate Method for eSet, not checking the target
#' @export
setMethod("reannotate",
          c("eSet", "missing"),
          function(object) {
            reannotate(object, check.target=FALSE)
          })
