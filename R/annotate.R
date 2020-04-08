#' Annotate eSet or probesets
#' 
#' The function annotates an object of \code{eSet}, or a vector of
#' characters representing probesets.
#' 
#' Once successfully annotated, the \code{annotation} slot of the
#' \code{eSet} object is set to the value of \code{target}.
#' 
#' @param object An object of \code{eSet}, or a character vector of
#' probesets
#' @param target Chip type to be annotated
#' @param check.target Logical, with \code{FALSE} as default. If set to
#' \code{TRUE}, before looking up the annotations, it first check whether
#' \code{target} is one of the valid chip types supported by GTI, and stops if
#' it is not the case.
#' @param \dots Currently not implemented
#' @return An \code{eSet}, or a \code{data.frame} containing
#' annotation information of the probesets.
#' @note Internal function \code{annChar} is called to annotate probesets, and
#' the method for \code{eSet} calls the version for characters to do
#' annotation.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso Internally the function \code{annotateProbesets} in the
#' \code{ribiosAnnotation} package is used to connect to GTI and fetch
#' annotation information.
#' @examples
#' 
#' data(ribios.eSet)
#' myset <- ribios.eSet[100:105,]
#' 
#' ## eSet
#' \dontrun{
#' annotate(myset, "HG_U95AV2")
#' annotate(myset, "HG_U_95AV2", check.target=TRUE)
#' }
#' 
#' ## characters
#' \dontrun{
#' annotate(featureNames(myset), "HG_U95AV2")
#' }
#' 
#' @exportMethod annotate
setGeneric("annotate",
           function(object, target, check.target, ...) standardGeneric("annotate"))

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
#' be first checked. }
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

#' Re-annotate: Transform an eSet object of Bioc-annotation to GTI-annotation
#' 
#' The method accepts an object to re-annotate them. The action of
#' \sQuote{reannotating} defined in \code{ribiosExpression} is to transform an
#' eSet object of Bioconductor-annotation to GTI-annotation.
#' 
#' @name reannotate-methods
#' @aliases reannotate-methods reannotate,eSet,logical-method
#' reannotate,eSet,missing-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"eSet\", check.target =
#' \"logical\")")}{ Currently the method only support an \code{eSet}
#' with a valid Bioconductor annotation set (for example \dQuote{hgu95av2}) in
#' the \code{annotation} slot. See \code{\link{reannotate}} for details and
#' examples.  } \item{list("signature(object = \"eSet\", check.target
#' =" \"missing\")")}{ \code{check.target} is set to FALSE when missing.
#' } }
#' @keywords methods
#' @exportMethod reannotate
setGeneric("reannotate",
           function(object, check.target,...) standardGeneric("reannotate"))

#' Transform an eSet object of Bioc-annotation into of GTI-annotation
#' 
#' The function is used to transform an eSet object, which is
#' annotated by Bioconductor annotation packages, into an object with
#' annotation information from GTI.
#' 
#' The translation between Bioconductor annotation package names and GTI chip
#' types is performed by the \code{bioc2gti} function in the
#' \code{ribiosAnnotation} package.
#' 
#' Once the re-annotation succeeds, the \code{annotation} slot of the
#' \code{eSet} object will be overwritten by the corresponding chip
#' type name in GTI.
#' 
#' @param object An \code{eSet} object, with the \code{annotation}
#' slot set as one of the valid annotations recognized by Bioconductor, for
#' instance \code{hgu95av2}.
#' @param check.target Logical, with \code{FALSE} as default. When set to
#' \code{TRUE}, beforing fetching database for annotations, the function first
#' checks whether the chip type is supported by GTI. If it is not the case, the
#' function will print error message and stop.
#' @param \dots Currently not implemented
#' @return An \code{eSet} object with feature annotations updated by
#' GTI, and the \code{annotation} slot is changed to the chip type in GTI.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{annotate}} to annotate an \code{eSet} object
#' without prior information of bioc-annotation, or if that information is not
#' saved in the \code{annotation} slot.
#' @examples
#' 
#' data(ribios.eSet)
#' print(ribios.eSet)
#' 
#' \dontrun{
#' gti.eSet <- reannotate(ribios.eSet)
#' gti.eSet <- reannotate(ribios.eSet, check.target=FALSE)
#' print(gti.eSet)
#' }
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

#' @describeIn reannotation,eSet,logical-method When 
#'    check.target is missing, it is assumed to be \code{FALSE}
#' @export
setMethod("reannotate",
          c("eSet", "missing"),
          function(object) {
            reannotate(object, check.target=FALSE)
          })
