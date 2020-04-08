#' Transform an expression matrix to long table
#' 
#' @param x A matrix or an ExpressionSet object
#' @param idvar Variable name of the feature identifier, passed to \code{\link{reshape}}
#' @param timevar The time variable, passed to \code{\link{reshape}}
#' @param valuevar The value variable
#' @param ids Feature identifiers
#' @param valueType Character string, value type
#' 
#' @aliases exprsToLong-matrix-method exprsToLong-eSet-method
#' @return A \code{data.frame}
#' @importFrom stats reshape
#' @exportMethod exprsToLong
setGeneric("exprsToLong", function(x,...) standardGeneric("exprsToLong"))

#' @exportMethod contrastNames
setGeneric("contrastNames", function(object) standardGeneric("contrastNames"))
#' @exportMethod designVariables
setGeneric("designVariables", function(object) standardGeneric("designVariables"))

#' @exportMethod groups
setGeneric("groups", function(object) standardGeneric("groups"))
#' @exportMethod dispGroups
setGeneric("dispGroups", function(object) standardGeneric("dispGroups"))
#' @exportMethod designMatrix
setGeneric("designMatrix", function(object) standardGeneric("designMatrix"))
#' @exportMethod contrastMatrix
setGeneric("contrastMatrix", function(object) standardGeneric("contrastMatrix"))
#' @exportMethod nContrast
setGeneric("nContrast", function(object) standardGeneric("nContrast"))


#' Return indices of samples involved in the given contrast of two or more coefficients
#'
#' @title 
#' @param object: A \code{DesignContrast} object
#' @param contrast: Either a contrast name or a integer indicating the index of the contrast
#' @return An integer vector, indices of samples that are involved, sorted by the ascending order of the coefficients of the contrast
#' 
#' @aliases contrastSampleIndices,DesignContrast,numeric-method contrastSampleIndices,DesignContrast,character-method
#' @examples
#' ## one-way ANOVA
#' myDesCon <- parseDesignContrast(sampleGroups="As,Be,As,Be,As,Be",
#'    groupLevels="Be,As", dispLevels="Beryllium,Arsenic", contrasts="As-Be")
#' contrastSampleIndices(myDesCon, 1L)
#' myInterDesCon <- new("DesignContrast", 
#'     design=matrix(c(rep(1,6), rep(0,2), rep(1,2), rep(0,2), 
#'            rep(0,4), rep(1,2)), nrow=6, byrow=FALSE), 
#'     contrasts=matrix(c(0,1,0, 0,0,1, 0,-1,1), byrow=FALSE, nrow=3), 
#'     groups=factor(rep(c("As", "Be", "Cd"), each=2)), 
#'     dispLevels=c("Arsenic", "Beryllium", "Cadmium"))
#' cont1Ind <- contrastSampleIndices(myInterDesCon, 1L)
#' cont2Ind <- contrastSampleIndices(myInterDesCon, 2L)
#' cont3Ind <- contrastSampleIndices(myInterDesCon, 3L)
#' stopifnot(identical(cont1Ind, 1:4))
#' stopifnot(identical(cont2Ind, c(1:2, 5:6)))
#' stopifnot(identical(cont3Ind, c(3:6)))
#' @exportMethod contrastSampleIndices
setGeneric("contrastSampleIndices", function(object, contrast) standardGeneric("contrastSampleIndices"))

#' Make strings in the GMT format
#' 
#' Resulting string(s) can be exported into \code{GMT} file by
#' \code{\link{writeLines}}
#' 
#' @param title Character, title(s) of gene set(s)
#' @param comment Character, comment(s) of gene set(s). Can be of the same
#' length as the \code{title}, or be of length one: in the latter case, it will
#' be replicated in gene set. This option can also be left out: the comment
#' field of the GMT file will be left blank.
#' @param genes A character vector of gene names, or a list of such vectors. In
#' the former case, one GMT line is produced; otherwise multiple lines are
#' returned. In the latter case, the length of the list must match the length
#' of \code{title}.
#' @return One or more lines of GMT file
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' formatGmt(title="GeneSet0", comment="My geneset", genes=c("MAPT", "MAPK", "AKT1"))
#' formatGmt(title="GeneSet0", genes=c("MAPT", "MAPK", "AKT1"))
#' 
#' formatGmt(title=c("GeneSet0", "GeneSet1"),
#'           comment=c("My geneset 0", "My geneset 1"),
#'           genes=list(c("MAPT", "MAPK", "AKT1"), c("EGFR", "CDC42")))
#' formatGmt(title=c("GeneSet0", "GeneSet1"),
#'           comment="My genesets",
#'           genes=list(c("MAPT", "MAPK", "AKT1"), c("EGFR", "CDC42")))
#' formatGmt(title=c("GeneSet0", "GeneSet1"),
#'           genes=list(c("MAPT", "MAPK", "AKT1"), c("EGFR", "CDC42")))
#' @exportMethod formatGmt
setGeneric("formatGmt",
           function(title, comment, genes)
             standardGeneric("formatGmt"))

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
#' data(ribios.ExpressionSet)
#' myset <- ribios.ExpressionSet[100:105,]
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
#' data(ribios.ExpressionSet)
#' print(ribios.ExpressionSet)
#' 
#' \dontrun{
#' gti.eSet <- reannotate(ribios.ExpressionSet)
#' gti.eSet <- reannotate(ribios.ExpressionSet, check.target=FALSE)
#' print(gti.eSet)
#' }
#' @aliases reannotate,eSet,logical-method reannotate,eSet,missing-method
#' @docType methods
#' @keywords methods
#' @exportMethod reannotate
setGeneric("reannotate",
           function(object, check.target,...) standardGeneric("reannotate"))

#' Export matrix or eEset that can be coereced as one into gct/cls files
#' @keywords methods
#' @param obj The input object, see methods below for supported data types
#' @param file The output file
#' @param feat.name Specifying feature names
#' @param feat.desc Specifying feature descriptions
#' @aliases writeGct,matrix,ANY,ANY,ANY-method writeGct,eSet,ANY,ANY,ANY-method
#' @exportMethod writeGct
setGeneric("writeGct", function(obj, file, feat.name, feat.desc) 
	   standardGeneric("writeGct"))
