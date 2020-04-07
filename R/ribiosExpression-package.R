

#' Annotate an object
#' 
#' Annotate an object.
#' 
#' 
#' @name annotate-methods
#' @aliases annotate-methods annotate,ExpressionSet,character,logical-method
#' annotate,ExpressionSet,character,missing-method
#' annotate,character,character,logical-method
#' annotate,character,character,missing-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"ExpressionSet\", target =
#' \"character\",\n", " check.target = \"logical\")")}{ Annotate an
#' \code{ExpressionSet} with the chip type defined by the value of
#' \code{target}. \code{check.target} determines whether the chip type should
#' be first checked. } \item{list("signature(object = \"ExpressionSet\", target
#' = \"character\",\n", " check.target = \"missing\")")}{ \code{check.target}
#' is set to FALSE when missing. }
#' 
#' \item{list("signature(object = \"character\", target = \"character\",
#' check.target=\"logical\")")}{ Treat \code{object} as a vector of probesets,
#' which are to be annotated according to the information of chip specified by
#' \code{target} }
#' 
#' \item{list("signature(object = \"character\", target = \"character\",
#' check.target=\"missing\")")}{ \code{check.target} is set to FALSE when
#' missing. }
#' 
#' }
#' @keywords methods
NULL





#' Annotate ExpressionSet or probesets
#' 
#' The function annotates an object of \code{ExpressionSet}, or a vector of
#' characters representing probesets.
#' 
#' Once successfully annotated, the \code{annotation} slot of the
#' \code{ExpressionSet} object is set to the value of \code{target}.
#' 
#' @param object An object of \code{ExpressionSet}, or a character vector of
#' probesets
#' @param target Chip type to be annotated
#' @param check.target Logical, with \code{FALSE} as default. If set to
#' \code{TRUE}, before looking up the annotations, it first check whether
#' \code{target} is one of the valid chip types supported by GTI, and stops if
#' it is not the case.
#' @param \dots Currently not implemented
#' @return An \code{ExpressionSet}, or a \code{data.frame} containing
#' annotation information of the probesets.
#' @note Internal function \code{annChar} is called to annotate probesets, and
#' the method for \code{ExpressionSet} calls the version for characters to do
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
#' ## ExpressionSet
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
NULL





#' Make GMT format strings
#' 
#' Make GMT format strings with titles, comments and genes
#' 
#' 
#' @name formatGmt-methods
#' @aliases formatGmt-methods formatGmt,character,character,character-method
#' formatGmt,character,character,list-method
#' formatGmt,character,missing,character-method
#' formatGmt,character,missing,list-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(title = \"character\", comment = \"character\", genes
#' = \"character\")")}{ For one line of GMT: containing one title, one comment
#' and a vector character of genes }
#' 
#' \item{list("signature(title = \"character\", comment = \"character\", genes
#' = \"list\")")}{ For multiple lines of GMT: containing titles, comments and a
#' list of genes. Each item of the list is a vector of character. The title and
#' the genes list must be of the same length. The length of comment can be
#' equal to that of the title, or can be one; in the latter case, the comment
#' will be replicated into each line of the GMT strings. }
#' 
#' \item{list("signature(title = \"character\", comment = \"missing\", genes
#' =\n", " \"character\")")}{ When comment is missing, it is set to empty
#' string
#' 
#' }
#' 
#' \item{list("signature(title = \"character\", comment = \"missing\", genes
#' =\n", " \"list\")")}{ When comment is missing, it is set to empty string } }
#' @keywords methods
NULL





#' Make GMT format strings
#' 
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
#' 
#' 
NULL





#' Interface to function kendallWmat
#' 
#' The S4 method acts as interface between advanced data structures (such as
#' \code{ExpressionSet}) and the \code{kendallWmat} function. The function
#' combines Kendall's W statistic with an iterative graph theory approach to
#' detect sub-groups resembling strong correlations.
#' 
#' 
#' @name kendallW-methods
#' @aliases kendallW kendallW-methods kendallW,ExpressionSet-method
#' kendallW,matrix-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(object =
#' \"matrix\")")}{The method for \code{matrix} is just a wrapper for
#' \code{kendallWmat}} \item{list("signature(object =
#' \"ExpressionSet\")")}{This method accepts an \code{ExpressionSet} object,
#' performs Kendall'W feature reduction, and store the sub-group information in
#' the resulting object. } }
#' @keywords methods
NULL





#' Re-annotate: Transform an ExpressionSet object of Bioc-annotation in
#' GTI-annotation
#' 
#' The method accepts an object to re-annotate them. The action of
#' \sQuote{reannotating} defined in \code{ribiosExpression} is to transform an
#' ExpressionSet object of Bioconductor-annotation to GTI-annotation.
#' 
#' 
#' @name reannotate-methods
#' @aliases reannotate-methods reannotate,ExpressionSet,logical-method
#' reannotate,ExpressionSet,missing-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"ExpressionSet\", check.target =
#' \"logical\")")}{ Currently the method only support an \code{ExpressionSet}
#' with a valid Bioconductor annotation set (for example \dQuote{hgu95av2}) in
#' the \code{annotation} slot. See \code{\link{reannotate}} for details and
#' examples.  } \item{list("signature(object = \"ExpressionSet\", check.target
#' =\n", " \"missing\")")}{ \code{check.target} is set to FALSE when missing.
#' } }
#' @keywords methods
NULL





#' Transform an ExpressionSet object of Bioc-annotation into of GTI-annotation
#' 
#' The function is used to transform an ExpressionSet object, which is
#' annotated by Bioconductor annotation packages, into an object with
#' annotation information from GTI.
#' 
#' The translation between Bioconductor annotation package names and GTI chip
#' types is performed by the \code{bioc2gti} function in the
#' \code{ribiosAnnotation} package.
#' 
#' Once the re-annotation succeeds, the \code{annotation} slot of the
#' \code{ExpressionSet} object will be overwritten by the corresponding chip
#' type name in GTI.
#' 
#' @param object An \code{ExpressionSet} object, with the \code{annotation}
#' slot set as one of the valid annotations recognized by Bioconductor, for
#' instance \code{hgu95av2}.
#' @param check.target Logical, with \code{FALSE} as default. When set to
#' \code{TRUE}, beforing fetching database for annotations, the function first
#' checks whether the chip type is supported by GTI. If it is not the case, the
#' function will print error message and stop.
#' @param \dots Currently not implemented
#' @return An \code{ExpressionSet} object with feature annotations updated by
#' GTI, and the \code{annotation} slot is changed to the chip type in GTI.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{annotate}} to annotate an \code{ExpressionSet} object
#' without prior information of bioc-annotation, or if that information is not
#' saved in the \code{annotation} slot.
#' @examples
#' 
#' data(ribios.ExpressionSet)
#' print(ribios.ExpressionSet)
#' 
#' \dontrun{
#' gti.ExpressionSet <- reannotate(ribios.ExpressionSet)
#' gti.ExpressionSet <- reannotate(ribios.ExpressionSet, check.target=FALSE)
#' print(gti.ExpressionSet)
#' }
#' 
NULL





#' An ExpressionSet for case demonstrations
#' 
#' This object is adapted from the \code{sample.ExpressionSet} object, with
#' feature annotations from GTI (Data stand: December 2011). It is used in case
#' studies where functionalities of the \code{ribiosExpression} package are
#' demonstrated.
#' 
#' 
#' @name ribios.ExpressionSet
#' @docType data
#' @format An \code{ExpressionSet} object.
#' @references Jitao David Zhang <jitao_david.zhang@roche.com>
#' @keywords datasets
#' @examples
#' 
#' data(ribios.ExpressionSet)
#' tbl <- eSetToLongTable(ribios.ExpressionSet)
#' 
NULL





#' writeGct methods
#' 
#' Write matrix or ExpressionSet in GCT file format
#' 
#' 
#' @name writeGct-methods
#' @aliases writeGct-methods writeGct,ExpressionSet-method
#' writeGct,matrix-method
#' @docType methods
#' @section Methods: \describe{ The S4-method is a wrapper of the
#' \code{write_gct} function implemented in the \code{ribiosIO} package.
#' \item{list("signature(obj = \"ExpressionSet\")")}{ An \code{ExpressionSet}
#' object. }
#' 
#' \item{list("signature(obj = \"matrix\")")}{ A matrix } }
#' @keywords methods
NULL



