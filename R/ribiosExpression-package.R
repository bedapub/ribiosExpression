#' Data structures and functions for gene expression analysis
#' @docType package
#' @description Provides data structures and functions for gene expression analysis
#' @name ribiosExpression-package
NULL

#' @importFrom methods as 
#' @importFrom stats cor median prcomp qt sd
#' @importFrom utils write.table
#' @importFrom ribiosUtils haltifnot assertFile sortByCol removeInvarCol summarizeRows dfFactor
#' @importFrom Biobase `annotation<-` assayData `fData<-` fData `pData<-` pData exprs `exprs<-` annotation
#' @importFrom Biobase featureData phenoData featureNames sampleNames  `featureNames<-` storageMode
#' @importFrom graph connComp
#' @importFrom ribiosAnnotation gtiChiptypes bioc2gti
#' @importFrom ribiosIO readMatrix read_gct_matrix write_gct writeMatrix
#' @importFrom ribiosArg parseStrings parseFactor
#' @importClassesFrom Biobase eSet ExpressionSet
NULL

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
#' =\"character\")")}{ When comment is missing, it is set to empty
#' string
#' 
#' }
#' 
#' \item{list("signature(title = \"character\", comment = \"missing\", genes
#' ="\"list\")")}{ When comment is missing, it is set to empty string } }
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

#' An ExpressionSet for case demonstrations
#' 
#' This object is adapted from the \code{sample.ExpressionSet} object, with
#' feature annotations from GTI (Data stand: December 2011). It is used in case
#' studies where functionalities of the \code{ribiosExpression} package are
#' demonstrated.
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



