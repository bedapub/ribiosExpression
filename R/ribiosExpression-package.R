#' Data structures and functions for gene expression analysis
#' 
#' @docType package
#' @description Provides data structures and functions for gene expression analysis
#' @name ribiosExpression-package
NULL

#' @importFrom methods as 
#' @importFrom stats cor median prcomp qt sd
#' @importFrom utils write.table
#' @importFrom ribiosUtils haltifnot assertFile sortByCol removeInvarCol summarizeRows dfFactor putColsFirst
#' @importFrom Biobase `annotation<-` assayData `fData<-` fData `pData<-` pData exprs `exprs<-` annotation
#' @importFrom Biobase featureData phenoData featureNames sampleNames  `featureNames<-` storageMode
#' @importFrom ribiosAnnotation gtiChiptypes bioc2gti
#' @importFrom ribiosIO readMatrix read_gct_matrix write_gct writeMatrix
#' @importFrom ribiosArg parseStrings parseFactor
#' @importClassesFrom Biobase eSet ExpressionSet
NULL

#' Make GMT format strings with titles, comments and genes
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

#' Interface to the function kendallWmat
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
NULL




