#' Data structures and functions for gene expression analysis
#' 
#' @docType package
#' @description Provides data structures and functions for gene expression analysis
#' @name ribiosExpression-package
NULL

#' @importFrom methods as 
#' @importFrom stats cor median prcomp qt sd
#' @importFrom utils write.table
#' @importFrom ribiosUtils haltifnot assertFile sortByCol removeInvarCol 
#'             summarizeRows dfFactor putColsFirst ulen
#' @importFrom Biobase `annotation<-` assayData `fData<-` fData `pData<-` 
#'             featureNames pData exprs `exprs<-` annotation
#'             featureData phenoData sampleNames 
#'             `featureNames<-` storageMode
#' @importFrom ribiosIO readMatrix read_gct_matrix write_gct writeMatrix
#' @importFrom ribiosArg parseStrings parseFactor
#' @importFrom ribiosPlot pcaScores
#' @importClassesFrom Biobase eSet ExpressionSet
#' @export `annotation<-` assayData `fData<-` fData `pData<-` 
#' @export featureNames pData exprs `exprs<-` annotation
#' @export featureData phenoData sampleNames 
#' @export `featureNames<-`
#' @importFrom methods new
#' @importFrom utils read.table
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




