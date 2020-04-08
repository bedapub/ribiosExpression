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




