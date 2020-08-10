#' Read in an annotation file in the tsv-format, with or without row names
#' @param file A tab-delimited file without quotes, the first column must
#'   contain identifiers (key names). In case the first column has no column name 
#'   and it contains row names, they will be used as feature names.
#' @param outputKeyName The key name used in the output \code{data.frame}. It 
#'   does not need to exist in the column names of the input \code{data.frame}. 
#'   In this situation, the first column will be used as output keys. If it does
#'   exist in the column names of the input \code{data.frame}, the content of 
#'   that column must be identical with the first column of the annotation file.
#' @param ... Other parameters passed to \code{\link[ribiosIO]{readMatrix}}, 
#'   which are further passed to \code{\link[utils]{read.table}}.
#' @return A \code{data.frame} containing the annotation, with the first
#'   column named as \code{outputKeyName} that contains feature identifiers as character 
#'   strings. In case the input table contains the column with the same name, 
#'   the content in that column must match the row names, otherwise an error 
#'   is reported.
#'   
#' This function is called by \code{readFeatureAnnotationFile} and
#'  \code{readSampleAnnotationFile}. Normal users are unlikely to use it.
#' @importFrom ribiosUtils putColsFirst haltifnot
#' @examples 
#' f1 <- system.file("extdata",
#'   "featureAnnotation/featureAnnotationFile-withRowNames.txt", 
#'   package="ribiosExpression")
#' f2 <- system.file("extdata",
#'   "featureAnnotation/featureAnnotationFile-withoutRowNames.txt", 
#'   package="ribiosExpression")
#' # 'FeatureName' does not exist in the column names of f1
#' f1Read <- readAnnotationFile(f1, outputKeyName="FeatureName") 
#' # 'GeneID' exists in the colum names of f2, and it is the first column.
#' f2Read <- readAnnotationFile(f2, outputKeyName="GeneID")
#' head(f1Read)
#' head(f2Read)
#' @export
readAnnotationFile <- function(file, outputKeyName="FeatureName", ...) {
  ribiosUtils::assertFile(file)
  annoTbl <- ribiosIO::readTable(file, row.names=FALSE, ...)
  if(outputKeyName %in% colnames(annoTbl)) {
    ribiosUtils::haltifnot(identical(as.character(annoTbl[, outputKeyName]),
                        as.character(annoTbl[,1L])),
                        msg="Error in readAnnotationFile: the content in the column speicified by outputKeyName does not match the first column. It is likely caused by using column names that are reserved for feature (FeatureName) or sample (SampleName) annotation. Please consider renaming such columns. If the problem persists, please contact the developer.")
  }
  if(colnames(annoTbl)[1]=="") { ## the annotation file contains row names
    if(outputKeyName %in% colnames(annoTbl)) {
      annoTbl <- annoTbl[, -1, drop=FALSE]
    } else {
      colnames(annoTbl)[1] <- outputKeyName
    }
  } else {
    if(!outputKeyName %in% colnames(annoTbl)) {
      annoTbl[, outputKeyName] <- annoTbl[,1]
    }
  }
  annoTbl <- ribiosUtils::putColsFirst(annoTbl, outputKeyName)
  annoTbl[, outputKeyName] <- as.character(annoTbl[, outputKeyName])
  rownames(annoTbl) <- as.character(annoTbl[, outputKeyName])
  return(annoTbl)
}

#' Read in feature annotation file in the tsv-format, with or without row names
#' @param file A tab-delimited file without quotes, the first column must
#'   contain feauture identifiers. In case the first column has no column name 
#'   and it contains row names, they will be used as feature names.
#' @param ... Other parameters passed to \code{\link[ribiosIO]{readMatrix}}, 
#'   which are further passed to \code{\link[utils]{read.table}}.
#' @return A \code{data.frame} containing feature annotation, with the first
#'   column named as `FeatureName` that contains feature identifiers as character 
#'   strings. In case the input table contains the column `FeatureName`, the
#'   content in that column must match the row names, otherwise an error 
#'   is reported.
#' @examples 
#' f1 <- system.file("extdata",
#'   "featureAnnotation/featureAnnotationFile-withRowNames.txt", 
#'   package="ribiosExpression")
#' f2 <- system.file("extdata",
#'   "featureAnnotation/featureAnnotationFile-withoutRowNames.txt", 
#'   package="ribiosExpression")
#' f1Read <- readFeatureAnnotationFile(f1)
#' f2Read <- readFeatureAnnotationFile(f2)
#' head(f1Read)
#' head(f2Read)
#' @export
readFeatureAnnotationFile <- function(file, ...) {
  res <- readAnnotationFile(file, outputKeyName="FeatureName", ...)
  return(res)
}

#' Read in sample annotation file in the tsv-format, with or without row names
#' @param file A tab-delimited file without quotes, the first column must
#'   contain sample identifiers. In case the first column has no column name 
#'   and it contains row names, they will be used as sample names.
#' @param ... Other parameters passed to \code{\link[ribiosIO]{readMatrix}}, 
#'   which are further passed to \code{\link[utils]{read.table}}.
#' @return A \code{data.frame} containing sample annotation, with the first
#'   column named as `ExperimentName` that contains sample identifiers as character 
#'   strings. In case the input table contains the column `ExperimentName`, the
#'   content in that column must match the row names, otherwise an error 
#'   is reported.
#' @examples 
#' f1 <- system.file("extdata",
#'   "sampleAnnotation/sampleAnnotationFile-withRowNames.txt", 
#'   package="ribiosExpression")
#' f2 <- system.file("extdata",
#'   "sampleAnnotation/sampleAnnotationFile-withoutRowNames.txt", 
#'   package="ribiosExpression")
#' f1Read <- readSampleAnnotationFile(f1)
#' f2Read <- readSampleAnnotationFile(f2)
#' head(f1Read)
#' head(f2Read)
#' @export
readSampleAnnotationFile <- function(file, ...) {
  res <- readAnnotationFile(file, outputKeyName = "ExperimentName", ...)
}
