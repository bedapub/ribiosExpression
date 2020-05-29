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
#' @importFrom ribiosUtils putColsFirst haltifnot
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
  ribiosUtils::assertFile(file)
  featAnno <- ribiosIO::readTable(file, row.names=FALSE, ...)
  if("FeatureName" %in% colnames(featAnno)) {
    ribiosUtils::haltifnot(identical(as.character(featAnno[,"FeatureName"]),
                        as.character(featAnno[,1L])),
                        msg="The content in the column 'FeatureName' does not match the first column.")
  }
  if(colnames(featAnno)[1]=="") { ## the annotation file contains row names
    if("FeatureName" %in% colnames(featAnno)) {
      featAnno <- featAnno[, -1, drop=FALSE]
    } else {
      colnames(featAnno)[1] <- "FeatureName"
    }
  } else {
    if(!"FeatureName" %in% colnames(featAnno)) {
      featAnno$FeatureName <- featAnno[,1]
    }
  }
  featAnno <- ribiosUtils::putColsFirst(featAnno, "FeatureName")
  featAnno$FeatureName <- as.character(featAnno$FeatureName)
  rownames(featAnno) <- as.character(featAnno[, "FeatureName"])
  colnames(featAnno) <- make.unique(colnames(featAnno))
  return(featAnno)
}
