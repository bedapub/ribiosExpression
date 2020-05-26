#' Read in feature annotation file in the tsv-format, with or without row names
#' @param file A tab-delimited file without quotes, the first column must
#'   contain feauture identifiers. In case the first column contains column 
#'   names, in which case the first column has no name, the row names will be 
#'   used as feature identifiers.
#' @return A \code{data.frame} containing feature annotation, with the first
#'   column named as `FeatureID` that contains feature identifiers. In case the 
#'   input table contains the column `FeatureID`, it will be renamed as
#'    `FeatureID.1`.
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
readFeatureAnnotationFile <- function(file) {
  ribiosUtils::assertFile(file)
  featAnno <- ribiosIO::readTable(file, row.names=FALSE)
  if(colnames(featAnno)[1]=="") { ## the annotation file contains row names
    colnames(featAnno)[1] <- "FeatureID"
  } else {
    featAnno$FeatureID <- as.character(featAnno[,1])
    featAnno <- putColsFirst(featAnno, "FeatureID")
  }
  colnames(featAnno) <- make.unique(colnames(featAnno))
  return(featAnno)
}
