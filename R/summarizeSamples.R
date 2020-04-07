#' Summarize samples by applying the function to sample subsets
#' 
#' The function takes an \code{eSet} object and a factor of the same length as
#' the object, and summarizes samples of the same factor level by applying the
#' function.
#' 
#' \code{poolReplicates} and \code{avgReplicates} are two specific form of the
#' more generic \code{summarizeSamples} function: they take sum and average of
#' replicates given by the factor, respectively.
#' 
#' @aliases summarizeSamples poolReplicates avgReplicates
#' @param eset An \code{eSet} object.
#' @param indSamples A factor of the same length as the sample number of the
#' object
#' @param removeInvarCols Logical, whether invariant columns of the resulting
#' \code{eSet} pheno data should be removed or not. This is useful in case
#' there are technical replicates.
#' @param fun The function to be applied to summarize samples
#' @param \dots Other parameters passed to the function
#' @return A \code{eSet} object.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso The function calls \code{\link[ribiosUtils]{summarizeColumns}}
#' internally.
#' 
#' Also see \code{\link{summarizeProbesets}}.
#' @examples
#' 
#' data(ribios.ExpressionSet, package="ribiosExpression")
#' index <- factor(c(gl(12,2), 13, 14))
#' 
#' (ss.eset1 <- summarizeSamples(ribios.ExpressionSet, index))
#' 
#' (ss.eset2 <- summarizeSamples(ribios.ExpressionSet, index, fun=mean,
#' na.rm=TRUE))
#' ## equivalently 
#' (ss.eset2 <- poolReplicates(ribios.ExpressionSet, index))
#' 
#' (ss.eset3 <- avgReplicates(ribios.ExpressionSet, index))
#' 
#' @export summarizeSamples
summarizeSamples <- function(eset, indSamples=eset$SAMPLEID, removeInvarCols=TRUE, fun=sum, ...) {
  if(!is.factor(indSamples)) indSamples <- as.factor(indSamples)
  stopifnot(length(indSamples)==ncol(eset))
  if(is.factor(indSamples))  indSamples <- droplevels(indSamples)
  eset.pool <- ribiosUtils::summarizeColumns(exprs(eset), indSamples, fun=fun, ...)
  eset.pd <- do.call(rbind, tapply(1:ncol(eset), indSamples, function(x) {
    pData(eset)[x[1],]
  }))
  if(removeInvarCols) eset.pd <- removeInvarCol(eset.pd)
  rownames(eset.pd) <- colnames(eset.pool)
  eset.poolEset <- new("ExpressionSet",
                       exprs=eset.pool,
                       phenoData=new("AnnotatedDataFrame", eset.pd),
                       featureData=featureData(eset))
  return(eset.poolEset)
}


#' @rdname summarizeSamples
#' @export
poolReplicates <- function(eset, indSamples=eset$SAMPLEID, removeInvarCols=TRUE) {
  summarizeSamples(eset, indSamples=indSamples, removeInvarCols=TRUE, fun=sum, na.rm=TRUE)
}

#' @rdname summarizeSamples
#' @export
avgReplicates <- function(eset, indSamples=eset$SAMPLEID, removeInvarCols=TRUE) {
  summarizeSamples(eset, indSamples=indSamples, removeInvarCols=TRUE, fun=mean, na.rm=TRUE)
}
