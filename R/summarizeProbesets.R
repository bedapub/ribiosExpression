#' Summarize probesets belonging to the same gene
#' 
#' The \code{summarizeRows} function summarizes (collapses) rows of a numeric
#' matrix by calculating summarizing statistics of rows that belong to the same
#' factor level.
#' 
#' \code{summarizeRows} is called internally by \code{summarizeProbesets} to
#' collapse probesets that belong to one index (e.g. GeneID).
#' 
#' The action of this function is univariate: namely the \code{fun} is applied
#' to all probesets on each sample independently. For example, if \code{fun} is
#' \code{mean}, the average value of mutliple probesets is taken for each
#' sample. With this function, there is no way to distinguish probesets on
#' their expression profiles (for instance: find the probeset with the maximum
#' average signal).
#' 
#' @param eset An \code{ExpressionSet} object
#' @param index.name Charcter, one column name in the \code{eset}, indicating
#' the index of probesets, probably the column holding the GeneID.
#' @param fun Function or character, the function used to summarize probes,
#' \code{mean} by default. Other possibilities include \code{median}.
#' @param keep.nonindex Logical, whether probesets without valid indices should
#' be kept or not.
#' @param keep.featureNames Logical, whether the featureNames of the input
#' object should be kept whenever possible. When multiple probesets are
#' summarized into one value representing, for example, one gene (by GeneID),
#' one arbitrary probeset is used to name the value when this option is set to
#' \code{TRUE}. Otherwise the GeneID will be used as the name of the value. In
#' case no summary was possible, for instance the index is \code{NA}, old
#' feature names are kept any way.
#' @param ... Futher parameters passed to the function
#' @return An \code{ExpressionSet}, with probesets summarized by indices
#' specified.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{summarizeRows}} in the \code{ribiosUtils} package.
#' @examples
#' 
#' data(ribios.ExpressionSet, package="ribiosExpression")
#' ribios.mean <- summarizeProbesets(ribios.ExpressionSet,
#' index.name="GeneID", fun=mean)
#' ribios.mean
#' 
#' data(ribios.ExpressionSet, package="ribiosExpression")
#' ribios.mean.keepFeatureNames <- summarizeProbesets(ribios.ExpressionSet,
#' index.name="GeneID", fun=mean, keep.featureNames=TRUE)
#' ribios.mean
#' 
#' ribios.inval.mean <- summarizeProbesets(ribios.ExpressionSet,
#' index.name="GeneID", fun=mean, keep.nonindex=TRUE)
#' 
#' ## the underlying method
#' ribios.meanMat <- ribiosUtils::summarizeRows(exprs(ribios.ExpressionSet),
#' fData(ribios.ExpressionSet)$GeneID, mean)
#' stopifnot(identical(exprs(ribios.mean), ribios.meanMat))
#' 
#' ## keep old featureNames
#' ribios.inval.mean.old <- summarizeProbesets(ribios.ExpressionSet,
#' index.name="GeneID", fun=mean, keep.nonindex=TRUE, keep.featureNames=TRUE)
#' 
#' @export summarizeProbesets
summarizeProbesets <- function(eset,
                               index.name,
                               fun=mean,
                               keep.nonindex=FALSE,
                               keep.featureNames=FALSE, ...) {
  
  if(missing(index.name) || !index.name %in% colnames(fData(eset))) {
    stop("'index.name' must be a valid column name in fData(", as.character(match.call()$eset), ")")
  }
  fun <- match.fun(fun)
  
  probe.index <- as.character(fData(eset)[,index.name])
  probe.has.index <- !is.na(probe.index) & probe.index != "" 
  eset.indexed <- eset[probe.has.index,]
  
  probe.indexed.fac <- factor(fData(eset.indexed)[,index.name])
  eset.fun <- summarizeRows(exprs(eset.indexed),
                            probe.indexed.fac,
                            fun=fun, ...)

  fun.fd.match <- match(levels(probe.indexed.fac), probe.index)
  eset.remain <- eset[fun.fd.match,]
  
  if(keep.featureNames) {
    newFeatureNames <- rownames(fData(eset.remain))
    ## featureNames(eset.remain) <- rownames(fData(eset.remain))
  } else {
    newFeatureNames <- rownames(eset.fun)
    ## rownames(fData(eset.remain)) <- featureNames(eset.remain)
  }
  rownames(eset.fun) <- featureNames(eset.remain) <- newFeatureNames
  exprs(eset.remain) <- eset.fun
  
  if(keep.nonindex) {
    eset.inval <- eset[!probe.has.index,]
    resExprs <- rbind(exprs(eset.remain),
                      exprs(eset.inval))
    resFdata <- rbind(fData(eset.remain),
                      fData(eset.inval))
    eset.remain <- new("ExpressionSet",
                       exprs=resExprs,
                       featureData=new("AnnotatedDataFrame", resFdata),
                       phenoData=phenoData(eset))
  }
  return(eset.remain)
}
