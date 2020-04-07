#' Merge two eSets by column binding
#' @param eset1 An \code{eSet} object
#' @param eset2 Another \code{eSet} object
#' @param by.x Column index of feature annotation of \code{eset1}
#' @param by.y COlumn index of feature annotation of \code{eset2}
#' @param normalization \code{NULL} or character string, which will be passed to normalizeBetweenArrays.
#' 
#' @return A new \code{eSet} object
#' @importFrom limma normalizeBetweenArrays
#' @importFrom ribiosUtils matchColumnIndex
#' 
#' @export
mergeEset <- function(eset1, eset2, by.x, by.y,
                      normalization="quantile") {
  stopifnot(ncol(pData(eset1)) == ncol(pData(eset2)) && all(colnames(pData(eset1)) == colnames(pData(eset2))))
  
  f1 <- fData(eset1)
  f2 <- fData(eset2)
  
  stopifnot(by.x %in% colnames(f1))
  stopifnot(by.y %in% colnames(f2))
  
  f1.id <- f1[, by.x]
  f2.id <- f2[, by.y]
    
  common.feats <- intersect(f1.id, f2.id) 
  ind1 <- matchColumnIndex(common.feats, f1, by.x)
  ind2 <- matchColumnIndex(common.feats, f2, by.y)
    
  np <- rbind(pData(eset1), pData(eset2))
  nf <- featureData(eset1)[ind1,]
  ne <- cbind(exprs(eset1)[ind1,],
              exprs(eset2)[ind2,])
  if(!is.null(normalization)) {
    ne <- limma::normalizeBetweenArrays(ne, method=normalization)
  } else {
    warning("[Attention] The exprs matrix is not normalized between arrays\n")
  }
  colnames(ne) <- rownames(np)
  me <- new("ExpressionSet",
            exprs=ne,
            featureData=nf,
            phenoData=new("AnnotatedDataFrame", np))
    
  return(me)
}
