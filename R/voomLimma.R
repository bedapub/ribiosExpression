#' Perform the voom+limma procedure
#' @param dgeList A DGEList object, it should be ideally already filtered 
#' @param design The design matrix
#' @param contrasts The contrast matrix
#' @param normalize.method Character string, passed to \code{voom}, keep it \code{none} unless you are sure
#' @param block Blocking factor, passed to \code{voom}
#' @param correlation Correlation between duplicates, passed to \code{voom}
#' @param weights Weights, passed to \code{voom}
#' @param plot Logical, whether the variance-mean relationship should be ploted
#' @param ... Passed to \code{\link[limma]{eBayes}}
#' 
#' @return \code{MArrayLM} object returned by \code{\link[limma]{eBayes}}, with voom object in the \code{voom} element of the list
#' 
#' @example 
#' y <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' d <- edgeR::DGEList(counts=y, group=rep(1:2,each=2))
#' d <- edgeR::calcNormFactors(d)
#' design <- model.matrix(~gl(2,2))
#' colnames(design) <- c("baseline", "treatment")
#' contrasts <- limma::makeContrasts("treatment", levels=design)
#' dvl <- voomLimma(d, design=design, contrasts=contrasts)
voomLimma <- function(dgeList, design, contrasts,
                      normalize.method="none",
                      block=NULL, correlation=NULL, weights=NULL, plot=FALSE, ...) {
  dgeList <- edgeR::calcNormFactors(dgeList)
  voomObj <- limma::voom(dgeList, design=design,
                  normalize.method=normalize.method,
                  block=block, correlation=correlation, weights=weights, plot=plot)
  fit <- limma::lmFit(voomObj, design=design,
               block=block,
               correlation=correlation)
  fit2 <- limma::contrasts.fit(fit, contrasts=contrasts)
  fit2 <- limma::eBayes(fit2, ...)
  fit2$voom <- voomObj
  return(fit2)
}

#' Return a list of dgeTable from limma fit objects
#' 
#' @param limmaFit A \code{MArrayLM} object returned by \code{\link[limma]{eBayes}}.
#' 
#' This function will be merged with \code{dgeTables}
#' 
#' @examples 
#' set.seed(2016)
#' sigma2 <- 0.05 / rchisq(100, df=10) * 10
#' y <- matrix(rnorm(100*6,sd=sqrt(sigma2)),100,6)
#' design <- cbind(Intercept=1,Group=c(0,0,0,1,1,1))
#' y[1,4:6] <- y[1,4:6] + 1
#' fit <- limma::lmFit(y,design)
#' contrasts <- limma::makeContrasts("Group", levels=design)
#' fit <- limma::contrasts.fit(fit, contrasts)
#' fit <- limma::eBayes(fit)
#' limmaDgeTables(fit)
limmaDgeTables <- function(limmaFit) {
  contrs <- contrastNames(limmaFit)
  res <- lapply(contrs, function(ctr) {
    res <- limma::topTable(limmaFit, coef=ctr, number=nrow(limmaFit)) %>%
      dplyr::mutate(Contrast=ctr) %>%
      dplyr::rename(PValue=P.Value, FDR=adj.P.Val) %>%
      ribiosUtils::putColsFirst("Contrast")
    return(res)
  })
  names(res) <- contrs
  return(res)
}
