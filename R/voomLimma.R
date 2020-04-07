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
#' 
#' @importFrom limma topTable
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate rename
#' @importFrom ribiosUtils putColsFirst
#' @export
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
