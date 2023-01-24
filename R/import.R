#' Read an expression matrix into an ExpressionSet object
#' 
#' The function reads in an expression matrix into an ExpressionSet object. The
#' expression matrix should be saved in the file format supported by the
#' \code{\link{read_exprs_matrix}} function: currently supported formats
#' include tab-delimited file and gct files.
#' 
#' The function is a wrapper of the \code{\link{read_exprs_matrix}} function in
#' the \code{ribiosIO} package. The difference is it returns a valid
#' \code{ExpressionSet} object instead of a primitive matrix.
#' 
#' @param x A file containing an expression matrix
#' @return An \code{ExpressionSet} object holding the expression matrix. Both
#' pData and fData are empty except for the feature/sample names recorded in
#' the expression matrix.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{read_exprs_matrix}} in the \code{ribiosIO} package.
#' @examples
#' 
#' idir <- system.file("extdata", package="ribiosExpression")
#' myeset <- readExprsMatrix(file.path(idir, "sample_eset_exprs.txt"))
#' myeset2 <- readExprsMatrix(file.path(idir, "test.gct"))
#' 
#' @importFrom ribiosIO read_exprs_matrix
#' @export readExprsMatrix
readExprsMatrix <- function(x) {
  exp <- read_exprs_matrix(x)
  res <- new("ExpressionSet",
             exprs=exp,
             phenoData=new("AnnotatedDataFrame",
               data.frame(row.names=colnames(exp))),
             featureData=new("AnnotatedDataFrame",
               data.frame(row.names=rownames(exp))))
  return(res)
}
