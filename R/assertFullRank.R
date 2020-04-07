#' Assert whether a matrix is of full rank numerically
#' @param matrix Numeric matrix
#' @return If not full rank, the function stops. Otherwise, an invisible \code{TRUE} is returned
#' @importFrom Matrix rankMatrix
#' @examples
#' myMat <- matrix(c(1,1,1,0,1,1), ncol=2, byrow=FALSE)
#' assertFullRank(myMat)
assertFullRank <- function(matrix) {
  rank <- Matrix::rankMatrix(matrix)
  nc <- ncol(matrix)
  if(rank!=nc) {
    stop(sprintf("Matrix is not full rank: it has %d columns but the rank is %d",
                 nc, rank))
  } else {
    return(invisible(TRUE))
  }
}

#' Return rank of the matrix and the ranks of resulting matrices when each column is removed
#' @param matrix A numeric matrix
#' @return A \code{data.frame} with \code{n+1} rows, where \code{n} is the column count of the input matrix
#' 
#' @example 
#' myMat <- matrix(c(1,1,1, 0,1,1, 0,0,1, 1,0,0), ncol=4, byrow=FALSE)
#' removeColRank(myMat)
removeColRank <- function(matrix) {
  colind <- 1:ncol(matrix)
  nc <- ncol(matrix)
  full <- c(ncol=nc, rank=Matrix::rankMatrix(matrix))
  res <- t(sapply(colind, function(i){
    obsrank <- Matrix::rankMatrix(matrix[,-i,drop=FALSE])
    return(c(ncol=nc-1, rank=obsrank))
  }))
  res <- rbind(full, res)
  cnames <- colnames(matrix)
  if(is.null(cnames)) {
    cnames <- sprintf("column%d", colind)
  }
  rownames(res) <- c(deparse(substitute(matrix)),
                     paste0(cnames, "_removed"))
  return(res)
}
