#' Read table with Foreign Keys
#' 
#' The concept \code{Foreign Keys} comes from relational databse systems. These
#' keys can be used to cross-reference tables. Say we have two
#' \code{data.frames}, one contains gene annotations and the other contains
#' protein annotations. A column named \code{mRNArefseqID} may be the foreign
#' key that can be used to specify relationships between gene and proteins.
#' 
#' The \code{readFKtable} reads a table from file, and checks if it contains
#' provided foreign keys: either as row.names or in the first column.
#' 
#' 
#' @param file A table file.
#' @param fk Characters, foreign keys.
#' @param strict.order Logical, whether the foreign keys must have the same
#' order as they appear in the file.
#' @param \dots Other parameters passed to the \code{read.table} function.
#' @return A \code{data.frame} if the FK-matching was successful, otherwise the
#' function will print an error message and stop.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' test.file <- tempfile()
#' fk.teams <- c("HSV", "FCB", "BVB")
#' ## FK in row names
#' test.mat <- matrix(rnorm(9), nrow=3, dimnames=list(fk.teams, NULL))
#' write.table(test.mat, test.file)
#' readFKtable(test.file, fk=fk.teams)
#' 
#' ## or: FK can be in the first column
#' test.df <- data.frame(team=fk.teams, pts=c(15,14,15),plc=c("H", "G", "H"))
#' write.table(test.df, test.file)
#' readFKtable(test.file, fk=fk.teams)
#' 
#' ## try strict.order=TRUE
#' test.df <- data.frame(pts=c(15,14,13), plc=c("H", "G", "H"), row.names=rev(fk.teams))
#' write.table(test.df, test.file)
#' readFKtable(test.file, fk=fk.teams, strict.order=FALSE)
#' \dontrun{readFKtable(test.file, fk=fk.teams, strict.order=TRUE)}
#' 
#' @export readFKtable
readFKtable <- function(file, fk,
                        strict.order=FALSE,...) {
  fk <- as.character(fk)
  data <- read.table(file,...)
  c0 <- as.character(rownames(data))
  hasc1 <- ncol(data)>0
  if(hasc1) c1 <- as.character(data[,1L])
  if(strict.order) {
    if(identical(c0, fk)) {
      return(data)
    } else if (hasc1 && identical(c1, fk)) {
      res <- data[,-1L,drop=FALSE]
      if(!anyDuplicated(fk))
        rownames(res) <- fk
      return(res)
    } else {
      stop("The 1st/2nd column of the input file are not stricktly identical with the foreign keys");
    }
  } else {
    if(!hasc1) {
      return(data[match(fk, c0),])
    } else {
      inc0 <- mean(fk %in% c0)
      inc1 <- mean(fk %in% c1)
      if(inc0>inc1 || (inc0==inc1 & inc0>0.5)) {
        return(data[match(fk, c0),])
      } else if (inc0<inc1) {
        res <- data[match(fk, c1),-1,drop=FALSE]
        if(!anyDuplicated(fk))
          rownames(res) <- fk
        return(res)
      } else {
        stop("The 1st/2nd column of the input file seem not to contain foreign keys");
      }
    }
  }
}
