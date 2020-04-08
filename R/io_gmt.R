#' Convert GRP files into GMT-formatted strings
#' 
#' GRP files are used by Connectivity Map on-line tool, which stores the
#' information of a rank-ordered list of probesets. They are simply one-column
#' text files, each line containing one probeset. \code{grpFiles2gmt} convert
#' GRP files into GMT-formatted strings, which can be written in GMT files to
#' be used by GSEA and other tools.
#' 
#' The function \code{grp2gmt}, called by \code{grpFiles2gmt} internally,
#' annotates probesets when \code{chiptype} is supported by GTI, and transform
#' them into the GMT format.
#' 
#' If chiptype is provided, the \code{\link{annotate}} function is called to
#' fetch probeset annotations from the databank.
#' 
#' @aliases grp2gmt grpFiles2gmt
#' @param txt A vector of character strings, each containing one probeset
#' @param chiptype Chip type, normally character representing the microarray
#' chip type. If the option is missing, or is of value \code{NA} or
#' \code{NULL}, no annotation is done.
#' @param name Character, name of the gene set (the first field of the GMT
#' file)
#' @param \dots GRP file names
#' @param n Integer, number of lines to be read; \code{-1} indicates all lines
#' should be read
#' @return A vector of character strings, each containing one line of a GMT
#' file. They can be written to a file with the \code{\link{writeLines}}
#' function.
#' @note It is user's responsibility to check that all GRP files do exist and
#' are readable.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @references See \url{http://www.broadinstitute.org/cmap/index.jsp} for the
#' use of GRP files in the Connectivity Map web tool.
#' @examples
#' 
#' up.file <- system.file("extdata/tags_up.grp", package="ribiosExpression")
#' down.file <- system.file("extdata/tags_down.grp", package="ribiosExpression")
#' grp2gmt(readLines(up.file, n=-1))
#' grpFiles2gmt(c(up.file, down.file), n=3)
#' 
#' \dontrun{
#' grp2gmt(readLines(up.file, n=-1), chiptype="HG_U95AV2")
#' grpFiles2gmt(c(up.file, down.file), n=-1L, chiptype="HG_U95AV2")
#' }
#' 
#' @export grp2gmt
grp2gmt <- function(txt, chiptype, name) {
  if(missing(chiptype) || is.na(chiptype) || is.null(chiptype)) {
    symbols <- txt
  } else {
    symbols <- annotate(object=txt,
                        target=chiptype,
                        check.target=TRUE)$GeneSymbol
  }
  symbols <- symbols[!is.na(symbols)]
  
  if(missing(name))
    name <- "grp"
  
  gmt <- paste(name,
               "grp2gmt",
               paste(symbols, collapse="\t"),
               sep="\t")
  return(gmt)
}

#' @rdname grp2gmt
#' @export
grpFiles2gmt <- function(..., chiptype, n=-1L) {
  files <- unlist(list(...), use.names=FALSE)
  if(missing(chiptype))
    chiptype <- NA

  res <- lapply(files, function(f) {
    grp2gmt(readLines(f, n=n),
            chiptype=chiptype,
            name=gsub("\\..*", "", basename(f)))
  })
  return(unlist(res, use.names=FALSE))
}