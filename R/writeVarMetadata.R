#' @title writexlxs: write AnnotatedDataFrame to a xlsx file
#' @param x An \code{ExpressionSet} object or an AnnotatedDataFrame.
#' @param path The xlsx file name to be written to.
#' @param overwrite Logical, whether the file should be overwritten if it exists.
#' @return An invisible \code{TRUE} in case the file is successfully created, else \code{FALSE}.
#' @importFrom openxlsx createWorkbook addWorksheet writeData addStyle createStyle writeData addStyle saveWorkbook
#' @importFrom Biobase varMetadata varLabels
#' @note We also tried the writexl package but the comments are not well supported by writexl, therefore we stay with openxlsx
#' @export
#' @examples
#' data("ribiosExpressionSet", package="ribiosExpression")
#' outfile <- tempfile()
#' writeVarMetadata(ribiosExpressionSet, path=outfile)
#' writeVarMetadata(phenoData(ribiosExpressionSet), path=outfile)
writeVarMetadata <- function(x, path=tempfile(fileext = ".xlsx"), overwrite=TRUE) {
    UseMethod("writeVarMetadata")
}

#' @rdname writeVarMetadata
#' @export
writeVarMetadata.default <- function(x,
                              path=tempfile(fileext = ".xlsx"),
                              overwrite=TRUE) {
  prepend <- sprintf("# %s: %s", varLabels(x), varMetadata(x)$labelDescription)
  df <- pData(x)
  wb <- openxlsx::createWorkbook()
  sheet <- "AnnotatedDataFrame"
  openxlsx::addWorksheet(wb, sheet)
  prependStyle <- openxlsx::createStyle(fgFill="#CCFFCC", fontColour="#003300")
  openxlsx::writeData(wb, sheet, prepend)
  openxlsx::addStyle(wb, sheet=sheet, prependStyle, rows=1:length(prepend), cols=1)
  colheadStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::writeData(wb, sheet, df, startRow=length(prepend)+1)
  openxlsx::addStyle(wb, sheet=sheet, colheadStyle, rows=length(prepend)+1, cols=1:ncol(df))
  res <- openxlsx::saveWorkbook(wb, path, overwrite=overwrite, returnValue=TRUE)
  return(invisible(res))
}
