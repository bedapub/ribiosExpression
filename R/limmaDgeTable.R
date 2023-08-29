limmaDgeTable <- function(marrayLM, contrast = NULL, confint=TRUE) {
  contrastNames <- contrastNames(marrayLM)
  if(missing(contrast) || is.null(contrast)) {
    contrast <- contrastNames
  } else if (is.logical(contrast) || is.numeric(contrast)) {
    contrast <- contrastNames[contrast]
  } else if (!is.null(contrast)) {
    if (length(contrast) == 0) {
      stop("No contrast selected")
    }
    else if (!all(contrast %in% contrastNames)) {
      stop("Following contrasts are not found:", setdiff(contrast, 
                                                         contrastNames))
    }
  }
  resList <- lapply(contrast, function(con) {
    res <- topTable(marrayLM, coef=con, number=nrow(marrayLM),
                    sort.by="none",
                    confint = confint)
    res$Contrast <- con
    return(res)
  })
  res <- do.call(rbind, resList)
  res <- ribiosUtils::putColsFirst(res, "Contrast")
  rownames(res) <- NULL
  return(res)
}

