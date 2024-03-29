#' Infer groups from a design matrix
#' @param designMatrix A design matrix
#' @return A factor vector giving the groups inferred from the design matrix
#'
#' A naive logic is used: samples of the same design vectors are of the same group.
#'
#' The inference is known to fail when control variables, such as age or RIN numbers,
#' vary between samples of the same group.
#'
#' @importFrom stats model.matrix
#' @examples
#' myDesign <- model.matrix(~gl(3,3))
#' design2group(myDesign)
#' @export
design2group <- function(designMatrix) {
  clevels <- apply(designMatrix, 2, ribiosUtils::ulen)
  useCol <- clevels < nrow(designMatrix)
  groups <- apply(designMatrix[,useCol, drop=FALSE],
                  1, paste, collapse="")
  res <- factor(groups)
  levels(res) <- sprintf("AutoGroup_%02d", 1:nlevels(res))
  return(res)
}

#' Contrast a DesignContrast object
#'
#' @param designMatrix A design matrix
#' @param contrastMatrix A contrast matrix. If null, no comparison can be done.
#' @param groups A factor vector of the same length as the number of columns of the design matrix.
#'   If missing, \code{design2group} is used to infer groups.
#' @param dispLevels A character vector of the same length as the number of levels encoded by \code{group},
#'   indicating how different groups should be labelled. If missing, levels of \code{group} are used.
#' @param contrastAnnotation A data.frame or NULL, annotating contrasts
#'
#' @importFrom limma makeContrasts
#' @return A DesignContrast object
#' @examples
#' myFac <- gl(3,3, labels=c("baseline", "treat1", "treat2"))
#' myDesign <- model.matrix(~myFac)
#' colnames(myDesign) <- c("baseline", "treat1", "treat2")
#' myContrast <- limma::makeContrasts(contrasts=c("treat1", "treat2"), levels=myDesign)
#' DesignContrast(myDesign, myContrast, groups=myFac)
#' DesignContrast(myDesign, myContrast, groups=myFac, dispLevels=c("C", "T1", "T2"))
#' @export
DesignContrast <- function(designMatrix,
                           contrastMatrix=NULL,
                           groups=NULL,
                           dispLevels=NULL,
                           contrastAnnotation=NULL) {
  if(is.null(groups))
    groups <- design2group(designMatrix)
  if(is.null(dispLevels))
    dispLevels <- levels(groups)
  if(is.null(contrastMatrix))
    contrastMatrix <- matrix(nrow=ncol(designMatrix), dimnames=list(colnames(designMatrix), NULL))
  if(is.null(contrastAnnotation)) {
    contrastNames <- colnames(contrastMatrix)
    if(is.null(contrastNames))  contrastNames <- 1:ncol(contrastMatrix)
    contrastAnnotation <- data.frame(row.names=contrastNames)
  }
  res <- new("DesignContrast",
             design=designMatrix,
             contrasts=contrastMatrix,
             groups=groups,
             dispLevels=dispLevels,
             contrastAnnotation=contrastAnnotation)
  return(res)
}

#' Parse contrast from strings
#' @param contrastStr A vector of character strings
#' @return A contrast matrix
#' @export
parseContrastStr <- function(contrastStr) {
  contrasts <- parseStrings(contrastStr)
  csplit <- strsplit(contrasts, "=")
  hasEqual <- grepl("=", contrasts)
  if(any(hasEqual)) {
    names <- sapply(strsplit(contrasts, "="), "[[", 1L)
    contrasts <- gsub("^.*=", "", contrasts)
    names(contrasts) <- names
  } else {
    names(contrasts) <- contrasts
  }
  return(contrasts)
}

#' Parse design and contrast from strings
#' @param groupsStr A factor vector indicating sample groups
#' @param levelStr Level strings
#' @param dispLevelStr Display level strings
#' @param contrastStr A vector of character strings indicating contrasts
#' @return A DesignContrast object
#' @export
parseDesignContrastStr <- function(groupsStr, levelStr, dispLevelStr, contrastStr) {
  groups <- parseFactor(groupsStr, rlevels=levelStr, make.names=TRUE)
  levels <- levels(groups)
  contrast.vec <- parseContrastStr(contrastStr)
  design <- model.matrix(~0+groups)
  colnames(design) <- levels
  contrasts <- makeContrasts(contrasts=contrast.vec, levels=levels)
  colnames(contrasts) <- names(contrast.vec)
  dispLevels <- parseStrings(dispLevelStr)
  if(is.null(dispLevels)) {
    dispLevels <- levels(groups)
  }
  res <- DesignContrast(designMatrix=design,
                        contrastMatrix=contrasts,
                        groups=groups,
                        dispLevels=dispLevels)
  return(res)
}

#' Parse design and contrast from files
#' @param designFile A tab-delimited file encoding the design matrix
#' @param contrastFile A tab-delimited file encoding the contrast matrix
#' @param groupsStr A vector of character strings, giving sample groups
#' @param levelStr A vector of level strings
#' @param dispLevelStr A vector of strings to be used as display labels, if exist
#' @return A DesignContrast object
#' @export
parseDesignContrastFile <- function(designFile, contrastFile,
                                    groupsStr=NULL, levelStr=NULL,
                                    dispLevelStr) {
  assertFile(designFile)
  assertFile(contrastFile)
  if(!is.null(groupsStr)) {
    groups <- parseFactor(groupsStr, rlevels=levelStr, make.names=FALSE)
    levels <- levels(groups)
    notvalid <- (levels != make.names(levels))
    if (any(notvalid))
      stop("The levels must by syntactically valid names in R, see help(make.names).  Non-valid names: ",
           paste(levels[notvalid], collapse = ","))
  } else {
    groups <- NULL
  }
  design <- readMatrix(designFile)
  contrast <- readMatrix(contrastFile)
  dispLevels <- parseStrings(dispLevelStr)
  if(is.null(dispLevels)) {
    dispLevels <- levels(groups)
  }
  res <- DesignContrast(designMatrix=design,
                        contrastMatrix=contrast,
                        groups=groups,
                        dispLevels=dispLevels)
  return(res)
}

plainFile2ConcString <- function(str) {
    if(!is.null(str) && file.exists(str)) {
        str <- paste(readLines(str), collapse=",")
    }
    return(str)
}

#' Build a data.frame from two vectors of potential different lengths
#' @param vec1 A vector
#' @param vec2 Another vector
#' @param col.names A character vector of length 2 giving column names of the output \code{data.frame}.
#'
#' The shorter vector of the two are extended to the same length by appending empty strings.
#' @return A \code{data.frame} of two columns. The row count matches the longer vector
#'
#' @examples
#' dataFrameTwoVecs(LETTERS[1:5], letters[2:9])
#' @export
dataFrameTwoVecs <- function(vec1, vec2, col.names=c("Vec1", "Vec2")) {
  len <- pmax(length(vec1), length(vec2))
  res <- data.frame(vec1 = c(as.character(vec1), rep("", len-length(vec1))),
		    vec2 = c(as.character(vec2), rep("", len-length(vec2))))
  colnames(res) <- col.names
  return(res)
}

#' Test whether the input design matrix is consistent with the sample names
#' @param descon A DesignContrast object
#' @param sampleNames A vector of string characters, specifying sample names
#'
#' If the sample names in DesignContrast are identical with the given sample names,
#' an invisible \code{TRUE} is returned.
#'
#' If the two sets are identical, however the order of sample names do not match, a warning
#' message is raised, and an invisible \code{FALSE} is returned
#'
#' If the two sets have differences, the mismatching sample names are printed for diagnosis.
#'
#' @return A invisible logical value. \code{TRUE} if and only if the sample names match perfectly.
isInputDesignConsistent <- function(descon, sampleNames) {
    designSampleNames <- rownames(designMatrix(descon))
    if(setequal(sampleNames, designSampleNames)) {
        if(identical(sampleNames, designSampleNames)) {
            return(invisible(TRUE))
        } else {
            warning("The order of samples in the design matrix differ from that in the input matrix! Please verify the consistency!")
            return(invisible(FALSE))
        }
    } else {
        warning("Sample names in the input matrix and in the design matrix do not match")
	print(dataFrameTwoVecs(sampleNames, designSampleNames,
			col.names=c("Given","DesignMatrix")))
        return(invisible(FALSE))
    }
}

#' Parse study design and asked questions encoded in design and contrast matrices or in one-way ANOVA designs
#' @param designFile A plain tab-delimited file with headers encoding the design matrix, or NULL
#' @param contrastFile A plain tab-delimited file with headers encoding the contrast matrix, or NULL
#' @param sampleGroups A character string concatenated by commas (e.g. A,B,C), or a plain text file containing one string per line (e.g. A\emph{newline}B\emph{newline}C), encoding sample group memberships.
#' @param groupLevels Similar format as 'sampleGroups', encoding levels (e.g. order) of the sampleGroups
#' @param dispLevels Similar format as 'sampleGroups', encoding the display of the groupLevels. Must match 'groupLevels'
#' @param contrasts Similar format as 'sampleGroups', encoding contrasts in case of one-way ANOVA designs
#' @param expSampleNames A vector of character strings giving the expected sample names (e.g. those in the input matrix)
#' @return A S4-object 'DesignContrast'
#' @examples
#' ## one-way ANOVA
#' parseDesignContrast(sampleGroups="As,Be,As,Be,As,Be",groupLevels="Be,As",
#'     dispLevels="Beryllium,Arsenic", contrasts="As-Be")
#' ## design/contrast matrix
#' designFile <- system.file("extdata/example-designMatrix.txt",
#'     package="ribiosExpression")
#' contrastFile <- system.file("extdata/example-contrastMatrix.txt",
#'     package="ribiosExpression")
#' # minimal information
#' parseDesignContrast(designFile=designFile, contrastFile=contrastFile)
#' # with extra information about sample groups
#' parseDesignContrast(designFile=designFile, contrastFile=contrastFile,
#'     sampleGroups="As,Be,As,Be,As,Be",
#'     groupLevels="Be,As", dispLevels="Beryllium,Arsenic")
#' @export
parseDesignContrast <- function(designFile=NULL, contrastFile=NULL,
                                sampleGroups=NULL, groupLevels=NULL, dispLevels=NULL,
                                contrasts=NULL, expSampleNames=NULL) {
  ## sampleGroups, groupLevels, dispLevels, and contrasts can be either a character string concatenated by commas, or a plain file that encode the strings (one per line)
    sampleGroups <- plainFile2ConcString(sampleGroups)
    groupLevels <- plainFile2ConcString(groupLevels)
    dispLevels <- plainFile2ConcString(dispLevels)
    contrasts <- plainFile2ConcString(contrasts)

    if(!is.null(designFile) & !is.null(contrastFile)) {
        descon <- parseDesignContrastFile(designFile=designFile,
                                       contrastFile=contrastFile,
                                       groupsStr=sampleGroups,
                                       levelStr=groupLevels,
                                       dispLevelStr=dispLevels)
    } else if (!is.null(sampleGroups) & !is.null(contrasts)) {
        descon <- parseDesignContrastStr(groupsStr=sampleGroups,
                                      levelStr=groupLevels,
                                      dispLevelStr=dispLevels,
                                      contrastStr=contrasts)
    } else {
        stop("Provide either a design matrix and a contrast matrix, or sample groups and contrasts")
    }

    if(!is.null(expSampleNames)) {
        isInputDesignConsistent(descon, expSampleNames)
    }
    return(descon)
}

.contrastSampleIndices<- function(descon, contrast) {
    contrastMat <- contrastMatrix(descon)
    designMat <- designMatrix(descon)
    haltifnot(contrast %in% colnames(contrastMat) || contrast %in% 1:ncol(contrastMat),
              msg=sprintf("contrast '%s' not found in the contrast matrix",
                  contrast))
    currContrast <- contrastMat[, contrast]
    isNonZero <- currContrast!=0
    ascNonZero <- which(isNonZero)[order(currContrast[isNonZero], decreasing=FALSE)]
    ascInds <- lapply(ascNonZero, function(designInd)
        which(designMat[,designInd]!=0))
    ## if there is intercept AND the sum of current contrast is not zero, add samples only having intercept
    isInter <- apply(designMat, 2L, function(x) all(x==1))
    if(any(isInter) & sum(currContrast)!=0) {
        hasInterOnly <- which(apply(designMat, 1L, function(x) all(x[!isInter]==0)))
        ascInds <- c(list(intersect=hasInterOnly), ascInds)
    }
    res <- unname(unlist(ascInds))
    return(res)
}

#' @include AllGenerics.R
#' @describeIn contrastSampleIndices Use character string to specify the contrast
#' @export
setMethod("contrastSampleIndices", c("DesignContrast", "character"), function(object, contrast) {
              .contrastSampleIndices(object, contrast)
          })

#' @describeIn contrastSampleIndices Use integer indices to specify the contrast
#' @export
setMethod("contrastSampleIndices", c("DesignContrast", "numeric"), function(object, contrast) {
              .contrastSampleIndices(object, contrast)
          })

#' Plot a DesignContrast object with two heatmaps
#' 
#' @param x A \code{DesignContrast} object
#' @param y NULL, ignored
#' @param title Title of the object, used in the title of the heatmaps
#' @param clusterDesign Logical, cluster rows of the design matrix
#' @param clusterSamples Logical, cluster columns of the design matrix
#' @param clusterContrasts Logical, cluster rows of the contrast matrix (notice that the contrast matrix required by limma is the transposed contrast matrix)
#' @param ... Other parameters passed to \code{\link[ComplexHeatmap]{Heatmap}}
#'
#' @description
#' The function plots a ComplexHeatmap containing two matrices, one of the design matrix and the other of the contrast matrix.
#' @return Heatmap object
#' @examples
#' myFac <- gl(3,3, labels=c("baseline", "treat1", "treat2"))
#' myDesign <- model.matrix(~myFac)
#' colnames(myDesign) <- c("baseline", "treat1", "treat2")
#' myContrast <- limma::makeContrasts(contrasts=c("treat1", "treat2"), levels=myDesign)
#' res1 <- DesignContrast(myDesign, myContrast, groups=myFac)
#' res2 <- DesignContrast(myDesign, myContrast, groups=myFac, dispLevels=c("C", "T1", "T2"))
#' plot(res1, title="DesCon 1")
#' plot(res2, title="DesCon 1 (identical)")
#' @importFrom ribiosPlot royalbluered
#' @importFrom ComplexHeatmap Heatmap `%v%`
#' @importFrom circlize colorRamp2
#' @export
plot.DesignContrast <- function(x, y=NULL,
                                title=NULL, 
                                clusterDesign = FALSE,
                                clusterSamples = FALSE,
                                clusterContrasts = FALSE,
                                ...) {
  
  if(is.null(title)) {
    title <- c("Design and contrast")
  }
  
  desMat <- designMatrix(x)
  contMat <- contrastMatrix(x)
  
  contMatAbsMax <- max(abs(contMat), na.rm=TRUE)
  contMatSymRange <- c(-contMatAbsMax, 0, contMatAbsMax)
  
  ht1 <- ComplexHeatmap::Heatmap(desMat, name="Design",
                                 col = circlize::colorRamp2(range(desMat, na.rm=TRUE),
                                                            c("black", "yellow")),
                                 cluster_rows = clusterDesign,
                                 cluster_columns = clusterSamples,
                                 border="black",
                                 column_title = title,
                                 row_title = "Design matrix",
                                 ...)
  ht2 <- ComplexHeatmap::Heatmap(t(contMat), name="Contrast",
                                 col = circlize::colorRamp2(contMatSymRange,
                                                            ribiosPlot::royalbluered(3)),
                                 cluster_rows = clusterDesign,
                                 cluster_columns = clusterContrasts,
                                 border="black",
                                 row_title = "Contrasts",
                                 ...)
  
  res <- ht1 %v% ht2
  return(res)
}
