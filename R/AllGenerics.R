#' @exportMethod exprsToLong
setGeneric("exprsToLong", function(x,...) standardGeneric("exprsToLong"))
#' @exportMethod contrastNames
setGeneric("contrastNames", function(object) standardGeneric("contrastNames"))
#' @exportMethod designVariables
setGeneric("designVariables", function(object) standardGeneric("designVariables"))

#' @exportMethod groups
setGeneric("groups", function(object) standardGeneric("groups"))
#' @exportMethod dispGroups
setGeneric("dispGroups", function(object) standardGeneric("dispGroups"))
#' @exportMethod designMatrix
setGeneric("designMatrix", function(object) standardGeneric("designMatrix"))
#' @exportMethod contrastMatrix
setGeneric("contrastMatrix", function(object) standardGeneric("contrastMatrix"))
#' @exportMethod nContrast
setGeneric("nContrast", function(object) standardGeneric("nContrast"))


#' Return indices of samples involved in the given contrast of two or more coefficients
#'
#' @title 
#' @param object: A \code{DesignContrast} object
#' @param contrast: Either a contrast name or a integer indicating the index of the contrast
#' @return An integer vector, indices of samples that are involved, sorted by the ascending order of the coefficients of the contrast
#' 
#' @aliases contrastSampleIndices,DesignContrast,numeric-method contrastSampleIndices,DesignContrast,character-method
#' @examples
#' ## one-way ANOVA
#' myDesCon <- parseDesignContrast(sampleGroups="As,Be,As,Be,As,Be",
#'    groupLevels="Be,As", dispLevels="Beryllium,Arsenic", contrasts="As-Be")
#' contrastSampleIndices(myDesCon, 1L)
#' myInterDesCon <- new("DesignContrast", 
#'     design=matrix(c(rep(1,6), rep(0,2), rep(1,2), rep(0,2), 
#'            rep(0,4), rep(1,2)), nrow=6, byrow=FALSE), 
#'     contrasts=matrix(c(0,1,0, 0,0,1, 0,-1,1), byrow=FALSE, nrow=3), 
#'     groups=factor(rep(c("As", "Be", "Cd"), each=2)), 
#'     dispLevels=c("Arsenic", "Beryllium", "Cadmium"))
#' cont1Ind <- contrastSampleIndices(myInterDesCon, 1L)
#' cont2Ind <- contrastSampleIndices(myInterDesCon, 2L)
#' cont3Ind <- contrastSampleIndices(myInterDesCon, 3L)
#' stopifnot(identical(cont1Ind, 1:4))
#' stopifnot(identical(cont2Ind, c(1:2, 5:6)))
#' stopifnot(identical(cont3Ind, c(3:6)))
#' @exportMethod contrastSampleIndices
setGeneric("contrastSampleIndices", function(object, contrast) standardGeneric("contrastSampleIndices"))
