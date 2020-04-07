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
#' @exportMethod contrastSampleIndices
setGeneric("contrastSampleIndices", function(object, contrast) standardGeneric("contrastSampleIndices"))
