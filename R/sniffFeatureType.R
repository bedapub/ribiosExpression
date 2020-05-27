#' @include AllGenerics.R ribiosExpression-package.R
NULL

#' Sniff the feature type of an object that implements the featureNames method
#'
#' @param object Any object that \code{featureNames} can be applied to, 
#'   for instance an \code{ExpressionSet}, an \code{EdgeObject}, etc.
#' @param majority A numeric value, used for majority voting, passed to 
#'   \code{guessFeatureType}.
#' @return A character string indicating likely feature type.
#'
#' @seealso{\code{\link[ribiosAnnotation]{guessFeatureType}}}
#' @importFrom ribiosAnnotation guessFeatureType
#' @export
sniffFeatureType <- function(object, majority=0.5) {
  featureIDs <- featureNames(object)
  res <- ribiosAnnotation::guessFeatureType(featureIDs, majority=majority)
  return(res)
}
