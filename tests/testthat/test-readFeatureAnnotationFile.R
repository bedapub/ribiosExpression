library(testthat)
library(ribiosExpression)

f1 <- system.file("extdata",
  "featureAnnotation/featureAnnotationFile-withRowNames.txt", 
  package="ribiosExpression")

f2 <- system.file("extdata",
  "featureAnnotation/featureAnnotationFile-withoutRowNames.txt", 
  package="ribiosExpression")

fileDir <- system.file("extdata",
                       "featureAnnotation", package="ribiosExpression")
f1 <- file.path(fileDir, "featureAnnotationFile-withRowNames.txt")
f2 <- file.path(fileDir, "featureAnnotationFile-withoutRowNames.txt")
f3 <- file.path(fileDir, "featureAnnotationFile-withRowNamesFeatureID.txt")
f4 <- file.path(fileDir, "featureAnnotationFile-withoutRowNamesFeatureID.txt")
f1Read <- readFeatureAnnotationFile(f1, stringsAsFactors=FALSE)
f2Read <- readFeatureAnnotationFile(f2, stringsAsFactors=FALSE)
f3Read <- readFeatureAnnotationFile(f3, stringsAsFactors=FALSE)
f4Read <- readFeatureAnnotationFile(f4, stringsAsFactors=FALSE)

f1Expected <- data.frame(FeatureID=c("1", "2"),
                         GeneID=c(1, 2),
                         GeneSymbol=c("A1BG", "A2M"),
                         row.names=c("1", "2"),
                         stringsAsFactors = FALSE)
f2Expected <- f3Expected <- f4Expected <- f1Expected

test_that("readFeatureAnnotationFile works as expected",
          {
            expect_equal(f1Read, f1Expected)
            expect_equal(f2Read, f2Expected)
            expect_equal(f3Read, f3Expected)
            expect_equal(f4Read, f4Expected)
          })
