library(testthat)
library(ribiosExpression)

fileDir <- system.file("extdata",
                       "featureAnnotation", package="ribiosExpression")
f1 <- file.path(fileDir, "featureAnnotationFile-withRowNames.txt")
f2 <- file.path(fileDir, "featureAnnotationFile-withoutRowNames.txt")
f3 <- file.path(fileDir, "featureAnnotationFile-withRowNamesFeatureName.txt")
f4 <- file.path(fileDir, "featureAnnotationFile-withoutRowNamesFeatureName.txt")
f1Read <- readFeatureAnnotationFile(f1, stringsAsFactors=FALSE)
f2Read <- readFeatureAnnotationFile(f2, stringsAsFactors=FALSE)
f3Read <- readFeatureAnnotationFile(f3, stringsAsFactors=FALSE)
f4Read <- readFeatureAnnotationFile(f4, stringsAsFactors=FALSE)

f1Expected <- data.frame(FeatureName=c("1", "2"),
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
