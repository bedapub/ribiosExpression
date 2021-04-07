library(testthat)
library(ribiosExpression)

fileDir <- system.file("extdata",
                       "sampleAnnotation", package="ribiosExpression")

f1 <- file.path(fileDir, "sampleAnnotationFile-withRowNames.txt")
f2 <- file.path(fileDir, "sampleAnnotationFile-withoutRowNames.txt")
f3 <- file.path(fileDir, "sampleAnnotationFile-withRowNamesSampleName.txt")
f4 <- file.path(fileDir, "sampleAnnotationFile-withoutRowNamesSampleName.txt")
f1Read <- readSampleAnnotationFile(f1, stringsAsFactors=FALSE)
f2Read <- readSampleAnnotationFile(f2, stringsAsFactors=FALSE)
f3Read <- readSampleAnnotationFile(f3, stringsAsFactors=FALSE)
f4Read <- readSampleAnnotationFile(f4, stringsAsFactors=FALSE)

f1Expected <- data.frame(ExperimentName=c("S1", "S2", "S3", "S4"),
                         SampleID=c("S1", "S2", "S3", "S4"),
                         group=c("ctrl", "trt", "ctrl", "trt"),
                         row.names=c("S1", "S2", "S3", "S4"),
                         stringsAsFactors = FALSE)
f2Expected <- f1Expected
f3Expected <- f4Expected <- cbind(f1Expected, SampleName=rownames(f1Expected))

test_that("readSampleAnnotationFile works as expected",
          {
            expect_equal(f1Read, f1Expected)
            expect_equal(f2Read, f2Expected)
            expect_equal(f3Read, f3Expected)
            expect_equal(f4Read, f4Expected)
          })
