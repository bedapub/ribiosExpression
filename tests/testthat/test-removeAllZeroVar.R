library(testthat)
library(ribiosExpression)

testthat::context("Test removeAllZeroVar")

myTestDesign <- matrix(c(1,1,1,1, 1,1,0,0,0,0,1,1,0,0,0,0), 
                       byrow=FALSE, nrow=4L, 
                       dimnames=list(sprintf("S%d", 1:4), 
                                     c("Baseline", "Trt1", "Trt2", "Trt3")))
myTestContrast <- matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1), nrow=4L, byrow=FALSE,
                         dimnames=list(colnames(myTestDesign), 
                                       c("Trt1", "Trt2", "Trt3")))

myTestDesignOut <-  matrix(c(1,1,1,1, 1,1,0,0,0,0,1,1), 
                           byrow=FALSE, nrow=4L, 
                           dimnames=list(sprintf("S%d", 1:4), 
                                         c("Baseline", "Trt1", "Trt2")))
attr(myTestDesignOut, "notEstCoefs") <- "Trt3"
myTestContrastOut <-  matrix(c(0,1,0, 0,0,1), nrow=3L, byrow=FALSE,
                             dimnames=list(colnames(myTestDesignOut), 
                                           c("Trt1", "Trt2")))
attr(myTestContrastOut, "notEstContrasts") <- "Trt3"

     
test_that("removeAllZeroVar works for design and contrast matrix pairs", {
  myTestDesignContrastOut <- removeAllZeroVar(myTestDesign, myTestContrast)
  expect_identical(myTestDesignContrastOut,
                   list(design=myTestDesignOut, contrasts=myTestContrastOut))
})

myDescon <- DesignContrast(myTestDesign, myTestContrast)
test_that("removeAllZeroVar works for DesignContrastObject", {

  myDesconOut <- removeAllZeroVar(myDescon)
  expect_identical(designMatrix(myDesconOut),
                   myTestDesignOut)
  expect_identical(contrastMatrix(myDesconOut),
                   myTestContrastOut)
})
