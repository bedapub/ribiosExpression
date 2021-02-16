library(ribiosExpression)
library(testthat)

context("Test fixDesignMatrixColnames")

myFac1 <- gl(6,2, labels=sprintf("Fac1_%d", 1:6))
myFac2 <- gl(2,6, labels=c("Ctrl", "Dis"))
myVar <- rnorm(12)
myDesignAdd <- model.matrix(~myFac1 + myFac2 + myVar)
myDesignAddFixed <- fixDesignMatrixColnames(myDesignAdd)
myDesignInt <- model.matrix(~myFac1 * myFac2 + myVar)
myDesignIntFixed <- fixDesignMatrixColnames(myDesignInt)
myDesignInteraction <- model.matrix(~interaction(myFac1, myFac2, sep="_")+myVar)
myDesignIntactionFixed <- fixDesignMatrixColnames(myDesignInteraction)
  
myDesignIntFixedPoint <- fixDesignMatrixColnames(myDesignInt,
                                                 interceptChar = ".")
myDesignAddFixedKeepContrastName <- fixDesignMatrixColnames(myDesignAdd,
                                                            removeContrastNames = FALSE)

expectedAddColnames <- c("Baseline", "Fac1_2", "Fac1_3", "Fac1_4", 
                         "Fac1_5", "Fac1_6", "Dis", "myVar")
expectedIntColnames <- c("Baseline", "Fac1_2", "Fac1_3", "Fac1_4", 
                         "Fac1_5", "Fac1_6", "Dis", "myVar",
                         "Fac1_2_Dis", "Fac1_3_Dis", "Fac1_4_Dis", "Fac1_5_Dis", "Fac1_6_Dis")
expectedInteractionColnames <- c("Baseline",
                                 c(as.vector(t(outer(levels(myFac1), levels(myFac2), paste, sep="_")))[-1],
                                 "myVar"))

test_that("fixDesignMatrixColnames works for additive models", {
  expect_equivalent(myDesignAdd, myDesignAddFixed)
  expect_identical(colnames(myDesignAddFixed),
                   expectedAddColnames)
})

test_that("fixDesignMatrixColnames works for additive models, without removing contrast names", {
  expect_equivalent(myDesignAdd, myDesignAddFixedKeepContrastName)
  expect_identical(colnames(myDesignAddFixedKeepContrastName),
                   gsub("Dis", "myFac2Dis", gsub("Fac1", "myFac1Fac1", expectedAddColnames)))
})


test_that("fixDesignMatrixColnames works for interaction models", {
  expect_equivalent(myDesignInt, myDesignIntFixed)
  expect_setequal(colnames(myDesignIntFixed),
                  expectedIntColnames)
})

test_that("fixDesignMatrixColnames works with interceptChar='.'", {
  expect_equivalent(myDesignInt, myDesignIntFixedPoint)
  expect_setequal(colnames(myDesignIntFixedPoint),
                  gsub("_Dis", ".Dis", expectedIntColnames))
})

test_that("fixDesignMatrixColnames works with interaction(f1, f2)", {
  expect_equivalent(myDesignIntactionFixed, myDesignInteraction)
  expect_setequal(colnames(myDesignIntactionFixed),
                  expectedInteractionColnames)
})
