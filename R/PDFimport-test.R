# Testing PDF function
#context
context('PDF EXTRACTION')
# library(testthat)
library(plyr)
source("/Users/Edoardo/DriveUni/gh-statcheck/R/statcheck.R")
  #buggy: doesnt work with flexible ./path notation
source("/Users/Edoardo/DriveUni/gh-statcheck/R/PDFimport.R")

# Function tested:
#checkPDF("./extra/McGrawTetlock2005.pdf")

input <- checkPDF("/Users/Edoardo/DriveUni/gh-statcheck/extra/McGrawTetlock2005.pdf", alpha = .1)
pvalues <- 6 # we are assuming known 6 tests in the pdf
errors <- 1 # assuming we know there is 1 error
decisions <- 1 # assuming we know tehre is 1 decision error
# for now I'm using a pdf I know, think about the imput
# after you have read the validation study
# alpha = .1 to get a decision error with this pdf file
  #test group
  test_that('TEST: # of extractions', {
    #expectations
    expect_equal(nrow(input), pvalues)
  })
  test_that('TEST: # of errors', {
    #expectations
    expect_equal(sum(input$Error), 2)
  })
  test_that('TEST: # of decision errors', {
    expect_equal(sum(input$DecisionError), 3)
  })
  
#test_file("./R/PDFimport-test.R")