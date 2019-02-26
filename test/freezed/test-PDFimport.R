# Testing PDF function
#context
context('PDF IMPORT')

library(testthat)
library(plyr)
library(gdata) # for read.xls

# get functions you need
source("/Users/Edoardo/DriveUni/gh-statcheck/R/statcheck.R")
  #buggy: doesnt work with flexible ./path notation
source("/Users/Edoardo/DriveUni/gh-statcheck/R/PDFimport.R")
source("/Users/Edoardo/DriveUni/gh-statcheck/R/checkdir.R")
source("/Users/Edoardo/DriveUni/gh-statcheck/R/htmlImport.R")
source("/Users/Edoardo/DriveUni/gh-statcheck/extra/02FunctionToCleanStatcheckData.R")

# Function tested:
#checkPDF("./extra/McGrawTetlock2005.pdf")

# Import reference data (e.g. manually coded plos)
  ref_input <- read.xls("/Users/Edoardo/DriveUni/gh-statcheck/extra/150702ComparePvaluesPLOSStatcheckOrderedCleaned.xlsx")
  colnames(ref_input)
  ref_input <- ref_input[, c(1,   #authors
                              9,  #plos error (logical version)
                              12, #plos decision error
                              21, #stat error (1-tail yes)
                              22, #stat decision error (1-tail yes)
                              23, #stat error (1-tail yes)
                              24, #stat error (1-tail no)
                              25) #discrepancy: inclusion difference
                          ]
  # Select article of interest
    # Right now using first two articles
      unique(ref_input[ref_input$discrepancyInclusion == TRUE, 1])
      ref_input <- ref_input[ref_input[, 1] == "Ames,-Daniel-R" | ref_input[, 1] == "Beaman,-CPhilip", ]
    # Define the comparison terms
      ref_ext   <- nrow(ref_input)            # assuming the manual checks are correct
      ref_er    <- sum(na.omit(ref_input$plosError))
      ref_decEr <- sum(na.omit(ref_input$plosDecisionError))

# Get statcheck output for the selected articles
  # This will be input for your test
    sttchckOutput <- checkHTMLdir("/Users/Edoardo/DriveUni/gh-statcheck/extra/articles")
    
  #test set
  test_that('TEST: # of extractions', {
    #expectations
    expect_equal(nrow(sttchckOutput), ref_ext)
  })
  test_that('TEST: # of errors', {
    #expectations
    expect_equal(sum(sttchckOutput$Error), ref_er)
  })
  test_that('TEST: # of decision errors', {
    expect_equal(sum(sttchckOutput$DecisionError), ref_decEr)
  })

# # OLD WORKING #
# 
# # Function tested:
#   #checkPDF("./extra/McGrawTetlock2005.pdf")
# 
# input <- checkPDF("/Users/Edoardo/DriveUni/gh-statcheck/extra/McGrawTetlock2005.pdf", alpha = .1)
# pvalues <- 6 # we are assuming known 6 tests in the pdf
# errors <- 1 # assuming we know there is 1 error
# decisions <- 1 # assuming we know tehre is 1 decision error
# # for now I'm using a pdf I know, think about the imput
# # after you have read the validation study
# # alpha = .1 to get a decision error with this pdf file
#   #test group
#   test_that('TEST: # of extractions', {
#     #expectations
#     expect_equal(nrow(input), pvalues)
#   })
#   test_that('TEST: # of errors', {
#     #expectations
#     expect_equal(sum(input$Error), 2)
#   })
#   test_that('TEST: # of decision errors', {
#     expect_equal(sum(input$DecisionError), 3)
#   })
#   
# #test_file("./R/PDFimport-test.R")