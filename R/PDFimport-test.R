# Testing PDF function
#context
context('PDF EXTRACTION')

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
  reff_input <- read.xls("/Users/Edoardo/DriveUni/gh-statcheck/extra/150702ComparePvaluesPLOSStatcheckOrderedCleaned.xlsx")
  colnames(reff_input)
  reff_input <- reff_input[, c(1,  #authors
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
      unique(reff_input[reff_input$discrepancyInclusion == TRUE, 1])
      reff_input <- reff_input[reff_input[, 1] == "Ames,-Daniel-R" | reff_input[, 1] == "Beaman,-CPhilip", ]
    # Trying to select articles that do not have inclusion errors
      # inv_index <- unique(reff_input[reff_input$discrepancyInclusion==TRUE, 1]) # articles w/ inclusion error
      # reff_input[-inv_index, 1]
    # Define the comparison terms
      reff_ext   <- nrow(reff_input)            # assuming the manual checks are correct
      reff_er    <- sum(na.omit(reff_input$plosError))
      reff_decEr <- sum(na.omit(reff_input$plosDecisionError))

# Get statcheck output for the selected articles
  # load data for cleaning
  # plosdata_loc <- "/Users/Edoardo/DriveUni/gh-statcheck/extra/ManuallyCodedComparisonSample.txt"
  # plosdata <- read.table(plosdata_loc, header = TRUE, sep = "\t")
      
  # This will be input for your test
    # stat_input <- checkPDFdir("/Users/Edoardo/DriveUni/gh-statcheck/extra/articles")
    # stat_input <- checkdir("/Users/Edoardo/DriveUni/gh-statcheck/extra/articles")
    
    stat_input <- checkHTMLdir("/Users/Edoardo/DriveUni/gh-statcheck/extra/articles")
    # stat_input <- clean_stat(stat_input, plosdata = plosdata)
    
    #stat_input1 <- checkHTML("/Users/Edoardo/DriveUni/gh-statcheck/extra/articles/Ames2004.htm")
    #stat_input2 <- checkHTML("/Users/Edoardo/DriveUni/gh-statcheck/extra/articles/Beaman2004.htm")
    #stat_input <- rbind(stat_input1, stat_input2)
    
  #test group
  test_that('TEST: # of extractions', {
    #expectations
    expect_equal(nrow(stat_input), reff_ext)
  })
  test_that('TEST: # of errors', {
    #expectations
    expect_equal(sum(stat_input$Error), reff_er)
  })
  test_that('TEST: # of decision errors', {
    expect_equal(sum(stat_input$DecisionError), reff_decEr)
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