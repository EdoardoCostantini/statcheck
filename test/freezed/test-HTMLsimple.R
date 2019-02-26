# Testing HTLM function
context('HTML SIMPLE CHECK')
#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")

# Set up (after context)

  # Locations and references specification
#
  testObj <- list(reffile = "../extra/articles/TrialArt.csv", 
                    # for trial purposes you changed the txt file
                    # so that the error column is actually TRUE
                    # (5th column from the right)
                  erros_indx = c("Error", "DecisionError"))
  attach(testObj)
  
  # Statcheck options
  
  statOpt <- list(stat           = c("t", "F", "cor", "chisq", "Z", "Q"),
                  OneTailedTests = FALSE,
                  alpha          = 0.05,
                  pEqualAlphaSig = TRUE,
                  pZeroError     = TRUE,
                  OneTailedTxt   = FALSE, 
                  AllPValues     = FALSE)
  attach(statOpt)
  
  # packages
  
  library(testthat)
  library(plyr) # for function ddply (need for the auto_test feature)
  
  # Source Functions to be tested
  
  source("../R/statcheck.R")
  source("../R/htmlImport.R")

# Import reference data (e.g. manually coded plos)
  if(grepl(".csv", reffile) == TRUE){
    ref_input <- read.csv(reffile, header = TRUE)
  } else {
    ref_input <- read.table(reffile, header = TRUE)
  }
  
  if(is.numeric(erros_indx) == TRUE){
    ref_input <- ref_input[, c(erros_indx[1], erros_indx[2])]
  } else {
    ref_input <- ref_input[, colnames(ref_input) == erros_indx[1] | colnames(ref_input) == erros_indx[2]]
  }
    # allows to specify the columns containing error and decision erros
    # both as character (names of columns) and as number (e.g. 3rd and 4th columns)
  
  ref_ext   <- nrow(ref_input)              # reference number of extractions     
  ref_er    <- sum(na.omit(ref_input[, 1])) # reference number of erros
  ref_decEr <- sum(na.omit(ref_input[, 2])) # reference number of decision errors
    # Define the comparison terms

# Get statcheck output for the selected articles
  sttchckOutput <- checkHTML("../extra/articles/TrialArt.html", stat = stat,
                                OneTailedTests = OneTailedTests, 
                                alpha = alpha, 
                                pEqualAlphaSig = pEqualAlphaSig, 
                                pZeroError = pZeroError,
                                OneTailedTxt = OneTailedTxt, 
                                AllPValues = AllPValues)
    # Arguments will be set at the beginning of the script
    # Such arguments will define the way the user wants to
    # use statcheck for a specific testing purpose

# Define tests of interest
  test_that('TEST: # of extractions', {
    expect_equal(nrow(sttchckOutput), ref_ext)
  })
  test_that('TEST: # of errors', {
    expect_equal(sum(sttchckOutput$Error), ref_er)
  })
  test_that('TEST: # of decision errors', {
    expect_equal(sum(sttchckOutput$DecisionError), ref_decEr)
  })
  