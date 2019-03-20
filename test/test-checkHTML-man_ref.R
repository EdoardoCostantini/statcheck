# Testing checkHTLM function (w/ manually coded reference)

context('HTML IMPORT: checkHTLM (test w/ manual reference)')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")

# Set up (after context)

# packages
  
  library(testthat)
  library(plyr) # for function ddply (need for the auto_test feature)


  # Locations and references specification

  testObj <- list(reffile_csv = "../extra/testHTML_articles/TrialArt.csv",
                  reffile_html = "../extra/testHTML_articles/TrialArt.html",
                  erros_indx = c("Error", "DecisionError")) # column names with error and decision error (can be numeric)
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
  
  
  # Source Functions to be tested
  
  source("../R/statcheck.R")
  source("../R/htmlImport.R")

# Import reference data (e.g. manually coded plos)
  if(grepl(".csv", reffile_csv) == TRUE){
    ref_input <- read.csv(reffile_csv, header = TRUE)
  } else {
    ref_input <- read.table(reffile_csv, header = TRUE)
  }

  ref_input <- ref_input[, c(erros_indx[1], erros_indx[2])]
    # allows to specify the columns containing error and decision erros
    # both as character (names of columns) and as number (e.g. 3rd and 4th columns)

# Get statcheck output for the selected articles
  sttchckOutput <- checkHTML(reffile_html, stat = stat,
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
#Test: Number of extractions ####
  
  # benchmark and tocheck (aka actual extractions)
  benchmark <- nrow(ref_input)              # reference number of extractions     
  tocheck   <- nrow(sttchckOutput)
  
  # test
  test_that('TEST: Number of extractions', {
    expect_equal(tocheck, benchmark)
  })


#Test: Number of Errors ####

  # benchmark and tocheck (aka actual extractions)
  benchmark <- sum(na.omit(ref_input[, 1])) # reference number of erros
  tocheck   <- sum(sttchckOutput$Error)
  
  # test
  test_that('TEST: Number of errors', {
    expect_equal(tocheck, benchmark)
  })

#Test: Number of decision errors ####
  
  # benchmark and tocheck
  benchmark <- sum(na.omit(ref_input[, 2])) # reference number of decision errors
  tocheck   <- sum(sttchckOutput$DecisionError)
  
  # test
  test_that('TEST: Number of decision errors', {
    expect_equal(tocheck, benchmark)
  })
    
  