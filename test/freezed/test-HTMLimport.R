# Testing HTLM function
context('HTML IMPORT')
#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)
# Set up (after context)

  # Locations and references specification

  testObj <- list(artdir = "../extra/articles",
                    # The user specifies the directory containing all the articles of 
                    # interest in HTML format.
                    # RELATIVE PATH: running the autotest will set the wd to the directory
                    # where the script containing the function auto_test is. To specify relative
                    # path keep this in mind.
                    
                  reffile = "../extra/ManuallyCodedComparisonSample.csv",
                    # The user specifies the path to the file (txt for now) that 
                    # reports the manually coded results.
                    # Can give both txt or csv (not csv2)
                  
                  erros_indx = c("plos.Error", "plos.DecisionError"))
                    # The user specifies the names (or numbers) of two columns:
                    # - one containing the logical variable identifying the the error status
                    # - one the decision error status. 
                    # Format for column number specification: erros_indx = c(10, 11)
  # ADD decision error index
  attach(testObj)
  
  # Statcheck options
  
  statOpt <- list(stat           = c("t", "F", "cor", "chisq", "Z", "Q"),
                  OneTailedTests = FALSE,
                  alpha          = 0.05,
                  pEqualAlphaSig = TRUE,
                  pZeroError     = TRUE,
                  OneTailedTxt   = FALSE, 
                  AllPValues     = FALSE)
    # I want the user to be concious at first glance of what is the set up in terms
    # of options tested with a specific test-script
  attach(statOpt)
  
  # packages
  
  library(testthat)
  library(plyr) # for function ddply (need for the auto_test feature)
  
  # Source Functions to be tested
  
  source("../R/statcheck.R")
  source("../R/htmlImport.R")
    # RELATIVE PATH: running the autotest will set the wd to the directory
    # where the script containing the function auto_test is (say user/project/test). 
    # To specify relative path keep this in mind.
    # (e.g. "../R/anyFunction.R" if your scripts are under: user/project/R)
  
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
  sttchckOutput <- checkHTMLdir(artdir, stat = stat,
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
  