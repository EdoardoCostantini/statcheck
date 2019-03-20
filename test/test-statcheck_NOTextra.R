# Testing statcheck function

context('STATCHEK: do NOT extract')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)

# Set up (after context)
  library(plyr)
  source("../R/statcheck.R")

#Test: Invalid p-values ####
        
  # Text Strings
    txt_invP <- c("A test that should not be read: t(48) = 1.02, p = 1",
                  "A test that should not be read: t(48) = 1.02, p = 1.01",
                  "A test that should not be read: t(48) = 1.02, p > 1",
                  "A test that should not be read: t(48) = 1.02, p < 0",
                  #"Correct way of reporting t test: t(48) = 1.02, p = .05", #use to check if test works
                  "A test that should not be read: t(48) = 1.02, p = -.01"
    )
    # you may simply add any other invalid pvalue you want to keep track of
    #statcheck(txt_invP)
    
  # benchmark and tocheck
    benchmark <- 0                               # for every test in this context (will not repeat definition)
    quiet( nextra <- nrow(statcheck(txt_invP)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){
      tocheck <- 0
    } else {
      tocheck <- nrow(statcheck(txt_invP))
    }
    #tocheck <- 1
    
  # Test
    test_that("TEST: Invalid p-values", {
      expect_equal(tocheck, benchmark)
    })
    
#Test: Non-APA: semi-colon instead of comma ####
    
  # Text Strings
    txt_semico <- c("Correct way of reporting t test: t(48) = 1.02; p = .3128421")
    #statcheck(txt_testsign)
    
  # tocheck
    quiet( nextra <- nrow(statcheck(txt_semico)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){
      tocheck <- 0
    } else {
      tocheck <- nrow(statcheck(txt_semico))
    }
    
  # Test
    test_that('TEST: Non-APA: semi-colon instead of comma', {
      expect_equal(tocheck, benchmark)
    })
  
#Test: Non-APA: brakets (squraed instead of round) ####

  # Text Strings
    txt_brakets <- c("Correct way of reporting t test: t[48] = 1.02; p = .3128421")
    #statcheck(txt_brakets)
    
  # tocheck
    quiet( nextra <- nrow(statcheck(txt_brakets)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){
      tocheck <- 0
    } else {
      tocheck <- nrow(statcheck(txt_brakets))
    }
    
  # Test
    test_that('TEST: Non-APA: brakets (squraed instead of round)', {
      expect_equal(tocheck, benchmark)
    })

    