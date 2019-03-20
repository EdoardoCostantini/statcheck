# Testing statcheck function

context('STATCHEK: DO extract')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)

# Set up (after context)
  #library(ddpcr)
  library(plyr)
  source("../R/statcheck.R")

#Test: Different comparison sings (<, >, =) ####
    
  # Text Strings (define strings to be tested)
    txt_compsign <- c("Correct way of reporting t test: t(48) = 1.02, p = .05",
                      "Correct way of reporting t test: t(48) = 1.02, p > .05.",
                      "Correct way of reporting t test: t(48) = 1.02, p < .05.")
    #statcheck(txt_compsign)
    
  # benchmark and tocheck (aka actual extractions)
    benchmark <- length(txt_compsign) # define benchmark based on how many tests should be read 
    quiet( tocheck   <- nrow(statcheck(txt_compsign)) ) # num of tests statcheck able to extract
    #tocheck <- 2
    
  # Test
    test_that('TEST: Different Comparison Sings (<, >, =)', {
      expect_equal(tocheck, benchmark)
    })

#Test: Positive/Negative Statistics ####
    
  # Text Strings
    txt_testsign <- c("Correct way of reporting t test: t(48) = 1.02, p = .3128421",
                      "Correct way of reporting t test: t(48) = -1.02, p = .3128421")
    #statcheck(txt_testsign)
    
  # Benchmark and tocheck
    benchmark <- length(txt_testsign)
    quiet( tocheck   <- nrow(statcheck(txt_testsign)) )
    
  # Test
    test_that('TEST: Positive/Negative Statistics', {
      expect_equal(tocheck, benchmark)
    })

#Test: Different Caps ####
    
  # Text Strings
    txt_caps <- c("Correct way of reporting t test: t(48) = 1.02, p = .3128421",
                  "Correct way of reporting t test: t(48) = 1.02, P = .3128421",
                  "Correct way of reporting t test: T(48) = 1.02, p = .3128421",
                  "Correct way of reporting t test: T(48) = 1.02, P = .3128421",
                  "Correct way of reporting F test: F(3, 27) = 7.77, p = .000675864",
                  "Correct way of reporting F test: F(3, 27) = 7.77, P = .000675864",
                  "Correct way of reporting F test: f(3, 27) = 7.77, p = .000675864",
                  "Correct way of reporting F test: f(3, 27) = 7.77, P = .000675864",
                  "Correct way of reporting chi-sq test: X2 (2, N = 170) = 14.14, p = .0008502331",
                  "Correct way of reporting chi-sq test: X2 (2, N = 170) = 14.14, P = .0008502331",
                  "Correct way of reporting chi-sq test: x2 (2, N = 170) = 14.14, p = .0008502331",
                  "Correct way of reporting chi-sq test: x2 (2, N = 170) = 14.14, P = .0008502331")
    
  # Benchmark and tocheck
    benchmark <- length(txt_caps)
    quiet( tocheck   <- nrow(statcheck(txt_caps)) ) # num of tests statcheck able to extract
    
  # Test
    test_that('TEST: Different Caps', {
      expect_equal(tocheck, benchmark)
    })

# Exactly reported p-values (?)
# test_that('TEST: Exactly reported p-values', {
#   expect_true(nrow(statcheck(txt_testsign)) == 2)
# })
  
  
  