### Project:     statcheck traineeship
### Object:      Testing local statcheck reads what it should
### Description: This script tests whether the local version of statcheck DOES
###              read statistics reported in ways we DO want to read.
###              The unwanted results controlled: 1) different comparison signs, 
###                                               2) positive/Negative stats, 
###                                               3) different caps
###              any other result of interest can easly be added with another test
### Requirements: (1) local statcheck function (the one you are working on);
### How to use:  - set path to local statcheck
###              - run the suite.R script which is contained in the same folder as this script
### Output: If the updated/local function works as expected reports: ok
###         if not, reports the number of things that are done wrong

context('STATCHEK: DO extract')

# Set Up ------------------------------------------------------------------

  # # Use the following if you want to run this test independently
  #   setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # # packages
  #   library(testthat)
  #   library(plyr) # for function ddply (need for the auto_test feature)
  # # Source Functions to be tested (local versions, not from package)
     source("../R/statcheck.R")

# Tests Specification -----------------------------------------------------

#> different comparison sings (<, >, =) ####
    
  # Text Strings (define strings to be tested)
    txt_compsign <- c("I did the following t test: t(48) = 1.02, p = .05",
                      "I did the following t test: t(48) = 1.02, p > .05.",
                      "I did the following t test: t(48) = 1.02, p < .05.")
    #statcheck(txt_compsign)
    
  # benchmark and tocheck (aka actual extractions)
    benchmark <- length(txt_compsign) # define benchmark based on how many tests should be read 
    quiet( nextra <- nrow(statcheck(txt_compsign)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){             # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nextra
    }
    #tocheck <- 2
    
  # Test
    test_that('TEST: different Comparison Sings (<, >, =)', {
      expect_equal(tocheck, benchmark)
    })

#> positive/Negative Statistics ####
    
  # Text Strings
    txt_testsign <- c("I did the following t test: t(48) = 1.02, p = .3128421",
                      "I did the following t test: t(48) = -1.02, p = .3128421")
    #statcheck(txt_testsign)
    
  # Benchmark and tocheck
    benchmark <- length(txt_testsign)
    quiet( nextra <- nrow(statcheck(txt_testsign)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){             # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nextra
    }
    
  # Test
    test_that('TEST: positive/Negative Statistics', {
      expect_equal(tocheck, benchmark)
    })

#> different Caps ####
    
  # Text Strings
    txt_caps <- c("I did the following t test: t(48) = 1.02, p = .3128421",
                  "I did the following t test: t(48) = 1.02, P = .3128421",
                  "I did the following t test: T(48) = 1.02, p = .3128421",
                  "I did the following t test: T(48) = 1.02, P = .3128421",
                  "I did the following F test: F(3, 27) = 7.77, p = .000675864",
                  "I did the following F test: F(3, 27) = 7.77, P = .000675864",
                  "I did the following F test: f(3, 27) = 7.77, p = .000675864",
                  "I did the following F test: f(3, 27) = 7.77, P = .000675864",
                  "I did the following chi-sq test: X2 (2, N = 170) = 14.14, p = .0008502331",
                  "I did the following chi-sq test: X2 (2, N = 170) = 14.14, P = .0008502331",
                  "I did the following chi-sq test: x2 (2, N = 170) = 14.14, p = .0008502331",
                  "I did the following chi-sq test: x2 (2, N = 170) = 14.14, P = .0008502331")
    
  # Benchmark and tocheck
    benchmark <- length(txt_caps)
    quiet( nextra <- nrow(statcheck(txt_caps)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){             # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nextra
    }
    
  # Test
    test_that('TEST: different Caps', {
      expect_equal(tocheck, benchmark)
    })
    
#> t test different Spacing ####
    
  # Text Strings
    txt_space <- c("Here is my t test: t(48)=1.02,p=.3128421", # No spacing
                   "Here is my t test: t(48) = 1.02, p = .3128421", # APA spacing
                   "Here is my t test: t ( 48 ) = 1.02 , p = .3128421") # Single space everywhere
    
  # Benchmark and check
    benchmark <- length(txt_space)
    quiet( nextra <- nrow(statcheck(txt_space)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){             # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nextra
    }
    
  # Test
    test_that('TEST: different spacings in t-test', {
      expect_equal(tocheck, benchmark)
    })
    
#> F-test different Spacing ####
    
  # Text Strings
    txt_space_F <- c("Here is my F test: F(2,125)=0.16,p=.854", # No spacing
                     "Here is my F test: F(2, 125) = 0.16, p = .854", # APA spacing
                     "Here is my F test: F ( 2 , 125 ) = 0.16 , p = .854") # Single space everywhere
    
  # Benchmark and check
    benchmark <- length(txt_space_F)
    quiet( nextra <- nrow(statcheck(txt_space_F)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){             # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nextra
    }
    
  # Test
    test_that('TEST: different spacings in F-test', {
      expect_equal(tocheck, benchmark)
    })
    
#> chi-square test different Spacing ####
    
  # Text Strings
    txt_space_chi <- c("Here is my chi-square test: X2(3,N=126)=10.1,p=.017",
                       "Here is my chi-square test: X2 (3, N = 126) = 10.1, p = .017",
                       "Here is my chi-square test: X2 ( 3 , N = 126 ) = 10.1 , p = .017")
    
  # Benchmark and check
    benchmark <- length(txt_space_chi)
    quiet( nextra <- nrow(statcheck(txt_space_chi)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){             # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nextra
    }

  # Test
    test_that('TEST: different spacings in Chi-square', {
      expect_equal(tocheck, benchmark)
    })
    
#> correlation test different Spacing (no need) ####
    
  # # Text Strings
  #   txt_space_r <- c("Here is correlation test: r(112)=.60,p=.012", # No spacing
  #                    "Here is correlation test: r(112) = .60, p = .012", # APA spacing
  #                    "Here is correlation test: r ( 112 ) = .60 , p = .012") # Single space everywhere
  #   
  # # Benchmark and check
  #   benchmark <- length(txt_space_r)
  #   quiet( tocheck   <- nrow(statcheck(txt_space_r)) )
  # 
  # # Test
  #   test_that('TEST: correlation', {
  #     expect_equal(tocheck, benchmark)
  #   })

#> z test different Spacing (no need) ####
    
  # # Text Strings
  #   txt_space_z <- c("We found: z=1.95,p=.05",
  #                      "We found: z = 1.95, p = .05")
  #   
  # # Benchmark and check
  #   benchmark <- length(txt_space_z)
  #   quiet( tocheck   <- nrow(statcheck(txt_space_z)) )
  # 
  # # Test
  #   test_that('TEST: z-test', {
  #     expect_equal(tocheck, benchmark)
  #   })
    
  