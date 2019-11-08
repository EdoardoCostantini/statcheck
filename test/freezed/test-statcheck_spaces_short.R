### Project:     statcheck traineeship
### Object:      Testing local statcheck reads what it should
### Description: This script tests whether the local version of statcheck DOES
###              read statistics reported in ways we DO want to read.
###              The unwanted results controlled: 1) Different comparison signs, 
###                                               2) Positive/Negative stats, 
###                                               3) Different caps
###              any other result of interest can easly be added with another test
### Requirements: (1) local statcheck function (the one you are working on);
### How to use:  - set path to local statcheck
###              - run the suite.R script which is contained in the same folder as this script
### Output: If the updated/local function works as expected reports: ok
###         if not, reports the number of things that are done wrong

context('STATCHEK: different spacings')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)

# Set up (after context)
  #library(ddpcr)
  library(plyr)
  source("../R/statcheck.R")

# tests read by statcheck:
# v t(df) = value, p = value             # yes
# v F(df1, df2) = value, p = value       # yes
# v r(df) = value, p = value             # reduntant (same as t)
# χ2(df, N = value) = value, p = value   # yes
# Z = value, p = value                   # reduntant (samish as t)
# Q (df) = value, p = value              # reduntant (same as t)

#Test: t test different Spacing ####
    
  # Text Strings
    txt_space <- c("Here is my t test: t(48)=1.02,p=.3128421", # No spacing
                   "Here is my t test: t(48) = 1.02, p = .3128421", # APA spacing
                   "Here is my t test: t ( 48 ) = 1.02 , p = .3128421") # Single space everywhere
    
  # Benchmark and check
    benchmark <- length(txt_space)
    quiet( tocheck   <- nrow(statcheck(txt_space)) ) # num of tests statcheck able to extract

  # Test
    test_that('TEST: t-test', {
      expect_equal(tocheck, benchmark)
    })
    
#Test: F-test different Spacing ####
    
  # Text Strings
    txt_space_F <- c("Here is my F test: F(2,125)=0.16,p=.854", # No spacing
                     "Here is my F test: F(2, 125) = 0.16, p = .854", # APA spacing
                     "Here is my F test: F ( 2 , 125 ) = 0.16 , p = .854") # Single space everywhere
    
  # Benchmark and check
    benchmark <- length(txt_space_F)
    quiet( tocheck   <- nrow(statcheck(txt_space_F)) ) # num of tests statcheck able to extract

  # Test
    test_that('TEST: F-test', {
      expect_equal(tocheck, benchmark)
    })
    
#Test: chi-square test different Spacing ####
    
  # Text Strings
    txt_space_chi <- c("Here is my chi-square test: X2(3,N=126)=10.1,p=.017",
                       "Here is my chi-square test: X2 (3, N = 126) = 10.1, p = .017",
                       "Here is my chi-square test: X2 ( 3 , N = 126 ) = 10.1 , p = .017")
    
  # Benchmark and check
    benchmark <- length(txt_space_chi)
    quiet( tocheck   <- nrow(statcheck(txt_space_chi)) )

  # Test
    test_that('TEST: Chi-square', {
      expect_equal(tocheck, benchmark)
    })
    
#Test: correlation test different Spacing ####
    
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

#Test: z test different Spacing ####
    
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
    