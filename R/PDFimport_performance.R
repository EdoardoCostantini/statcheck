### Project:     statcheck traineeship
### Object:      checkperformance of new function on many papers
### Description: This script tests whether the extractions done using the new version are in line with "reliability" studies.
###              Expect 10 % of extractions to be errors, expect 1.5% extractions to be gross errors (decision errors)
### Requirements: modified version of checkDIR with pdf_text function instead of call to pdftotext external app
### Date:        2019-04-10


# set up ------------------------------------------------------------------

  # functions and packs
  library(pdftools) # for pdf_text function in the new getPDF function
  library(tabulizer)
  library(stringi)  # for encoding function in the new getPDF function
  library(plyr)     # for ddply function
  library(stringr)  # for str_remove
  
  source("./R/statcheck.R")
  source("./R/PDFimport.R")
  source("./R/pdf_process.R")

  # Location of articles to test
  x <- "./extra/articles_consistency" # (89 articles across different pubblications and publishers)
  x <- "./extra/articles_BDM"         # (306 articles from Journal of Behavioral Decision Making, Wiley)

# Perform statcheck -------------------------------------------------------

  out_pdftools  <- checkPDFdir(x)            # using new version of getPDF w/ pdftools:pdf_text
  out_baseline  <- statcheck::checkPDFdir(x) # using CRAN version of getPDF w/ pdftotext
  out_tabulizer <- checkPDFdir_tabu(x)       # using new version of getPDF w/ tabulizer::extract_text

# Performaces -------------------------------------------------------------

  # Percentage of errors found among all extractions
  percErrors <- c(round(mean(na.omit(out_pdftools$Error))*100, 1),
                  round(mean(na.omit(out_tabulizer$Error))*100, 1),
                  round(mean(na.omit(out_baseline$Error))*100, 1))
  # Percentage of decision errors found among all extractions
  percDecErr <- c(round(mean(na.omit(out_pdftools$DecisionError))*100, 1),
                  round(mean(na.omit(out_tabulizer$DecisionError))*100, 1),
                  round(mean(na.omit(out_baseline$DecisionError))*100, 1))
  # Number of extractions
  nextract <- c(nrow(out_pdftools[is.na(out_pdftools$Error)==FALSE, ]),
                nrow(out_tabulizer),
                nrow(out_baseline))
  # Summary
  percComp <- data.frame(percErrors = percErrors,
                         percDecErr   = percDecErr,
                         nextract     = nextract)
    colnames(percComp) <- c("Errs_%", "Dec_Errs_%", "n_extra")
    row.names(percComp) <- c("pdftools", "tabulizer", "CRAN")
  percComp
  
# Explore differences in results ------------------------------------------
  
  # pdftool YES | CRAN NO
  length(out_pdftools$Value %in% out_baseline$Value)
  sum(out_pdftools$Value %in% out_baseline$Value == FALSE) #approx. because does not have all values
  out_pdftools[out_pdftools$Value %in% out_baseline$Value == FALSE, ]
  unique(out_pdftools[out_pdftools$Value %in% out_baseline$Value == FALSE, ]$Source) #from which journals
  
  # tabulizer YES | pdftools NO
  length(out_tabulizer$Value %in% out_pdftools$Value)
  sum(out_tabulizer$Value %in% out_pdftools$Value == FALSE) #approx. because does not have all values
  out_tabulizer[out_tabulizer$Value %in% out_pdftools$Value == FALSE, ]
  unique(out_tabulizer[out_tabulizer$Value %in% out_pdftools$Value == FALSE, ]$Source) #from which journals
  
  nrow(out_pdftools[out_pdftools$Source == "bdm-13-26-4-362", ])
  nrow(out_tabulizer[out_tabulizer$Source == "bdm-13-26-4-362", ])
  
  out_tabulizer[out_tabulizer$Value %in% out_pdftools$Value == FALSE, ][out_tabulizer[out_tabulizer$Value %in% out_pdftools$Value == FALSE, ]$Source != "bdm-13-26-1-51",]
  
  # CRAN YES | pdftools NO
  length(out_baseline$Value %in% out_pdftools$Value)
  sum(out_baseline$Value %in% out_pdftools$Value == FALSE) #
  out_baseline[out_baseline$Value %in% out_pdftools$Value == FALSE, ]
  unique(out_baseline[out_baseline$Value %in% out_pdftools$Value == FALSE, ]$Source) #from which journals
   # When test is on two lines, does statcheck misses the test
   # What happens
    # Check out this result
    out_baseline[out_baseline$Value %in% out_pdftools$Value == FALSE, ][1, ]
    # is in article
    art <- "./extra/articles_pub/APA-JEP-2019.pdf"
    # read it with new fucntion
    out <- getPDF(art)[10]
    # Observe how in the middle of the output there is this bit:
    # "t(78.0) = 1.01,\n                                                                 p = 0.32),"
    # in the paper itself (page 10), you can see that the paper presents has a return between "," and "p"
    # could be fixed somehow because it's very consisntent across papers
    out_no_n <- str_remove_all(out, "\n                                                                ")
    statcheck(out_no_n)
    
    stri_enc_toutf32(txtfiles)
    
    # Check out this other result:
    out_baseline[out_baseline$Value %in% out_pdftools$Value == FALSE, ][11, ]
    art <- "./extra/articles_pub/ELS-JESP-2009.pdf"
    getPDF(art)[2]

    # towards the end you can see this is read:
    # "t(65) = 19.72,\nlowed us to attach Black and White heads (matched in level of fa-                     p < .001"
    # unfrotunately words are place in between.
    # This is the disadvantage of this method. However, to me it seems that the overall imporved performance
    # and the easier installation method make it a better option. Especially because there is no particular link
    # between how ”well" a statistic is reported and the fact that it's reported on two different lines.