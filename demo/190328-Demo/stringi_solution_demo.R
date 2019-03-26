### Project:     Statcheck traineeship
### Object:      Demo for statcheck meeting 28/03/2019
### Description: In this demo you implemented a rough PDF processing system using:
###              - pdftools:pdf_text function to convert pdf files in R character strings
###              - stringi:stri_enc_toutf32 and stringi:stri_encfromut32 to convert unreadable stuff
### Date:        2019-03-26

# Install Packages
  install.packages("pdftools")
  install.packages("stringi")

# Load Packages and functions
  library(pdftools)
  library(stringi)
  library(plyr)
  source("./statcheck.R")
  source("./htmlImport.R")
  source("./PDFimport.R")
  
# Load Articles (two articles for this demo)
  x <- "./Ames2004.pdf"   # real paper not working (nothing is able to read the "=") page 4 for tests
  x <- "./Beaman2004.pdf" # real paper not working (nothing is able to read the "=") page 3 for tests
  page_of_int <- 4
  
# One page exposé ---------------------------------------------------------
    
  # 1. Show output of pdf_text(x) in UTF32 encoding
    text_output_nostrin <- pdftools::pdf_text(x)[[page_of_int]]
    text_output <- stri_enc_toutf32(pdftools::pdf_text(x)[[page_of_int]])
  # 2. Peak at the last 30ish values (there should be an equal sing there)
    tail(text_output[[1]], 30)
  # 3. Substitute codes
    text_output[[1]][which(text_output[[1]] == 11005)] <- 61
    text_output[[1]][which(text_output[[1]] == 11021)] <- 60
  # 4. Get things back in UTF8
    text_output_nostrin[[1]]
    stri_enc_fromutf32(tail(text_output[[1]], 7500))
  # 5. Apply statcheck (happly and produly)
    statcheck(stri_enc_fromutf32(text_output[[1]]))

# Full article test -------------------------------------------------------
  
  # 1. Show output of pdf_text(x) in UTF32 encoding
    text_output <- stri_enc_toutf32(pdftools::pdf_text(x))
    str(text_output) # an object per page
    
  # 2. Peak at the last 30ish values (there should be an equal sing there)
    tail(text_output[[4]], 30)
    
  # 3. Substitute codes
    # Detect how many double solidus bars
    which.11005.detect <- function(x) {which(x == 11005)} 
    lapply(text_output, which.11005.detect)
    
    # Substitute double solidus bars with UTF36 code 61 (aka = sign)
    which.11005.substi <- function(x) {
      x[which(x == 11005)] <- 61
      return(x)
    }
    out_61 <- lapply(text_output, which.11005.substi)
  
    lapply(out_61, which.11005.detect) #check how many double solidus in the form of 11005 are there: ZERO!
    
    # Substitute \u2b0d with UTF36 code 60 (aka < sign)
    which.11021.substi <- function(x) {
      x[which(x == 11021)] <- 60
      return(x)
    }
    out_61_60 <- lapply(out_61, which.11021.substi)
    
  # 4. Get things back in UTF8
    stri_enc_fromutf32(out_61_60)
    
  # 5. Apply statcheck (happly and produly)
    statcheck(stri_enc_fromutf32(out_61_60))
    nrow(statcheck(stri_enc_fromutf32(out_61_60)))
    
  # 6. CFR w/ other methods
    # XPDF (current version)
    statcheck(getPDF(x))
    statcheck::statcheck(x)
    # Manual
    manual_coded <- read.table("ManuallyCodedComparisonSample.txt", header = TRUE)
    manual_coded[manual_coded$authors == "Ames,-Daniel-R", ]
    sum(manual_coded$authors == "Ames,-Daniel-R")
    # HTML
    html_out <- checkHTML("./Ames2004.htm")
    nrow(html_out)