### Project:     statcheck traineeship
### Object:      pdf processing functions (splotPdf and support true_false)
### Description: This script contains an helpful function that helps maintaining the format of 
###              pdf files with a multiple columns layout.
### Requires:    stringi package
### Date:        2019-04-11
### Credits to:  https://github.com/fsingletonthorn/EffectSizeScraping/blob/master/R/pdf_process.R for original function
# library(pdftools)
# library(stringr)
# library(stringi)

true_false <- function(x, chars) {
  x > chars
}

pdf_columns <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
                                  # \p{L} matches a single code point in the category "letter".
                                  # {3,} three or more
  # This function is slightly adapted from pdfsearch! https://github.com/lebebr01/pdfsearch/blob/master/R/split_pdf.r

  x_lines <- stringi::stri_split_lines(x)
  x_lines <- lapply(x_lines, base::gsub,
                    pattern = "^\\s{1,20}",
                    # ^ string that starts with
                    # \ creastes regular expression containing following...
                    # \s matches any whitespace
                    # {1,20} between 1 and 20 of these [ in pyour case this will bocme +]
                    replacement = "")
  
  x_page <- lapply(
    x_lines,
    stringi::stri_split_regex,
    pattern = pattern,
    omit_empty = NA,
    simplify = TRUE
  )
  
  page_lines <- unlist(lapply(x_page, nrow))
  columns <- unlist(lapply(x_page, ncol))
  
  num_chars <- lapply(x_page, base::nchar)
  num_chars_tf <- lapply(num_chars, true_false, chars = 3)
  
  for (xx in seq_along(num_chars_tf)) {
    num_chars_tf[[xx]][is.na(num_chars_tf[[xx]])] <- FALSE
  }
  
  output <- lapply(seq_along(x_page), function(xx)
    x_page[[xx]][num_chars_tf[[xx]]])

  output <- lapply(output, paste, collapse = " ")
  return(output)
}
