## How does testing work

When changing and maintaining functions or other parts of some software, such as an R package, it is good programming practice to have a testing framework in place. This allows to systematize unstructured developer tests (running code with different inputs to check whether the output you get is as expected) and it makes debugging easier. The first part of my internship was devoted to researching and implementing R testing tools/frameworks for the R package `statcheck`.
Thanks to the package `testthat`, aimed at making testing easier in R, I tried to put in place a framework to test the following statcheck functions:

* `statcheck` - the goal here was writing testing functions to check that changes to the main function `statcheck` would not change what is extracted and what is not. In particular, the script "test-statcheck_DOextra.R" checks that, after changes to `statcheck` are saved, the function still works as expected with different reported comparison signs, positive and negative statistics, different capitalization styles, and different spacing in the reporting of statistics and pvalues. The script "test-statcheck_NOTextra.R" checks that after changes, `statcheck` continues to not extracting invalid pvalues (smaller than 0 or larger than 1), does not read semi-colons as commas, does not read squared brackets as round ones (only APA style is admitted). [For an easy exampe try to change the regular expression for the f tests that allows for 1 space after the first degree of freedom]

* `checkPDF` and `checkPDFdir` - the goal was to write a test that checks whether the number of extracted pvalues,
and the number of reporting errors and decision errors detected is the same after changes have been done to the pdf reading function in the package

* `checkHTML` and `checkHTMLdir` - checking the same things as in `checkPDF` and `checkPDFdir` but for HTML inputs.

The latter two aspects are not currently fully implemented. The testing scripts I worked on are now in the folder 'freezed'.

