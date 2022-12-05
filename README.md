# TaxCanada
R-package to support tax planning and financial calculations in the context of British Columbia Canada

# UNDER ACTIVE DEVELOPMENT 

Feel free to contribute but - DO NOT USE!
Functions are incomplete and untested


### Development and deployment

Useful commands to build and test deployment of new functions and features.

```r
library(testthat)
library(devtools)

# Ctrl-Shift-F10 (Restart Session)
rm(list = ls())      # Clear memory
devtools::load_all() # Load functions
devtools::document() # Document function
devtools::test()     # Run tests
devtools::check()    # Operating system test

```
