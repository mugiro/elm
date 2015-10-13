# if(require("testthat", quietly = TRUE)) {
#   pkg   <- "lme4"
#   require(pkg, character.only=TRUE, quietly=TRUE)
#   test_package(pkg)
#   print(warnings()) # TODO? catch most of these by expect_warning(..)
# } else {
#   cat( "package 'testthat' not available, cannot run unit tests\n" )
# }


# testing Rpackages code working

library(devtools)
has_devel()
library(roxygen2)
library(testthat)
devtools::session_info()
# Session info -------------------------------------------------------------
#   setting  value
# version  R version 3.1.1 (2014-07-10)
# system   x86_64, darwin13.1.0
# ui       RStudio (0.99.447)
# language (EN)
# collate  en_US.UTF-8
# tz       Asia/Tokyo
# date     2015-10-13
#
# Packages -----------------------------------------------------------------
#   package  * version date       source
# crayon     1.3.1   2015-07-13 CRAN (R 3.1.3)
# devtools * 1.9.1   2015-09-11 CRAN (R 3.1.3)
# digest     0.6.8   2014-12-31 CRAN (R 3.1.2)
# magrittr   1.5     2014-11-22 CRAN (R 3.1.2)
# memoise    0.2.1   2014-04-22 CRAN (R 3.1.2)
# Rcpp       0.12.1  2015-09-10 CRAN (R 3.1.3)
# roxygen2 * 4.1.1   2015-04-15 CRAN (R 3.1.3)
# stringi    0.5-5   2015-06-29 CRAN (R 3.1.3)
# stringr    1.0.0   2015-04-30 CRAN (R 3.1.3)
# testthat * 0.10.0  2015-05-22 CRAN (R 3.1.3)
.libPaths()
# [1] "/usr/local/spark-1.4.0-bin-hadoop2.6/R/lib"
# [2] "/Library/Frameworks/R.framework/Versions/3.1/Resources/library"
lapply(.libPaths(), dir)
# [[1]]
# [1] "crayon"   "magrittr" "SparkR"   "testthat"
#
# [[2]]
# [1] "abind"                 "annotate"              "AnnotationDbi"
# [4] "AnnotationForge"       "ape"                   "assertthat"
# ....

