#
#  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
#
#
# ELM Class
#

#
# Class definition, accessor functions, print and summary methods
#

SFLN = setClass("SLFN",
               slots = c(inputs = "numeric",
                         activation = "character",
                         type = "character",
                         bigdata = "logical"))

elm2=elm()
elm3=new("elm")
getClass("elm")
help("getClass")

# Testing the devtools
elm1 = elm(inputs = 4,
           activation = "sigmoid",
           type = "I-ELM",
           bigdata = FALSE)
