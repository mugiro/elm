#
#  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
#
#
# ELM Class
#

#
# Class definition, accessor functions, print and summary methods
#

source("R/SLFNclass.R")

setClass("ELM",
         slots = c(ID = "numeric"),
         contains = "SLFN",
         prototype = prototype(ID=0))

# Get/Set methods


#
# PRINT method
#

setMethod("show","ELM",
          function(object) {
            summary(object)
          })


# model1=new("ELM")
#
# show(model1)
#
# showClass("ELM")

