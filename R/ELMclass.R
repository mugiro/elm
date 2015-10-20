##' Class "ELM"
##' ELM Class definition, accessor functions, print and summary methods.
##' This is the main class for training the extreme learning machine.
##' Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
##'  --> ../man/ELM-class.Rd
##'      ~~~~~~~~~~~~~~~~~~~~~~~
##' @include SLFNclass.R
##' @keywords classes
##' @import methods
##' @export
setClass("ELM",
         slots = c(ID = "numeric"),
         contains = "SLFN",
         prototype = prototype(ID=0))

# Getter and setter methods (Remove the ones that should be private)


#### Show ####

#' Display a ELM object
#'
#' @rdname show.ELM
#' @name show.ELM
#' @param object The ELM object to be displayed
#' @importFrom methods setMethod
#' @exportMethod show
setMethod("show","ELM",
          function(object) {
            summary(object)
          })


# model1=new("ELM")
#
# show(model1)
#
# showClass("ELM")

