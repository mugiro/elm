### ELM Class definition, accessor functions, print and summary methods
### #  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

##' Class "ELM"
##'  --> ../man/ELM-class.Rd
##'      ~~~~~~~~~~~~~~~~~~~~~~~
##' @include SLFNclass.R
##' @keywords classes
##' @importFrom methods setClass
##' @export
setClass("ELM",
         slots = c(ID = "numeric"),
         contains = "SLFN",
         prototype = prototype(ID=0))

# Get/Set methods


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



