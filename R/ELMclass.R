### ELM Class definition, accessor functions, print and summary methods
### #  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Class "ELM"
#'  --> ../man/ELM-class.Rd
#'      ~~~~~~~~~~~~~~~~~~~~~~~
#' @include SLFN-class.R
#' @keywords classes
#' @export
setClass("ELM",
         slots = c(ID = "numeric"),
         contains = "SLFN",
         prototype = prototype(ID=0))

# Getter and setter methods


#' Display a ELM object
#'
#' @rdname show.ELM
#' @name show.ELM
#' @param object The ELM object to be displayed
#' @exportMethod show
setMethod("show","ELM",   # Show methods for class ELM
          function(object) {
            summary(object)
          })



