### Methods from SLFN class to save and load models
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Save a SLFN model.
#'
#' \code{saveSLFN} create a .rda file taht stores the object SLFN created.
#' @param object The instance to SLFN class to serialize.
#' @param filename The name of the file where the SLFN object is read from.
#' @param path A character string with the path name to a directory.
#' @export
setGeneric("saveSLFN", function(object, ...) standardGeneric("saveSLFN"))
setMethod("saveSLFN", "SLFN",
          function(object, filename, path="") {
            print("function saveSLFN")
            if(path)
              res <- tryCatch({saveRDS(object, file=paste0(path,filename,".rda"))},
                              error=function(e) return(e))
            if (inherits(res, "error")) {
              print("Error in function saveSLFN.\n")
              print(res)
            }else {
              print("ok, function saveSLFN worked well!!!\n")
            }
          })

#' Load a SLFN model.
#'
#' \code{loadSLFN} reads the settings of a SLFN from a file.
#' @param object The instance to SLFN class to be updated.
#' @param filename The name of the file where the SLFN object is read from.
#' @param path A character string with the path name to a directory.
#' @export
setGeneric("loadSLFN", function(object, ...) standardGeneric("loadSLFN"))
setMethod("loadSLFN", "SLFN",
          function(object, filename="", path="") {
            res <- tryCatch({object=readRDS(file=paste0(path,filename,".rda"))},
                            error=function(e) return(e))
            if (inherits(res, "error")) {
              print("Error in function loadSLFN.\n")
              print(res)
            }else {
              print("ok, function loadSLFN worked well!!!\n")
            }
          })
