#' safe mode for library
#'
#' library multiple packages with automatic installation
#'
#' @param ...     names of packages, same as in library()
#' @param install a bool value to indicate whether automatically install
#'                the uninstalled packages, default as TRUE
#'
#' @return
#' @export
#'
#' @examples
#' slibrary(lattice)
#' slibrary(lattice, sf, raster, install = FALSE)
slibrary <- function(..., install = TRUE){
  funs <- as.character(substitute(list(...)))
  for(i in seq_along(funs)){
    fun_name <- funs[i]
    if(fun_name != "install" & fun_name != "list"){
      if(is.element(fun_name, installed.packages()[,1]) == FALSE){
        if(install == TRUE){
          install.packages(fun_name)
        }
        else{
          warning(paste("package '", fun_name, "' is not installed", sep = ""))
        }
      }
      COMMAND <- paste("library(", fun_name, ")", sep = "")
      eval(parse(text = COMMAND))
    }
  }
}
