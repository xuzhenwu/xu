#' safe mode for library
#' check if it is installed before library
#'
#' @param fun the name of the packages
#' @param install automatically install the uninstalled packages
#'
#' @return
#' @export
#'
#' @examples
#' slibrary(lattice)
#' slibrary(lattice, install = FALSE)
#'
slibrary <- function(..., install = TRUE){
  funs <- as.character(substitute(list(...)))
  for(i in seq_along(funs)){
    fun_name <- funs[i]
    if(fun_name != "install" & fun_name != "list"){
      if(is.element(fun_name, installed.packages()[,1]) == FALSE){
        if(install == TRUE){
          install.packages(fun_name)
          COMMAND <- paste("library(", fun_name, ")", sep = "")
          eval(parse(text = COMMAND))
        }
        else{
          print(paste("package '", fun_name, "' is not installed", sep = ""))
        }
      }
    }
  }
}
