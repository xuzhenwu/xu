# There are some system settings for personal use
# With lots of fuctions

# 1-slibrary, install the package before library when the package was not installed
slibrary <- function(fun){
  fun_name <- as.character(substitute(fun))
  if(is.element(fun_name, installed.packages()[,1]) == FALSE)
    install.packages(fun_name)
  COMMAND <- paste("library(", fun_name, ")", sep = "")
  eval(parse(text = COMMAND))
}
