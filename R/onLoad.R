.onLoad <- function(lib, pkg){
  #automatically loads the dataset when package is loaded
  #do not use this in combination with lazydata=true
  utils::data(weibull, package = pkg, envir = parent.env(environment()))
  utils::data(newdata, package = pkg, envir = parent.env(environment()))
}