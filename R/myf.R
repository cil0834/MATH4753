#' My quadratic function
#'
#' Creates a quadratic on a vector
#'
#' This is a test function for working on packages
#'
#' @param x a vector of double values
#'
#' @return a vector of doubles
#' @export
#'
#' @examples
#' x=1:30; myf(x)
myf = function(x){
  obj = 2*x^2-5*x+6
  return(obj)
}
