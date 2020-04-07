#' myci
#'
#' given a sample of a population, my ci creates a 95% confidence interval of the population mean
#'
#' @param x sample of the population
#'
#' @return 95% confidence interval of the population mean
#' @export
#'
#' @examples
#' myci(x)
myci<-function(x)
{
  m = mean(x)
  mp=c(-1,1)
  return(m + mp*qt(1-.05/2, length(x) - 1)*(sd(x)/sqrt(length(x))))
}
