#' myncurve
#'
#' This function takes the average, standard deviation and plots a normal graph
#'
#' Using the parameter a, the area from -infinity to a is shaded and it's area is computed
#'
#' @param mu- the average of the distribution
#' @param sigma - the standard deviation of the distribution
#' @param a - the right end point of the area that is being summed
#'
#' @return a graph with the area from -infinity to a shaded in.
#' @export
#'
#' @examples
#' myncurve(5, 10, 6)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu-3*sigma, a, length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(mu-5*sigma, xcurve, a), c(0, ycurve, 0), col = "RED")

  area=pnorm(a, mean=mu, sd=sigma, lower.tail=TRUE)
  area=round(area,4)
  text(x=xcurve[500], y=ycurve[500], paste("Area = ", area, sep=""))
}
