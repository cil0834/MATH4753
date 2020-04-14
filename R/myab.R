#' Myab
#'
#' This function plots the alpha (type 1 errors) and beta (type 2 errors) of the mean
#' Given one sample we can visualize how the beta's alphas change based off of various cuttoff values, standard deviations
#' and sample sizes.
#'
#'
#' @param a- the value of the null hypothesis
#' @param b- the value of the alternative hypothesis
#' @param sigma- the standard deviation
#' @param xcut- the cutoff mean value
#' @param n- the size of the sample
#'
#' @return a graph that visualises the alpha and beta values (type 1 in green and type 2 errors in blue)
#' @export
#'
#' @examples
#' myab(xcut=14)
myab=function(a=10,b=15,sigma=5, xcut=qnorm(0.9,mean=a,sd=5/sqrt(n)),n=20){ # a for Ho, b for H1
  #xcut is the cut off used to define a one tailed test
  # determine the range of the x values
  lmin=min(a-3*sigma/sqrt(n),b-3*sigma/sqrt(n))
  rmax=max(a+3*sigma/sqrt(n),b+3*sigma/sqrt(n))

  #plot the curves
  curve(dnorm(x,mean=a,sd=sigma/sqrt(n)), xlim=c(lmin,rmax), main=paste("xcut=",xcut),ylab="Density")
  curve(dnorm(x,mean=b,sd=sigma/sqrt(n)),add=TRUE, col="Red", lwd=2)

  #vertical lines
  abline(v=a)
  abline(v=b)

  # Text on the lines
  text(a,0.5*dnorm(a,mean=a,sd=sigma/sqrt(n)),expression(H[0]),pos=1, cex=2)
  text(b,0.5*dnorm(b,mean=b,sd=sigma/sqrt(n)),expression(H[1]),pos=1,cex=2)
  # beta
  #prob accepting H0 when it is false
  # left of xcut when mean is b
  bet=pnorm(xcut,mean=b,sd=sigma/sqrt(n))

  #x,y coords for polygon beta
  xcurve=seq(xcut,lmin,length=1000)
  ycurve=dnorm(xcurve,mean=b,sd=sigma/sqrt(n))
  polygon(c(xcut,xcurve,lmin),c(0,ycurve,0),col="Blue")

  #x,y for polygon for alpha
  xxcurve=seq(xcut,rmax,length=1000)
  yycurve=dnorm(xxcurve,mean=a,sd=sigma/sqrt(n))
  polygon(c(xcut,xxcurve,rmax),c(0,yycurve,0),col="Green")
}
