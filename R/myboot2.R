#' myboot2
#'a function that takes in data and with that data makes a specified number of sample statistics from the population.
#'These statistics can then be used to make confidence intervals of the entire population.
#'
#' @param iter - how many samples you want
#' @param x - the data
#' @param fun - the sample statistic that you want to find
#' @param alpha - alpha represents the confidence interval. 1- aplha is the % confidence you want
#' @param cx - defines how much the text on the graph should be scaled
#' @param ... - a parameter that lets you set your own parameter for the histogram
#'
#' @return a histogram of the distribution of the samples which has a histogram, the confidence interval, each samples statistic, the function used
#' @export
#'
#' @examples
#' myboot2(10000,x=sam,fun="mean",alpha=0.05,xlab="mean",col="Purple",cx=1.5)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
