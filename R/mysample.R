
#' mysample
#'
#' @param n - the number of events per trial
#' @param iter the number of independent trials. Initially set to 10
#' @param time- the time until the function timesout. Initially set to .5
#'
#'This funcion runs a specified number of trials that records the frequency that the numbers 1:10
#'are randomly selected. For each trial a bar graph of the frequencies for each number is created
#'
#'
#' @return- makes bar graphs for each trial/iteration
#' @export
#'
#' @examples
#' mysample(n=100, iter=5, time=1)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
