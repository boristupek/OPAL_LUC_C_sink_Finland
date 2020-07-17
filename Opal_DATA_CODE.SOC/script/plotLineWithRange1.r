#function for plotting average with shades
#http://stackoverflow.com/questions/25244241/line-plot-with-average-and-shadow-for-min-max
plotLineWithRange1 <- function(x, yVal, yMin, yMax,
                              lineColor, linetype, #define col and number
                              rangeColor,
                              main="", xlab="X", ylab="Y"){
  if(missing(x)){
    x <- 1:length(yVal)
  }
  stopifnot(length(yVal) == length(yMin) && length(yVal) == length(yMax))
  
  #plot(x=c(min(x),max(x)),y=c(min(yMin),max(yMax)),type="n", main=main,xlab=xlab,ylab=ylab)
  polygon(x=c(x,rev(x)),y=c(yMax,rev(yVal)),col=rangeColor,border=NA)
  polygon(x=c(x,rev(x)),y=c(yMin,rev(yVal)),col=rangeColor,border=NA)
  lines(x=x,y=yVal,col=lineColor, lwd = 1, lty = linetype)#lineWidth)
}