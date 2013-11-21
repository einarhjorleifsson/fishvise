#' @title Do a ggplot
#' 
#' @description xx
#' 
#' @param x xx
#' @param d xx
#' @export 
do.ggplot <- function(x,d) {
  q05 <- q10 <- q16 <- q50 <- q84 <- q90 <- q95 <- value <- variable <- NULL
  gg.plot <- ggplot(x,aes(variable)) +  
    geom_ribbon(aes(ymin=q05,ymax=q95),fill='grey',alpha=1/2) +
    geom_ribbon(aes(ymin=q10,ymax=q90),fill='grey',alpha=1/2) +
    geom_ribbon(aes(ymin=q16,ymax=q84),fill='grey',alpha=1/2) +
    geom_line(data=d,aes(variable,value,group=iter),alpha = 0.05,col='red') +
    geom_line(aes(y=q50),col='red',lwd=1) +
    geom_line(aes(y=mean),col='blue',lwd=1)
  
  return(gg.plot)
}