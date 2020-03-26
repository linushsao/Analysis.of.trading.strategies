#' A backtestFunction
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_backtest <- function(ret,object2){
	charts.PerformanceSummary(cbind(ret,object2),lty=c(1,6))
	performance <- function(x){
		winpct <- length(x[x>0]) / length(x[x != 0])
		annRet <- Return.annualized(x,scale=252)
		sharpe <- SharpeRatio.annualized(x,scale=252)
		DDs <- findDrawdowns(x)
		maxDD <- min(DDs$return)
		perfo <- c(winpct,annRet,sharpe,maxDD)
		names(perfo) <- c("win rate","annualized return","annualized sharpe ratio","maxium drawdown")

		return(perfo)		
	}
	cbind( BuyAndHold=performance(ret),Trade=performance(object2) )
}
