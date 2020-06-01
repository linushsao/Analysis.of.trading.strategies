#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_smaCal <- function(ts, n=NULL, increase=TRUE){

	len.ts <- length(ts)
	
	if( increase ) 
	{
        n.start <- n
        n.end   <- len.ts
        }else{
        n.start <- len.ts
        n.end   <- n
	}

	for(rowid in n.start:n.end) ts$samCal[rowid] = mean(ts[,1][(rowid-n+1):rowid],na.rm=TRUE)
	return(ts)
}

#testing
# stop()
# a <- read.csv(file='/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stock.price/1101.TW.csv', header=TRUE, sep=',')
# a.xts <- xts(a[,-1], order.by=as.Date(a$Index))
# close <- Cl(a.xts)
# b <- m_smaCal(close, n=5, increase=FALSE)

