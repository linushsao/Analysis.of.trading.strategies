#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

signal.filter <- function(signal, max=0)
{
    row.num <- nrow(signal)
    tmp.data <- c(rep(0, row.num))
    signal[is.na(signal)] <- 0
    
    for(rowid in 2:row.num) tmp.data[rowid] <- signal[rowid] * (signal[rowid] + tmp.data[rowid-1])
    
    while(TRUE)
    {
        if(row.num < 2 || max==0) break
        if( tmp.data[row.num] == 0 || is.na(tmp.data[row.num]) ) { row.num <- row.num -1 ; next }

        t.range <- c((row.num - tmp.data[row.num] + 1) : row.num )
        t.fill <- ifelse(tmp.data[row.num] > max, 1, 0)
        t.step <- tmp.data[row.num]
        for( v.id in 1:length(t.range)) tmp.data[t.range[v.id]] <- t.fill
        
        row.num <- row.num - t.step 
    }
    return(tmp.data)
}

#testing
# stop()
# a <- read.csv(file='/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stock.price/1101.TW.csv', header=TRUE, sep=',')
# a.xts <- xts(a[,-1], order.by=as.Date(a$Index))
# close <- Cl(a.xts)
# b <- m_smaCal(close, n=5, increase=FALSE)

