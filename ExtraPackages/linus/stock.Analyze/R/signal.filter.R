#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

signal.filter <- function(signal, max.days=0, value.filted=NULL, fixed.indicator=NULL)
{
    row.num <- nrow(signal)
    tmp.data <- c(rep(0, row.num))
    signal[is.na(signal)] <- 0
    
    if(max.days < 0) 
    {   # if down max.days days, sell it.
        signal[signal == 0] <- -1
        #work only for value <0
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  signal[rowid]<0, 
                                                            signal[rowid] + ifelse(tmp.data[rowid-1]>0, 0, tmp.data[rowid-1]),
                                                            signal[rowid]) 
        #check if continue [max.days]days <0                                                         
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  tmp.data[rowid]<0, 
                                                            tmp.data[rowid] - (max.days-1),
                                                            tmp.data[rowid])
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  tmp.data[rowid]>0, 
                                                            1, 0);tmp1.data <- tmp.data   
    }
    
    if(max.days > 0)
    { # if up max.days days, buy it.
        for(rowid in 2:row.num) tmp.data[rowid] <- signal[rowid] * (signal[rowid] + tmp.data[rowid-1])

            while(TRUE)
            {
#                 if(row.num < 2 || max.days==0) break
                if(row.num < 2) break
                if( tmp.data[row.num] == 0 || is.na(tmp.data[row.num]) ) { row.num <- row.num -1 ; next }
        
                t.range <- c((row.num - tmp.data[row.num] + 1) : row.num )
                t.fill <- ifelse(tmp.data[row.num] > max.days, 1, 0)
                t.step <- tmp.data[row.num]
                for( v.id in 1:length(t.range)) tmp.data[t.range[v.id]] <- t.fill
                
                row.num <- row.num - t.step 
            }
        }

    if(! is.null(fixed.indicator)) {
        tmp.data <- signal
        co.indicator <- fixed.indicator[[1]]
        co.mode <- fixed.indicator[[2]]
        
        if( co.mode == 'and' ) tmp.data <- as.numeric(tmp.data & co.indicator)
        if( co.mode == 'not' ) tmp.data <- as.numeric(!(tmp.data & co.indicator))
        if( co.mode == 'or' ) tmp.data <-  as.numeric(tmp.data | co.indicator)
    }
    
    if(! is.null(value.filted))
    {
        tmp.data <- signal
        if(value.filted[1] == 'min') tmp.data <- ifelse(tmp.data < value.filted[2], 0, tmp.data)
        if(value.filted[1] == 'max') tmp.data <- ifelse(tmp.data > value.filted[2], 0, tmp.data)   
    }
#     return(data.frame(a=tmp.data, b=tmp1.data, c=co.indicator))
    return(tmp.data)
}

#testing
# stop()
# a <- read.csv(file='/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stock.price/1101.TW.csv', header=TRUE, sep=',')
# a.xts <- xts(a[,-1], order.by=as.Date(a$Index))
# close <- Cl(a.xts)
# b <- m_smaCal(close, n=5, increase=FALSE)

