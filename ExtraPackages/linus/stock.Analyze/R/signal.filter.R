#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

signal.filter <- function(signal, max=0, co.indicator=NULL)
{
    row.num <- nrow(signal)
    tmp.data <- c(rep(0, row.num))
    signal[is.na(signal)] <- 0
    
    if(max < 0) 
    {
        signal[signal == 0] <- -1
        #work only for value <0
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  signal[rowid]<0, 
                                                            signal[rowid] + ifelse(tmp.data[rowid-1]>0, 0, tmp.data[rowid-1]),
                                                            signal[rowid]) 
        #check if continue 3days <0 or using co.indicator                                                          
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  tmp.data[rowid]<0, 
                                                            tmp.data[rowid] - (max-1),
                                                            tmp.data[rowid])
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  tmp.data[rowid]>0, 
                                                            1, 0);tmp1.data <- tmp.data   
        if(! is.null(co.indicator)) {
            for(rowid in 3:(row.num-3)) 
            {
                if(sum(tmp.data[(rowid-1):(rowid+1)])!=0 && !is.na(co.indicator[rowid-1]))
                {
                    if(mean(co.indicator[(rowid-1):(rowid+1)])>0){
                        tmp.data[rowid] <- 1
                        }else{
                        tmp.data[rowid] <- 0}
                }
            }
        }
        
    }else{
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

