#' A m_time.invested Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

   m_time.invested <- function( data=NULL, ma.VALUE=c(3,5,20) ) {
        
                stock.close <- data
                
                for(ii in 1:length(ma.VALUE)) {

                    ma.value <- ma.VALUE[ii]
                    col.name <- paste("ma.value", ma.value, sep="")
                    tmp.stock.ma <- runMean(ROC(stock.close), n=ma.value, cumulative=FALSE)
                    names(tmp.stock.ma) <- col.name
                    if(ii == 1) {
                            stock.ma <- tmp.stock.ma
                        }else{
                            stock.ma <- merge(stock.ma, tmp.stock.ma)
                        }
                    }

            stock.ma$average <- apply(stock.ma, 1,function(x) mean(x[c(2,3)]))
            stock.ma$average_1 <- apply(stock.ma, 1,function(x) mean(x[c(1:3)]))
            result <- merge(stock.close, stock.ma)

        return(result)
    }
    
    
