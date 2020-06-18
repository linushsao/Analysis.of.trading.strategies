#' A m_time.invested Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

   m_time.invested <- function( data=NULL, ma.VALUE=c(3,5,20) ,method=0) 
   {
                
                for(ii in 1:length(ma.VALUE)) {

                    ma.value <- ma.VALUE[ii]
                    col.name <- paste("ma.value", ma.value, sep="")
#                     tmp.stock.ma <- runMean(ROC(data), n=ma.value, cumulative=FALSE)
                    tmp.stock.ma <- m_smaCal(ROC(data), n=ma.value, increase=FALSE)[,2]
                    names(tmp.stock.ma) <- col.name
                    if(ii == 1) {
                            stock.ma <- tmp.stock.ma
                        }else{
                            stock.ma <- merge(stock.ma, tmp.stock.ma)
                        }
                    }
            stock.ma <- na.filter(stock.ma)
            if(method == 0) #default method
            { 
#             stock.ma$average <- apply(stock.ma, 1,function(x) mean(x[c(2,3)]))
#             stock.ma$average_1 <- apply(stock.ma, 1,function(x) mean(x[c(1:3)]))
#               tmp.data <- xts(data.frame(average=apply(stock.ma, 1,function(x) mean(x[c(2,3)])),
#                                      average_1=apply(stock.ma, 1,function(x) mean(x[c(1:3)])) ) ,
#                                      order.by=index(stock.ma))
#               stock.ma <- cbind(stock.ma, tmp.data)
                stock.ma <- append.col.xts( data=stock.ma, 
                                            col.data=apply(stock.ma, 1,function(x) mean(x[c(2,3)])), 
                                            col.name='average')
                stock.ma <- append.col.xts( data=stock.ma, 
                                col.data=apply(stock.ma, 1,function(x) mean(x[c(1,3)])), 
                                col.name='average_1')
            }
        result <- merge(data, stock.ma)
        return(result)
    }
    
    
