#' A surge.indicator Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

        oscillation.indicator <- function(  Clprs=Clprs, 
                                            trigger.action=c('brown', 'orange') , 
                                            trigger.mat=c(-0.005, 0.005, -0.01, 0.01, -0.015, 0.015, -0.025, 0.025, -0.04, 0.04),
                                            record.path=NULL,
                                            auto.detect=FALSE
                                    ) 
        {
            # indicator brown down through orange line, used for black swan ex. COVID VIRUS
            # indicator blue down through purple line, used for normal time
            color.name <- c('Close','gray','gray.1','brown','blue','red')
            if( auto.detect && !is.null(record.path) ) 
            {
                if( file.exists(record.path) ) 
                {
                    trade.summary <- list.load(record.path)
                    stock.ma <- trade.summary[[2]]
                }
            
            } else {
                stock.ma <- m_time.invested( data= (Clprs) ) 
                stock.ma <- stock.ma[complete.cases(stock.ma)]
                names(stock.ma) <- color.name
            }
            
            stock.ma$clpr.ret <- ROC(stock.ma[,1])
            stock.ma <- stock.ma[complete.cases(stock.ma)]
        
            trigger.mat <- matrix(trigger.mat, byrow=TRUE, nrow=5, ncol=2)
            colnames(trigger.mat) <- c('min', 'max')
            rownames(trigger.mat) <- c('green', 'purple', 'orange', 'deep pink', 'black')
            
            trigger.action <- matrix(trigger.action, ncol=2) #indicator brown through level orange
            colnames(trigger.action) <- c('indicator','trigger')
            indicator.color <- c('gray80','gray60','brown','blue','red')

            for(trigger.id in 1:nrow(trigger.action)) {
            trade.signal <-  c(rep(0, nrow(stock.ma)))
            trigger.line <-  match(trigger.action[trigger.id, 'indicator'],color.name)
            trigger.level <- match(trigger.action[trigger.id, 'trigger'],rownames(trigger.mat))
            
                for(rowid in 2:nrow(stock.ma)) {
                    
                        if((stock.ma[rowid-1, trigger.line] > trigger.mat[trigger.level,'min']) && (stock.ma[rowid, trigger.line] < trigger.mat[trigger.level,'min'])) {
                            trade.signal[rowid] <- 1    #first time to down through purple.min 
                            }                                        
                        else if((trade.signal[rowid-1] == 1) && (stock.ma[rowid, trigger.line] < trigger.mat[trigger.level,'min'])) {
                            trade.signal[rowid] <- 1    #keep down over purple.min
                            }                        
                        else if((stock.ma[rowid-1, trigger.line] < trigger.mat[trigger.level,'min'])  && ((stock.ma[rowid, trigger.line] > trigger.mat[trigger.level,'min']) )) {
                            trade.signal[rowid] <- 1    #first time up through purple.min and forward to purple.max
                            }                                      
                        else if((trade.signal[rowid-1] == 1) && (stock.ma[rowid, trigger.line] < trigger.mat[trigger.level,'max'])) {
                            trade.signal[rowid] <- 1    #up through min and forward to max
                            }                       
                        else if((trade.signal[rowid-1] == 1) && (stock.ma[rowid-1, trigger.line] < trigger.mat[trigger.level,'max'])  && ((stock.ma[rowid, trigger.line] > trigger.mat[trigger.level,'max']) )) {
                            trade.signal[rowid] <- 0    #first time up through purple.max
                            }    
                        else if((trade.signal[rowid-1] == 1) && (stock.ma[rowid, trigger.line] > trigger.mat[trigger.level,'max'])) {
                            trade.signal[rowid] <- 1    #keep up over max
                            }                          

                    }
            }
            
        trade.signal <- xts(trade.signal, order.by=as.Date(index(stock.ma)))
            
        result <- list(trade.signal=trade.signal , stock.ma=stock.ma, trigger.action=trigger.action, indicator.color=indicator.color, trigger.line=trigger.line, trigger.mat=trigger.mat)
        
        if(!is.null(record.path) && !auto.detect) 
        {
            list.save(result, file=record.path)
        }
        
        return(result)
    }

# a <- read.csv(file='/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stock.price/3050.TW.csv', header=TRUE,sep=',')
# a.xts <- xts(a[,-1], order.by=as.Date(a[,1]))
# 
# Clprs <- a.xts$X3050.TW.Close
# Clprs <- Clprs[complete.cases(Clprs), ]
# 
# 
# 
# b <- oscillation.indicator(Clprs=Clprs)
# 



