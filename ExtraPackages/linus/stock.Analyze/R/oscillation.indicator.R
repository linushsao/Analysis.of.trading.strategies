#' A surge.indicator Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

        oscillation.indicator <- function(Clprs=Clprs, 
                                    trigger.action=c('brown', 'orange') , 
                                    trigger.mat=c(-0.005, 0.005, -0.01, 0.01, -0.015, 0.015, -0.025, 0.025, -0.04, 0.04)) 
        {
        
            stock.ma <- m_time.invested( data= (Clprs) ) 
            stock.ma <- stock.ma[complete.cases(stock.ma)]
            color.name <- c('Close','gray','gray.1','brown','blue','red')
            names(stock.ma) <- color.name
            
            stock.ma$clpr.ret <- ROC(stock.ma[,1])
            stock.ma <- stock.ma[complete.cases(stock.ma)]
            
                trigger.mat <- matrix(trigger.mat, byrow=TRUE, nrow=5, ncol=2)
                colnames(trigger.mat) <- c('min', 'max')
                rownames(trigger.mat) <- c('green', 'purple', 'orange', 'deep pink', 'chocolate')
                
                trigger.action <- matrix(trigger.action, ncol=2) #indicator brown through level orange
                colnames(trigger.action) <- c('indicator','trigger')
                indicator.color <- c('darkgray','darkgray','brown','blue','red')

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
                
            return( list(trade.signal=trade.signal , stock.ma=stock.ma, trigger.action=trigger.action, indicator.color=indicator.color, trigger.line=trigger.line, trigger.mat=trigger.mat) )
        }



