#' A tradeBysignal Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    tradeBysignal <- function(signal,price,limit.up=0,limit.dn=0,testing=FALSE){
    
        if((limit.up+limit.dn) != 0){
            temp = 0 
            for(i in 1:length(signal)){
                ifelse(signal>=limit.up,1,ifelse(signal<=-1,-1),temp)
                temp <- signal
            }
        }
        
        if( testing ){
             for(i in 1:length(signal)){
                ifelse(signal<0,0,signal)
            }           
        }
        
        ret <- ROC(price,n=1,type="discrete")[-1]
        names(ret) <- "ret"
        signal <- na.omit(lag(signal,1))
        tradeRet <- ret * signal
        names(tradeRet) <- "tradeRet"
        Return <- na.omit(merge(ret,tradeRet))
        return(Return)
    }
