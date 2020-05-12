#' A m_series Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_series <- function(type='Fibo', n=15){


    if(type=='Fibo' || type=='Fi')
    {
        f.series <- c(1,1)
        for(i in 3:n) f.series[i] <- f.series[i-1] + f.series[i-2]
        result <- t(t(f.series))
    }
    
    return(result)
 }
 
#  testing
#  stop()
