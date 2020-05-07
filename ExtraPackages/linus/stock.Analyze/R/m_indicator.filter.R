#' A m_indicator.filter Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_indicator.filter <- function(ind, level=1){

    for(rowid in 1:nrow(ind))
    {
        ind$filted <- apply(ind, 1, function(v) return(ifelse(sum(v) >= level, 1, 0)) )
    }
    
    return(ind)
 }
 
#  testing
# stop()
# 
# a <- data.frame(a=c(rep(0,5), rep(1,5)), b=rep(1,10))
# 
# m_indicator.filter(a, level=2)
 
 
 
