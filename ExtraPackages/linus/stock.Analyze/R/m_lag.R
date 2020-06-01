#' Am_lag Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_lag <- function(x, lag.num=1,shift=FALSE) {
    l <- length(x)
    l.shift <- (l-(lag.num-1))
    if(shift) {
    x.tmp <- x[l.shift:l]
    }else{
    x.tmp <- c(rep(NA, lag.num))
    }
    result <- c(x.tmp, x[1:(l-lag.num)])
    return(result)
}

#testing
# stop()
# a <- 1:10
# m_lag(a,2,shift=TRUE)
