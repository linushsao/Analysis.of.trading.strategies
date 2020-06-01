#' A m_dupli.vector Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    m_dupli.vector <- function(x) {
    
        result <- rep(0, length(x)*2)
        for(i in 1:length(x)) {
        
            pos <- 2*i -1
            result[pos] <- x[i]
            result[pos+1] <- x[i]
            }
        
        return(result)
        }
