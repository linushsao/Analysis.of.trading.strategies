#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_std.data <- function(x, restore=NULL){
    
    if(is.null(restore)) {
        result <- x
        ret <- c()
        for(i in 1:ncol(x)) {
            v <- c(min(x[,i]), max(x[,i]))
            for(j in 1:nrow(x)){
                result[j,i] <- (x[j,i] - v[1])/(v[2]-v[1])
            }
            ret <- c(ret, v)
        }
        ret.array <- matrix(ret, nrow=2, ncol=ncol(x))
        rownames(ret.array) <- c('min', 'max')
        colnames(ret.array) <- colnames(x)
        
        return(list(data=result, ret=ret.array))
    } else{
        result <- c()
        ret <- restore #ret(min, max)
        result <- (x * (ret[2]-ret[1])) + ret[1]
        
        return(result)
    }
}
