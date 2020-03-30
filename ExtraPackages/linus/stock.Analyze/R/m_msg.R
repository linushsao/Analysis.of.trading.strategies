#' A message Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_msg <- function(v, order="%b%d %X"){
    
    result <- paste(format(Sys.time(), order), v,sep=" ")
    return(result)
    
}
