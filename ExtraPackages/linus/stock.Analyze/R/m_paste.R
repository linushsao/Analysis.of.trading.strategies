#' A multi paste Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

 m_paste <- function(x,op=""){
    temp <- ""
     for(i in 1:length(x)){
        temp <- ifelse(i == 1,x[i],paste(temp,x[i],sep=op))
        }
    return(temp)
 }
