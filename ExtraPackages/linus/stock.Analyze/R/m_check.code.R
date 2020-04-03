#' A m_check.code Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    m_check.code <- function(string, num=4) {
        string.l <- nchar(string)
        if(string.l  <  num) {
            result <- m_paste(c(rep("0",( num-string.l )),string),op="")
            }else{
            result=string}
        return(result)
    }
