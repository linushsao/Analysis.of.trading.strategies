#' A message Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_msg <- function(info, order="%b%d %X",action="msg"){

    if( action == "msg" ) {
        result <- m_paste(c("[",format(Sys.time(), order),"]", info),op=" ")
        print(result)
    }else if(action == "log") {
        #
    
    }
    
}
