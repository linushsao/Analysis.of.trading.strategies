#' A multi sort Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

get.conf <- function(name,env.name="env", default.conf, splite.char=FALSE, splite.num=FALSE){
#     browser()
    result <- m_env(name=name,mode="r",dataset=env.name)
    if( is.null(result) ) { result <- default.conf  }
    
    return(result)
}
