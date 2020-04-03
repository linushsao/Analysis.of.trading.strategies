#' A multi sort Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

get.conf <- function(name,env.name="env"){

    result <- m_env(name=name,mode="r",dataset=env.name)

    return(result)
}
