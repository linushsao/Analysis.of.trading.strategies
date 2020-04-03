#' A set.conf Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

set.conf <- function(name, value, env.name="env"){

    result <- m_env(name=name, value=value, mode="w",dataset=env.name)
    return(TRUE)
}
