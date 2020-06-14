#' A MISC Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

na.filter <- function(x) {return(x[complete.cases(x),])}

replace.colname <- function(x, pattern, replace)
{
    tmp.colname <- names(x)
    tmp.colname <- gsub(pattern, replace, tmp.colname)
    names(x) <- tmp.colname
    return(x)
}
