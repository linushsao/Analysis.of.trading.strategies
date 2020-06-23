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

    data.clean <- function(data, replace=NA)
    {
        data[is.infinite(data)] <- replace
        data <- na.filter(data)
        return(data)
    }

    append.col.xts <- function(data=NULL, col.data=NULL, col.name=NULL)
    {

        tmp.data <- xts(data.frame(col.data), order.by=index(data))
        names(tmp.data) <- col.name
        data <- cbind(data, tmp.data)
        return(data)
    }
