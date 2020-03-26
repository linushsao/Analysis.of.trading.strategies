#' A downbreak Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

downbreak <- function(Line,RefLine){
    data <- merge(Line,RefLine,lag(Line),lag(RefLine))
    data <- na.omit(data)
    signal <- apply(data,1,function(x){
    ifelse(x[1]<x[2] & x[3]>x[4],1,0)
    })
    signal <- xts(as.numeric(signal),order.by=index(data))
    return(signal)
}
