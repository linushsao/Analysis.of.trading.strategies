#' A multi sort Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_sort <- function(x,key=NULL,order = FALSE,reindex=FALSE){

                a <- x[(order(x[,key],decreasing = order)),]
                if(reindex){
                    l <- dim(a)[1]
                    rownames(a) <- 1:l
                }
                return(a)
}
