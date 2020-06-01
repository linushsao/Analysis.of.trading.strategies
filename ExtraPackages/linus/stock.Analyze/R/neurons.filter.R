#' A neurons.filter Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    neurons.filter <- function(x, type='pureline') {
            
                if(type == 'pureline') cell.fun <- function(n) { return( drop(n) ) }
                if(type == 'tansig')   cell.fun <- function(n) { return( (exp(n)-exp(-n))/(exp(n)+exp(-n)) ) }
                if(type == 'sigmoid')  cell.fun <- function(n) { return( 1/(1+exp(-n)) ) }
                
                return( cell.fun(x) )
            }

#
