#' A m_rebind.xtsFunction
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    m_rebind.xts <- function(x, v, byrow=TRUE)
        {
            for(id in 1:length(v))
            {
                if(is.na(v[id]) || is.null(v[id])) next
                if(id==1) {tmp.data <- x[as.character(v[id])]
                    }else{
                    if(byrow) {
                        tmp.data <- rbind(tmp.data, x[as.character(v[id])])
                        }else{
                        tmp.data <- cbind(tmp.data, x[as.character(v[id])])
                        }
                }
            }
            return(tmp.data)
        }
 
#  testing
#  stop()
#  
# v <- c("abcd","abcd","abcd","abcd")
# 
# be_replace <- c("a","b")
# patten <- c("+","*")
# patten <- c("+")
# m_gsub(be_replace,patten,v)
# # #   
#   b <- c()
#  a <- c()
#  length(a)
# v<-  gsub("a","+",v)
# v
# v<- gsub("b","-",v)
# v


 
 
 
