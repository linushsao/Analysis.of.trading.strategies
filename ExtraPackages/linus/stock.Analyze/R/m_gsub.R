#' A multi gsub Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_gsub <- function(be_replace,patten,v,op=""){

#testing
# v <- c("abcd","abcd","abcd","abcd")
# be_replace <- c("a","b")
# patten <- c("+","*")

    r1 <- length(be_replace)
    r2 <- length(patten)
    
    r <- max(r1,r2)
    j <- 0
    for(i in 1:r){
        if((i <= r1) || (i <=r2)){
            j=j+1
#             p <- ifelse(r2==0,"",patten[i])
            if(i>r2) {
                j=1
                p <- patten[j]
            }else if(r2==0){
                p <- ""
            }else{
                p <- patten[j]
            }
            v <- gsub(be_replace[i] ,p ,v)
        }
    }
    return(v)
 }
 
 #testing
#  stop()
#  
# v <- c("abcd","abcd","abcd","abcd")
# be_replace <- c("a","b")
# be_replace <- c("a")
# patten <- c("+","*")
# patten <- c("+")
# m_gsub(be_replace,patten,v)
#   
#   b <- c()
#  a <- c()
#  length(a)
# v<-  gsub("a","+",v)
# v
# v<- gsub("b","-",v)
# v


 
 
 
