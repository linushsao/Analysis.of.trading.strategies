#' A multi gsub Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_gsub <- function(be_replace, patten, v){

    be.num <- length(be_replace)
    pa.num <- length(patten)
    if( be.num == pa.num ) {
    
        for(i in 1:be.num){
            v <- gsub(be_replace[i] ,patten[i] ,v)
            }
        }else{
        
            print(c("length of be_replace :",be.num))
            print(c("length of patten :",pa.num))
            print("error: length of be_replace must be equal with patten.")
            v <- NULL
        }
    
    return(v)
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


 
 
 
