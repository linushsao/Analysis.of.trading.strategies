#' A dbg Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

dbg<- function(x=NULL,type="print",name,pause=FALSE,debug=FALSE)  {


    announce <- function(x,op){
        if(debug){
            print(msg)}
    }
    
    msg <- "[dbg] ####### Beginning to DEBUG PROCESS. #######" 
    announce(msg,op=debug)
    
    if(debug || (type == "if.debug"))   {
#         msg <- paste("[Debug Msg] ",x,sep="")
        if(type == "print") {
            print(x)
            }else if(type == "head")  {
            print(head(x))
            }else if(type == "tail")  {
            print(tail(x,3))
            }else if(type == "msg")  {
            View(x)
            }else if(type == "file")  {
                write.zoo(x,file=name,sep=",")
            }else if(type == "custom")  {
            #custom funtion
                return("")
            }else if(type == "if.debug")  {
                return(debug)
            }else if(type == "exec")  {
            #custom funtion
                f <- function(y){
                    y <- gsub(".TW","",y)
                    return(y)
                }
                return(f(x))
            }else{
            View(x)
        }
    }
    
    msg <- "[dbg] ####### End of DEBUG PROCESS. ####### "
    announce(msg,op=debug)

    if( pause ) {
    stop()
    }
}

#testing
# dbg("HELLO",op=TRUE)
