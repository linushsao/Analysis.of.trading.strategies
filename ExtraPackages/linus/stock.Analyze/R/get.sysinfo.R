#' A get sysinfo Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    get.sysinfo <- function(x=0){
    
        #get system info
        #x=0 path of home
        #x=1 sysnam
        #x=2 release
        #x=3 version 
        #x=4 nodename
        #x=5 machine
        #x=6 login 
        #x=7 user(default)
        #x=8 effective_user
        
        sys <- Sys.info()
        
        if (x == 0 ) { result <- m_paste(c("/home/",sys[6],"/"),op="") }
     
        return(result)
    }
