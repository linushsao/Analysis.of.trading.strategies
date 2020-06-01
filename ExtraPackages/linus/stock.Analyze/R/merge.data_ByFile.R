#' A for merge.data_ByFile  Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

merge.data_ByFile <- function(name,period=NULL,order=NULL,debugger=FALSE,header=FALSE) {

    l_name <- length(name)
    temp <- data.frame()
    
    for(i in 1:l_name) {
        
        name[i]-> symbol
        data_RAW <- read.csv(symbol,header=header)
        data <- data.frame(data_RAW[,period])
        data[,1] <- as.numeric(gsub(".TW","",data[,order]))
        names(data) <- c("code","name")

        if(i == 1) {
        temp <- data
        }else{
        temp <- merge(temp,data,by="code")
        }
        result <- temp
      
    }
    
    return(result)
}




