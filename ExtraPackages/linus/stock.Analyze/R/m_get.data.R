#' A m_get.data Function
#'
#' This function allows you to get data from list.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_get.data <- function(list,header=TRUE,data.col=1) {
    #read all code for download
    name <- read.csv(list, header=TRUE, sep=",")[,data.col]
    name <- as.character(name)
    
    for(i in 1:length(name)) {
        name[i]-> symbol
        tryit <- try(getSymbols(symbol,auto.assign=FALSE))
        if(inherits(tryit, "try-error")){
            i <- i+1
        } else {
            temp <- xts()
            temp <- getSymbols(symbol,auto.assign=FALSE)

            file_name <- paste(symbol,"csv", sep = ".") 	
            write.zoo(temp, file = file_name, sep = ",")
        }
    }
}
