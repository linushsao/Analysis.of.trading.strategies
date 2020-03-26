#' A for testing Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

testing_data <- function(stock_name="^GSPC",period="2014-01-01/2015-04-30"){
#    stock_name="^GSPC"
#   period="2014-01-01/2015-04-30"
    file_name_csv <-  gsub(" ","",paste(stock_name,".csv"))
#    data_RAW <- getSymbols(stock_name,auto.assign=FALSE)
#    write.zoo(data_RAW,file=file_name_csv,sep=",")
    GSPC <- read.csv(file_name_csv,header=TRUE)
    GSPC <- xts(GSPC[,-c(1)],order.by=as.Date(GSPC$Index))[period]
    names(GSPC) <- c("Open","High","Low","Close","Volume","Adjusted")
    return(GSPC)
}

