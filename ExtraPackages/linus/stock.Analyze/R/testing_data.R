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
    if( stock_name == "^GSPC") {
#     file_name_csv <-  gsub(" ","",paste(stock_name,".csv"))
# #    data_RAW <- getSymbols(stock_name,auto.assign=FALSE)
# #    write.zoo(data_RAW,file=file_name_csv,sep=",")
#     GSPC <- read.csv(file_name_csv,header=TRUE)
#     GSPC <- xts(GSPC[,-c(1)],order.by=as.Date(GSPC$Index))[period]
#     names(GSPC) <- c("Open","High","Low","Close","Volume","Adjusted")
#     return(GSPC)
        return(gspc())
        }else if( stock_name == "chap19.3.1") {
        return()
    }
  
    gspc <- function(stock_name) {
        file_name_csv <-  gsub(" ","",paste(stock_name,".csv"))
        #    data_RAW <- getSymbols(stock_name,auto.assign=FALSE)
        #    write.zoo(data_RAW,file=file_name_csv,sep=",")
        GSPC <- read.csv(file_name_csv,header=TRUE)
        GSPC <- xts(GSPC[,-c(1)],order.by=as.Date(GSPC$Index))[period]
        names(GSPC) <- c("Open","High","Low","Close","Volume","Adjusted")
        return(GSPC)
        }
}

#
# stop()
# 
# setwd("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/")
# stocks.name <- c("600000","600016","600018","600028","600048")
# stocks.code <- paste(stocks.name,"ss",sep=".")
# sapply(stocks.code,function(v) 
#             {
#             getSymbols(v,auto.assign=FALSE)
#             write(v,file=paste(v,"csv",sep="."))
#             })
            
            
            
