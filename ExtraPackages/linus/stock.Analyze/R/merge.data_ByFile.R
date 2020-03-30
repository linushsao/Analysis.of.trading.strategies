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

#testing
#  stock.selected_Files <- c("2019-01-01::2019-04-30_ANALYZE","2019-08-01::2019-12-31_ANALYZE","2019_ANALYZE","2018_ANALYZE","2017_ANALYZE")
#  stock.selected.Files_Extension <- ".csv"

#讀入所有股票的編號
#  name <- gsub(" ","",paste(stock.selected_Files,stock.selected.Files_Extension,sep=""))
#  
#  dbg(length(KK_RAW[,1]))
#  KK_RAW <- merge.data_ByFile(name,period=2:3,order=1)




