#' A for testing Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))
# rm(list = ls())
# 
# LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2')
# sapply(LIBRS,library,character.only=TRUE)
# 
# #local devel fubction
# setwd("/home/linus/ProjectStock/ExtraPackages/linus/stock.Analyze")
# roxygenize()
# library("stock.Analyze")
# 
# setwd("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/")
dataset.MGR <- function(dataset.name,verbose=FALSE){

# sub function <<<
    msg<- function(v){
        if( verbose ){
            msg(v)
            }
        }
        
    if( dataset.name == "^GSPC") {
            #GSPC
        gspc <- function(){
                file_name_csv <-  gsub(" ","",paste(dataset.name,".csv"))
                GSPC <- read.csv(file_name_csv,header=TRUE)
                GSPC <- xts(GSPC[,-c(1)],order.by=as.Date(GSPC$Index))
                names(GSPC) <- c("Open","High","Low","Close","Volume","Adjusted")
                return(GSPC)
                }
                return(gspc())
                
         }else if( dataset.name == "chap19.3.1") {
         
         chap19.3.1 <- function(){  
                stocks.name <- c("600000","600016","600018","600028","600048")
                stocks.code <- paste(stocks.name,"ss",sep=".")

                for(i in 1:length(stocks.code)) {
                        v <- stocks.code[i]
                        targetkFile <- paste(v,"csv",sep=".")
                        if(! file.exists(targetkFile) ) {
                            zero <- xts()
                            zero <- getSymbols(v,auto.assign=FALSE)
                            zero <- na.omit(xts(zero,order.by=as.Date(index(zero))))
                            write.zoo(zero, file=targetkFile,sep=",")
                            rm(zero)
                            msg(c("DOWNLOADED FILE ",targetkFile))
                            }
                        data.singal <- xts()
                        data.singal <- read.csv(targetkFile ,header=TRUE)
                        data.singal <- na.omit(xts(data.singal[c(2:7)],order.by=as.Date(data.singal$Index)))
                    # data.singal <- data.singal["2009:2013"]
                        msg(c("PROCESSING FILE ",targetkFile))
    #                       names(data.singal) <- c("Open","High","Low","Close","Volume","Adjusted")
                        data.singal.Cl <- Cl(data.singal)
                        if ( i != 1 ) {
                            data.all <- merge(data.all, data.singal.Cl)
                            }else{
                            data.all <-  data.singal.Cl
                            }
                        msg(c("MERGED FILE ",i,targetkFile))
                        }
                    return(data.all)
                }
            return(chap19.3.1())
            
    }
}

#
# stop()
# rm(list=ls())
# #
# setwd("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/")
# a <- dataset.MGR("chap19.3.1")
# # 
# stocks.name <- c("600000","600016","600018","600028","600048")
# stocks.code <- paste(stocks.name,"ss",sep=".")
# zero.xts <- xts()
# 
# for(i in 1:length(stocks.code)) {
#         v <- stocks.code[i]
#         zero.xts <- getSymbols(v,auto.assign=FALSE)
#         zero.xts <- xts(zero.xts,order.by=as.Date(index(zero.xts)))
#         write.zoo(zero.xts, file=paste(v,"csv",sep="."),sep=",")
#        # write.zoo(filter.list,file=file_name,sep=",")
#        rm(c(zero,xts))
#         }
            
            
            
