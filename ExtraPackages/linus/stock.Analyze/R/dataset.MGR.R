#' A dataset.MGR Function
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

dataset.MGR <- function(dataset.name=FALSE, col.name=NULL, group="stock.yahoo", mode="data", file.extension=".csv", header=TRUE, to.exts=FALSE, online=FALSE, verbose=FALSE){
 
    #configure <<<
    if(! is.null(col.name) ) col.name <- tolower(col.name)
    file.extension <- ".csv"
    header.format <- c("Index","Open","High","Low","Close","Volume","Adjusted") 
    names(header.format) <- tolower(header.format)
    env.name <- "dataset.MGR"
#     data.dir <- list(
#             stock.yahoo = c("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/Stock/") ,
#             index.yahoo = c("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/Index/") ,
#             stock.twse  = c("/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/stock.price/1_raw.data/4_SpliteIntoSingleSTOCK/")
#             )
    #configure >>>
    
    # main function
    main <- function() {
#         browser()
        switch(mode,
            data = {    z.file <- get.Inquire.data()
                        names(z.file) <- header.format
                        z.index <- z.file[,c(1)] 
                        if( !is.null(col.name) ) {

                            z.file <- z.file[ header.format[col.name] ]
                            }else{
                            
                            z.file <- z.file[,-c(1)]
                            }
            #             browser()                  
                        if( to.exts ) z.file <- xts(z.file, order.by=as.Date(z.index))
                    
                        result <- z.file
                        return(result)
                        },
            path =  { return(data.path(group)) }
        )
    }
                
            # sub function <<<
            msg<- function(v){
                if( verbose ){
                    m_msg(v)
                    }
                }
                
            data.path <- function(group){
#                             browser()
#                             result <- data.dir[[group]]
                            result <- get.conf(name=group,env.name=env.name)
#                             path <- switch(group,
#                                 stock.yahoo = data.dir[["stock.yahoo"]] ,
#                                 stock.twse = data.dir[["stock.twse"]] 
#                                 )
                            return(result)
                    }
            
            get.Inquire.data <- function(){
                    file_name_csv <-  m_paste(c(data.path(group), dataset.name, file.extension), op="")
                    z.file <- read.csv(file_name_csv, header=header, sep=",")
                    #z.file <- xts(z.file[,-c(1)],order.by=as.Date(z.file$Index))

                    return(z.file)
                }
            # sub function >>>
            
    ## execute main functrion
    main()

}
# 
# 
# stop()
# 
# rm(list=ls())
# # #
# # setwd("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/")
# a <- (dataset.MGR("^GSPC",group="index.yahoo"))
# head(a)
# a <- (dataset.MGR("^GSPC",col.name="close",group="index.yahoo"))
# head(a)
# a <- (dataset.MGR(group="index.yahoo", mode="path"))
# a

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
            
            
            
