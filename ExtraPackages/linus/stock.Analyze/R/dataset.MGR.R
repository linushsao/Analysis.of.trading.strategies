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

dataset.MGR <- function(
                dataset.name=NULL, group=c("stock",'data'), request='conf', force.update=FALSE,
                file.extension=".csv", header=TRUE, to.exts=FALSE, online=FALSE, verbose=FALSE)
{

    #configure <<<
    if(length(group)==1) group <- c(group, 'data')
#     if(! is.null(col.name) ) col.name <- tolower(col.name)
    file.extension <- ".csv"
#     header.format <- c("Index","Open","High","Low","Close","Volume","Adjusted") 
#     names(header.format) <- tolower(header.format)
    env.name <- "dataset.MGR"
    env.name.file <- '~/env.conf.csv'
    if(file.exists(env.name.file))
    {
       env.config <- read.csv(env.name.file, header=TRUE, sep=',')
       rownames(env.config) <- env.config[,1]
       env.config <- env.config[,-c(1)]
    }else{ #default config
        env.config <- data.frame(
            data=c( "/home/linus/Project/9.Shared.Data/0_Global/Index/",
                    "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stock.price/",
                    "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/etf.price/"),
            list=c( "/home/linus/Project/1_R/Analysis.of.trading.strategies/world.wide.broad.index.csv",
                    "/home/linus/Project/1_R/Analysis.of.trading.strategies/all_codes.csv",
                    "/home/linus/Project/1_R/Analysis.of.trading.strategies/all.ETF.code.csv"),
            remix=c(    "/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/03_index/",
                        "/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/",
                        "/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/02_etf/"),
            ma=c(    "/home/linus/Project/0_Comprehensive.Research/02_Logarithmic.table/02_Index/",
                    "/home/linus/Project/0_Comprehensive.Research/02_Logarithmic.table/01_stock/",
                    "/home/linus/Project/0_Comprehensive.Research/02_Logarithmic.table/03_etf/"),
            raw=c(  "/home/linus/Project/0_Comprehensive.Research/00_raw.data/",
                    "/home/linus/Project/0_Comprehensive.Research/00_raw.data/",
                    "/home/linus/Project/0_Comprehensive.Research/00_raw.data/"),
            extension=c("",".TW",".TW"),
            string.leng=c(0, 4, 5),
            row.names=c('index', 'stock', 'etf')
        )
    }

    # main function
    main <- function() 
    {
#         browser()
        switch(request,
            info = {   z.file <- get.Inquire.data()
#                         names(z.file) <- header.format
                        z.index <- z.file[,c(1)] 
#                         if( !is.null(col.name) ) z.file <- z.file[ header.format[col.name] ]
                        if( to.exts ) z.file <- xts(z.file[,-c(1)], order.by=as.Date(z.index))
                    
                        result <- z.file
                        return(result)
                        },
            conf =  { return(as.character(data.path(group))) },
            all_conf =  { return(env.config) }
        )
    }        
                
            # sub function <<<
            msg<- function(v){
                if( verbose ){
                    m_msg(v)
                    }
                }
                
            data.path <- function(group){
                result <- env.config[group[1], group[2]]
                return(result)
                    }
            
            get.Inquire.data <- function(){
                    if(! is.null(dataset.name) )
                    {
                        file_name_csv <-  as.character(paste0(data.path(group), dataset.name, file.extension))
                            if(!file.exists(file_name_csv) || force.update) 
                            {
                                tmp.data <- getSymbols(dataset.name, auto.assign=FALSE)
                                write.zoo(tmp.data, file=file_name_csv, sep = ",")
                            }
                        }else{
                        file_name_csv <-  as.character(data.path(group))
                    }
                    
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
# a <- (dataset.MGR(group="index.yahoo", request="path"))
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
            
            
            
