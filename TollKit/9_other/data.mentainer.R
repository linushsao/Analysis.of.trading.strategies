
rm(list=ls())

LIBRS <- c('quantmod','stringr')
sapply(LIBRS,library,character.only=TRUE)

#local devel fubction
setwd( "/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/" )
library(roxygen2)
roxygenize()
library("stock.Analyze")


# setwd("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/")
source.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/stock.price/1_raw.data/2_PrePRocessing/"
pre.processing <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/stock.price/1_raw.data/2_PrePRocessing/"
processed.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/stock.price/1_raw.data/3_processedDATA/"
single.stock.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/stock.price/1_raw.data/4_SpliteIntoSingleSTOCK/"

colnames.list <- c("Date","NUM_SharesTraded","NUM_Transactions","Turnover","open",   "high", "low", "close", "Up_Dn",    "Price_Difference", "Fin.Revealed.BidPrice","Fin.Reveal.PurchaseAmount","Fin.Revealed.SellingPrice","Fin.Revealed.SellingVolume","PEratio")

# action = file.transfer / file.spliter

data.mentainer <- function(action, filter,to.dir) {

    switch(action,
        #import www.twse.com.tw raw data
        file.transfer = {
                all.list <- list.files(source.dir)
                setwd(source.dir)
                
                file.transfer(all.list, source.dir, processed.dir,filter=filter)
                },
        #splite data into single stock
        file.spliter = {
                all.list <- sort(list.files(processed.dir))
                #                   靡ㄩ腹	 靡ㄩ嘿	  Θユ计	        Θユ掸计	       Θユ肂	  秨絃基	程蔼基	程基	Μ絃基	害禴(+/-)	害禴基畉	        程处ボ禦基	        程处ボ禦秖	            程处ボ芥基	              程处ボ芥秖	             セ痲ゑ
#                 colnames.list <- c("Date","NUM_SharesTraded","NUM_Transactions","Turnover","open",   "high", "low", "close", "Up_Dn",    "Price_Difference", "Fin.Revealed.BidPrice","Fin.Reveal.PurchaseAmount","Fin.Revealed.SellingPrice","Fin.Revealed.SellingVolume","PEratio")
                splite.to.single.stock(all.list, processed.dir, single.stock.dir, col.list=colnames.list)
                },
        rebuild.stockprice = {
                all.list <- sort(list.files(processed.dir))
                write.into.singal.stock(stock.code=filter, source.path=processed.dir, target.path=to.dir, col.list=NULL , filter=filter)
                }
        )
}   


    # sub function <<<
    
    msg<- function(v){
        verbose <- as.logical(m_env(name="if.debug",mode="r"))
        if( verbose ){
            m_msg(v)
            }
        }
    
    file.transfer <- function(list, source.path, target.path, filter=NULL) {

        for( i in 1:length(list) ) {

            from.file.name <- list[i]
            data <- read.csv(from.file.name, header=TRUE, sep=",")
                num <-match(filter,data[,1])
                data.1 <- data[-c(1:num-1),]
                data.1[,1] <- as.character(data.1[,1] )
                data.1[,1] <- gsub("=", "", data.1[,1] )
                title <- sapply( data.1[1,],function(x) {as.character(x) } )
                data.1 <- data.1[-c(1),]
                names(data.1) <- title
                rownames(data.1) <- NULL
                to.file.name <- paste(target.path, from.file.name, sep="")
            write.csv(data.1,file=to.file.name)
            msg(c("Finished : ",i,from.file.name))
            }
    }

    splite.to.single.stock <- function(list, source.path, target.path, col.list=NULL , filter=NULL) {

        for( i in 1:length(list) ) {

            write.in.date <- gsub(".csv","",list[i]) #filename as date
            from.file.name <- paste(source.path, list[i], sep="")
            data <- read.csv(from.file.name, header=TRUE, sep=",")
            L <- length(data[,1])
                stock.code <- as.character(data[,2])
                stock.cname <- as.character(data[,3])
                stock.date <- gsub(".csv","",list[i])
                data.1 <- data[,-c(1:3)]
                
                for(i in 1:L) { #amount of all stocks
                    stock.code[i] <- m_check.code(stock.code[i])
                    stock.name.cvs <- m_paste(c(target.path, stock.code[i], ".", stock.cname[i], ".csv"), op="")
                    
                    if (! file.exists(stock.name.cvs) ) { #if not existed,create it
                        z.temp <- as.data.frame(t(rep(NA,length(col.list))))
                        names(z.temp) <- col.list
                        write.csv(z.temp, file=stock.name.cvs) 
                        }
                   
                        z.file <- read.csv(stock.name.cvs,header=TRUE,sep=",")[,-c(1)]
                        z.in.file.date <- z.file[length(z.file[,1]),1]
                        z.in.file.date <- as.Date(ifelse( (is.null(z.in.file.date) || is.na(z.in.file.date)), as.Date(write.in.date)-1, as.Date(z.in.file.date)))
                        if (z.in.file.date < write.in.date) {
                            z.collect <- cbind(stock.date,data.1[i,]) #data will be merged into stock data

                            names(z.collect) <- col.list
                            z.file <- na.omit(rbind(z.file, z.collect))
                            write.csv(z.file, file=stock.name.cvs)
                            
                            z.info <- m_paste(c("Writing File : ",stock.date,stock.code[i],stock.cname[i]) ,op=" ")
                            }else{
                            z.info <- m_paste(c("Ignore Finished File : ",stock.date,stock.code[i],stock.cname[i]) ,op=" ")
                                }
                            msg(z.info)
                    } 

            }
    
    }
    

    rebuild.single.stock <- function(list, source.path, target.path, col.list=NULL , filter=NULL) {

        for( i in 1:length(list) ) {    #filter as single.stock for rebuilded

            write.in.date <- gsub(".csv","",list[i]) #filename as date
            from.file.name <- paste(source.path, list[i], sep="")
            data <- read.csv(from.file.name, header=TRUE, sep=",")
            L <- length(data[,1])
                stock.code <- as.character(data[,2])
                stock.cname <- as.character(data[,3])
                stock.date <- gsub(".csv","",list[i])
                data.1 <- data[,-c(1:3)]
                
                for(i in 1:L) { #amount of all stocks
                    stock.code[i] <- m_check.code(stock.code[i])
                    stock.name.cvs <- m_paste(c(target.path, stock.code[i], ".", stock.cname[i], ".csv"), op="")
                    
                    if (! file.exists(stock.name.cvs) ) { #if not existed,create it
                        z.temp <- as.data.frame(t(rep(NA,length(col.list))))
                        names(z.temp) <- col.list
                        write.csv(z.temp, file=stock.name.cvs) 
                        }
                   
                        z.file <- read.csv(stock.name.cvs,header=TRUE,sep=",")[,-c(1)]
                        z.in.file.date <- z.file[length(z.file[,1]),1]
                        z.in.file.date <- as.Date(ifelse( (is.null(z.in.file.date) || is.na(z.in.file.date)), as.Date(write.in.date)-1, as.Date(z.in.file.date)))
                        if (z.in.file.date < write.in.date) {
                            z.collect <- cbind(stock.date,data.1[i,]) #data will be merged into stock data

                            names(z.collect) <- col.list
                            z.file <- na.omit(rbind(z.file, z.collect))
                            write.csv(z.file, file=stock.name.cvs)
                            
                            z.info <- m_paste(c("Writing File : ",stock.date,stock.code[i],stock.cname[i]) ,op=" ")
                            }else{
                            z.info <- m_paste(c("Ignore Finished File : ",stock.date,stock.code[i],stock.cname[i]) ,op=" ")
                                }
                            msg(z.info)
                    } 

            }
    
    }
    
    writeinto.singal.stock <- function(stock.code, source.path, target.path, col.list=NULL , filter=NULL) {
                    
                    stock.name.cvs <- m_paste(c(target.path, stock.code[i], ".", stock.cname[i], ".csv"), op="")
                    
                    if (! file.exists(stock.name.cvs) ) { #if not existed,create it
                        z.temp <- as.data.frame(t(rep(NA,length(col.list))))
                        names(z.temp) <- col.list
                        write.csv(z.temp, file=stock.name.cvs) 
                        }
                   
                        z.file <- read.csv(stock.name.cvs,header=TRUE,sep=",")[,-c(1)]
                        z.in.file.date <- z.file[length(z.file[,1]),1]
                        z.in.file.date <- as.Date(ifelse( (is.null(z.in.file.date) || is.na(z.in.file.date)), as.Date(write.in.date)-1, as.Date(z.in.file.date)))
                        if (z.in.file.date < write.in.date) {
                            z.collect <- cbind(stock.date,data.1[i,]) #data will be merged into stock data

                            names(z.collect) <- col.list
                            z.file <- na.omit(rbind(z.file, z.collect))
                            write.csv(z.file, file=stock.name.cvs)
                            
                            z.info <- m_paste(c("Writing File : ",stock.date,stock.code[i],stock.cname[i]) ,op=" ")
                            }else{
                            z.info <- m_paste(c("Ignore Finished File : ",stock.date,stock.code[i],stock.cname[i]) ,op=" ")
                                }
                                
                            return(z.info)
    }

    # sub function <<<
    
## engage
mission <- get.conf("data.mentainer.mission")
data.mentainer(action=mission)
    

# 
# url <- "https://www.twse.com.tw/zh/page/ETF/list.html"
# css.code <- "td , th"
# a <- page2table(page.url=url, css.selector=css.code)
# a.table <- as.data.frame(a[[2]])
# a.table[,2] <- as.character(a.table[,2])
# to.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/'
# write.csv(a.table,file=paste(to.path, "all.ETF.csv",sep=""))


