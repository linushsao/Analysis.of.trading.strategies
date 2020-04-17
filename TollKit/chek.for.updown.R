
rm(list=ls())

# load library
LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2','PerformanceAnalytics','stats')
sapply(LIBRS,library,character.only=TRUE)

setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")

# basic configure
data.type.name <- c("global.index","stock","etf")

list.file <- c("/home/linus/Project/9.Shared.Data/0_Global/world.wide.broad.index.csv","/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/all_codes.csv","/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/all.ETF.code.csv")
names(list.file) <- data.type.name

# path of raw data storged and from
data.dir <- c("/home/linus/Project/9.Shared.Data/0_Global/Index/","/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/Stock/","/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/etf.price/")
names(data.dir) <- data.type.name

# path of cooked data storged and to
target.dir <- c("/home/linus/Project/0_Comprehensive.Research/02_ma.for.ret/01_index/","/home/linus/Project/0_Comprehensive.Research/02_ma.for.ret/02_stock/","/home/linus/Project/0_Comprehensive.Research/02_ma.for.ret/03_etf/")
names(target.dir) <- data.type.name

data.extension <- c("",".TW",".TW")
names(data.extension) <- data.type.name

#
data.type <- "etf"

file.extension <- ".csv"
ma.VALUE <- c(5, 20, 60, 120, 252)
# ma.value <- 5 # N days moveable average line
#
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)
# check if file processed
ignore.file <- m_paste(c(".ignore.check.for.up_down.of.", data.type, ".csv"),op="")

main <- function() {
#     browser()
    if(! file.exists(ignore.file) ) { 
        processed.list <- c("0")
        write.table(processed.list, ignore.file) 
    }
    #read all code of taiwan stocks
    stock.code.list <- read.csv(list.file[data.type], header=TRUE, sep=",")
    stock.code <- as.character(stock.code.list[,1])
    stock.cname <- as.character(stock.code.list[,2])
    for (i in 1:length(stock.code))  { 

            processed.list <- read.table(ignore.file)
            processed.code <- as.numeric(processed.list)
            if(i > processed.code ) {
            
                write.table(as.character(i),ignore.file) 
                if( data.type != 'global.index' ) {
                    check.stock.code <- m_check.code(stock.code[i])
                    }else{
                    check.stock.code <- stock.code[i]
                    }
                target.stock <- m_paste(c(data.dir[data.type],  check.stock.code, data.extension[data.type], file.extension), op="")
                destfile <- m_paste(c(target.dir[data.type], check.stock.code, data.extension[data.type], file.extension), op="")
                if(file.exists(target.stock)) {
                
                        stock.ma <- data.frame()

                        stock.data <- read.csv(target.stock,header=TRUE,sep=",")
                        stock.data.xts <- xts(stock.data[,-c(1)], order.by=as.Date(stock.data$Index))
                        stock.close <- Cl(stock.data.xts)
                        stock.rate <- na.omit(ROC(stock.close))
                        names(stock.rate) <- "Ret"

                        for(ii in 1:length(ma.VALUE)) {

                            ma.value <- ma.VALUE[ii]
                            col.name <- m_paste(c(check.stock.code, "_ratio.ma", ma.value),op="")
                            tmp.stock.ma <- na.omit(runMean(stock.rate, n=ma.value, cumulative=FALSE))
                            names(tmp.stock.ma) <- col.name
    #                         browser()
                            if(ii == 1) {
                                stock.ma <- tmp.stock.ma
                            }else{
                                stock.ma <- merge(stock.ma, tmp.stock.ma)
                            }

                        }
                        stock.ma <- merge(stock.rate, stock.ma)
                        write.zoo(stock.ma, file=destfile,sep=",")
                        print(m_paste(c("_Finished File ", as.character(i), ": ",check.stock.code ),op=""))
                }
                
                }else{
                print(m_paste(c("_Checked File ",as.character(i),"and Jump to Next File, Ignore Procssed File: ",m_check.code(stock.code[i])),op=""))
                }
                        
    }
    
}
 main()


