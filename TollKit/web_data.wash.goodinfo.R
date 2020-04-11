rm(list=ls())

LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2','rvest')
sapply(LIBRS,library,character.only=TRUE)

setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library(roxygen2)
roxygenize()
library("stock.Analyze")
#
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)
#
file.extension <- ".csv"
source.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/goodinfo.tw/01_Company.Basic.Information/01_raw.data/"
target.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/goodinfo.tw/01_Company.Basic.Information/02_processed.data/"

rowdata.count <- 2    
# Main Program <<

#read all code of taiwan stocks
all.list <- list.files(source.dir)
all.list.sizenonzero <- all.list[(file.size(paste(source.dir, all.list, sep="")) > 7)]

list.l <- length(all.list.sizenonzero)

for (i in 1:list.l)  { 

    symbol <- m_check.code(all.list.sizenonzero[i])
    target.stock <- paste(source.dir, symbol, sep="")
    raw.data <- read.csv(target.stock, header=TRUE, sep=",")[-c(1),-c(1)]
    raw.data.format <- matrix(raw.data,ncol=rowdata.count,byrow=TRUE)
    stop()
                #compare if current stock code was downloaded before

                if(! file.exists(destfile) ) {
                    z.null <- page2csv(page.url=url , page.encoding=encoding, export.file=destfile, css.selector=css.header)
                        if(inherits(z.null, "try-error")){ 
                            next 
                            }else{
                                print(paste("_File download completely ",symbol,sep=":"))
                                sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                                print(paste("_Sleep.Second ",sle.num,sep=":"))
                                Sys.sleep(sle.num)
                            }
                        }else{
                        print(m_paste(c("_Checked File ",as.character(i),"and Jump to Next File, Ignore Procssed File: ",destfile),op=""))
                    }
#                     }
}

