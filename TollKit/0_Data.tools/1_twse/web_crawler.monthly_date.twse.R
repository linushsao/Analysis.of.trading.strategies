
rm(list=ls())

# loading library
# LIBRS <- c('quantmod','stringr','fTrading','xts','TTR')
# sapply(LIBRS,library,character.only=TRUE)
library(mondate)
#custom library
source('/home/linus/Project/1_R/Analysis.of.trading.strategies/TollKit/custom.pkg.R')
source(m_env(name="custom.pkg.path", mode="r"))

# basic configure
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)

data.dir <- c( MTI="/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/Market.transaction.information/" )
# https://www.twse.com.tw/exchangeReport/FMTQIK?response=csv&date=20200401
# data.url.head <- "https://www.twse.com.tw/exchangeReport/FMTQIK?response=csv&date="
data.url.head <- c( MTI="https://www.twse.com.tw/exchangeReport/FMTQIK?response=csv&date=" )
data.url.tail <- c( MTI="" )
data.fake.header <- c( MTI="" )

back.remain <- as.numeric(m_env(name="crawl.date.back.remain", mode="r"))
stop.date <-  c( MTI='2004-01-01' )
start.date.raw <- c( MTI=as.Date(format(Sys.Date(), "%Y-%m-%d")) )
crawl.delay.seconds <- m_env(name="crawl.delay.seconds",mode="r")
if( is.null(crawl.delay.seconds) || (crawl.delay.seconds  == "")) { 
    crawl.delay.seconds <- c(10:30) #default
    }
    
    
# Main Program <<

repeat {
#         browser()
        start.date.raw <- format((as.mondate(start.date.raw) - 1), "%Y-%m-%d")
        start.date <- as.Date(format((as.mondate(start.date.raw) - 1), "%Y-%m-%d"))
        datestr <- paste(format(start.date, "%Y%m"), '01', sep='')
        url <- m_paste(c(data.url.head ,datestr ,data.url.tail), op="")
        destfile <- m_paste(c(data.dir, as.character(start.date), ".csv"),op="")

        if(start.date <= stop.date) break #till stop date
        
        if (! file.exists(destfile) )  {                
                
                tryit <- download.file(url, destfile, mode="wb", method="auto")
                if(inherits(tryit, "try-error")){ 
                    next 
                    }else{
                    
                        sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                        m_msg(info=paste("_File download completely :", destfile, sep=""))
                        m_msg(info=paste("_Sleep seconds : ", sle.num, sep=""))
                        Sys.sleep(sle.num)
                    }
            }else{
            
            m_msg(info=paste("_Ignore processed file :", destfile, sep=""))
            }
}
    
# Main Program >>
