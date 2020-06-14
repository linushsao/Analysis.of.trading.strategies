
rm(list=ls())

#custom library
library('roxygen2')
setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")

#
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)

twse.df <- data.frame(
    data.dir=c( "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/foreign.investment.Sales.Summary/1_raw.file/" ,
                     '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/foreign.investment.Sales.Summary/4_raw.file.day/'),
    data.url.head=c( "https://www.twse.com.tw/fund/TWT38U?response=csv&date=",
                          'https://www.twse.com.tw/fund/BFI82U?response=csv&dayDate=' ),
    data.url.tail=c( "", '&type=day'),
    data.fake.header=c( "", '' ),
    row.names=c('F.I.Summary', 'F.I.Day.Summary')
)

back.remain <- as.numeric(get.conf(name="crawl.date.back.remain"))
stop.date <- as.Date(Sys.time()) - back.remain
crawl.delay.seconds <- m_env(name="crawl.delay.seconds",mode="r")
if( is.null(crawl.delay.seconds) || (crawl.delay.seconds  == "")) { 
    crawl.delay.seconds <- c(10:30) #default
    }
# Main Program <<

type.name <- rownames(twse.df)
for(type.id in type.name)
{
    start.date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
    repeat {
            week.check <- weekdays(as.Date( start.date ))
            datestr <- format(start.date, "%Y%m%d")
            url <- m_paste(c(twse.df[type.id, 'data.url.head'] ,datestr ,twse.df[type.id, 'data.url.tail']),op="")
            destfile <- m_paste(c(twse.df[type.id, 'data.dir'], as.character(start.date), ".csv"),op="")

            zero <- m_env(name="crawl.date", value=gsub(".csv","",destfile), mode="w")

            if (! file.exists(destfile) )  {                
                    #check ignore if holiday <<
                    if(start.date <= stop.date) break #till stop date
                        if(!(week.check %in% c("Saturday","Sunday" ))){

                            tryit <- download.file(url, destfile, mode="wb", method="auto")
                            if(inherits(tryit, "try-error")){ 
                                next 
                                }else{
                                    sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                                    m_msg(info=paste(type.id, "_File download completely :", destfile, sep=""))
                                    m_msg(info=paste("_Sleep seconds : ", sle.num, sep=""))
                                    Sys.sleep(sle.num)
                                }
                            }
                    }else{
                    m_msg(info=paste(type.id, "_Ignore processed file :", destfile, sep=""))
                }
            if(start.date == stop.date) { #tell task.MGR to ignore
                break
                }
            start.date <- as.Date( start.date ) - 1 
    }
}    


