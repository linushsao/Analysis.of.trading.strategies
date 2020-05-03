
rm(list=ls())

# LIBRS <- c('quantmod','stringr','fTrading','xts','TTR')
# sapply(LIBRS,library,character.only=TRUE)

#custom library
source('/home/linus/Project/1_R/Analysis.of.trading.strategies/TollKit/custom.pkg.R')
source(m_env(name="custom.pkg.path", mode="r"))

#
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)

data.dir <- c( FISS="/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/foreign.investment.Sales.Summary/" )
#https://www.twse.com.tw/fund/TWT38U?response=csv&date=20090106
# data.url.head <- "https://www.twse.com.tw/exchangeReport/FMTQIK?response=csv&date="
data.url.head <- c( FISS="https://www.twse.com.tw/fund/TWT38U?response=csv&date=" )
data.url.tail <- c( FISS="" )
data.fake.header <- c( FISS="" )

# crawl.check <- ".crawl.check.csv"
extra.holiday.date <- c(paste("2020-01-",as.character(c(21:29)),sep=""),"2020-10-10","2018-12-31","2019-01-01","2018-08-31") #Chinese New Year

back.remain <- as.numeric(m_env(name="crawl.date.back.remain", mode="r"))
stop.date <- c( FISS=(as.Date(Sys.Date()) - back.remain) )
start.date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
crawl.delay.seconds <- m_env(name="crawl.delay.seconds",mode="r")
if( is.null(crawl.delay.seconds) || (crawl.delay.seconds  == "")) { 
    crawl.delay.seconds <- c(10:30) #default
    }
# Main Program <<

repeat {
#         browser()
        week.check <- weekdays(as.Date( start.date ))
        datestr <- format(start.date, "%Y%m%d")
        url <- m_paste(c(data.url.head ,datestr ,data.url.tail),op="")
        destfile <- m_paste(c(data.dir, as.character(start.date), ".csv"),op="")

        zero <- m_env(name="crawl.date", value=gsub(".csv","",destfile), mode="w")

        if (! file.exists(destfile) )  {                
                #check ignore if holiday <<
                if(start.date <= stop.date) break #till stop date
                
                if( as.character(start.date) %in% extra.holiday.date ) { next } #ignore holiday
                
                    if(!(week.check %in% c("Saturday","Sunday" ))){

                        tryit <- download.file(url, destfile, mode="wb", method="auto")
                        if(inherits(tryit, "try-error")){ 
                            next 
                            }else{
                            
                                sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                                m_msg(info=paste("_File download completely :", destfile, sep=""))
                                m_msg(info=paste("_Sleep seconds : ", sle.num, sep=""))
                                Sys.sleep(sle.num)
                            }
                        
                        }
                        
                }else{
                
                m_msg(info=paste("_Ignore processed file :", destfile, sep=""))
            }
                
        if(start.date == stop.date) { #tell task.MGR to ignore
            break
            }
        start.date <- as.Date( start.date ) - 1 
                
}
    

    
    




#format(Sys.time(), "%Y-%m-%d")  /   format(Sys.time(), "%Y-%m-%d")
# r = requests.post('https://www.twse.com.tw/exchangeReport/MI_INDEX?response=csv&date=' + datestr + '&type=ALL')
# data.url.head <- "https://www.twse.com.tw/exchangeReport/MI_INDEX?response=csv&date="
# data.url.tail <- "&type=ALL"
# url <- m_paste(c(data.url.head,datestr,data.url.tail),op="")
# destfile <- "test.csv"
# 
# download.file(url, destfile, mode="wb")
# 
# new <- as.Date("2020-03-23")
# new -1

