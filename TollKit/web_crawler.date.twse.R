
LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2')
sapply(LIBRS,library,character.only=TRUE)
setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")
#

data.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/foreign.investment.Sales.Summary/"
setwd(data.dir)

#https://www.twse.com.tw/fund/TWT38U?response=csv&date=20090106
# url.head <- "https://www.twse.com.tw/exchangeReport/FMTQIK?response=csv&date="
url.head <- "https://www.twse.com.tw/fund/TWT38U?response=csv&date="
url.tail <- ""
crawl.check <- ".crawl.check.csv"
# stop.date <- "2006-01-01"
stop.date <- m_env(name="stock.data.earliest",mode="r")
extra.holiday.date <- c(paste("2020-01-",as.character(c(21:29)),sep=""),"2020-10-10","2018-12-31","2019-01-01","2018-08-31") #Chinese New Year
# stop.time.everyday <- ""
fake.header <- ""
stop.date <- as.Date("2005-01-01")
start.date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))

# Main Program <<
m_regist.task.status(TRUE)

repeat {

        week.check <- weekdays(as.Date( start.date ))
        datestr <- format(start.date, "%Y%m%d")
        url <- m_paste(c(url.head ,datestr ,url.tail),op="")
        destfile <- paste(format(start.date, "%Y-%m-%d"),"csv",sep=".")

        zero <- env(name="crawl.date", value=gsub(".csv","",destfile), mode="w")

        if (! file.exists(destfile) )  {                
                #check ignore if holiday <<
                if((datestr <= stop.date)) break #till stop date
                if( as.character(start.date) %in% extra.holiday.date ) { next } #ignore holiday
                
                    if(!(week.check %in% c("Saturday","Sunday" ))){

                        crawl.delay.seconds <- env(name="crawl.delay.seconds",mode="r")
                        if( is.null(crawl.delay.seconds) || (crawl.delay.seconds  == "")) { 
                            crawl.delay.seconds <- c(10:30) #default
                        }
                        tryit <- crawl.file(destfile ,url ,sleep=c(10:30) ,extra.holiday.date )
                    if(inherits(tryit, "try-error")){ next }
                    }
                }else{
                print(paste("_File download completely ",destfile,sep=":"))
                }
                
        if(start.date == stop.date) { #tell task.MGR to ignore
            m_regist.task.status(FALSE)
            break
            }
        start.date <- as.Date( start.date ) - 1 
                
}
    

    
    




#format(Sys.time(), "%Y-%m-%d")  /   format(Sys.time(), "%Y-%m-%d")
# r = requests.post('https://www.twse.com.tw/exchangeReport/MI_INDEX?response=csv&date=' + datestr + '&type=ALL')
# url.head <- "https://www.twse.com.tw/exchangeReport/MI_INDEX?response=csv&date="
# url.tail <- "&type=ALL"
# url <- m_paste(c(url.head,datestr,url.tail),op="")
# destfile <- "test.csv"
# 
# download.file(url, destfile, mode="wb")
# 
# new <- as.Date("2020-03-23")
# new -1

