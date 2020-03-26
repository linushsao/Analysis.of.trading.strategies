
LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2')
sapply(LIBRS,library,character.only=TRUE)
setwd("/home/linus/ProjectStock/ExtraPackages/linus/stock.Analyze")
roxygenize()
library("stock.Analyze")
#

data.dir <- "/home/linus/ProjectStock/DATA/stock.tw/Market.transaction.information/"
setwd(data.dir)

#
url.head <- "https://www.twse.com.tw/exchangeReport/FMTQIK?response=csv&date="
url.tail <- ""
crawl.check <- ".crawl.check.csv"
# stop.date <- "2006-01-01"
stop.date <- env(name="stock.data.earliest",mode="r")
extra.holiday.date <- c(paste("2020-01-",as.character(c(21:29)),sep=""),"2020-10-10","2018-12-31","2019-01-01","2018-08-31") #Chinese New Year
# stop.time.everyday <- ""
fake.header <- ""
# crawl.delay.seconds <- env(name="crawl.delay.seconds",mode="r")
# if( is.null(crawl.delay.seconds) || (crawl.delay.seconds  == "")) { 
#     crawl.delay.seconds <- c(10:30) #default
# }
# raw.date <- env(name="crawl.date", mode="r")
# if ( is.null(raw.date) ) {
# grep from today - 1
#     }else{
#     raw.date <- as.Date(raw.date)
#     }
raw.date <- as.Date(Sys.time()) + 1 # grep from today+1
#     }else{
#     raw.date <- as.Date(date.crawl.stop) + 1
#     }

# SubFunction <<

# crawl.file <- function(url,destfile,sleep=c(10:30),check=TRUE) {
# 
#         print(paste("_Processing Date ",Sys.time(),sep=":"))
#         print(paste("_crawling ",destfile,sep=":"))
#         download.file(url, destfile, mode="wb", method="auto")
#         
#         sle.num  <- sample(sleep,size=1,replace=TRUE)
#         print(paste("_Sleep.Second ",sle.num,sep=":"))
#         Sys.sleep(sle.num)
# }

# SubFunction >>


# Main Program <<

repeat {

        week.check <- weekdays(as.Date( raw.date ))
        datestr <- format(raw.date, "%Y%m%d")
        url <- m_paste(c(url.head ,datestr ,url.tail),op="")
        destfile <- paste(format(raw.date, "%Y-%m-%d"),"csv",sep=".")

        raw.date <- as.Date( raw.date ) - 1 
        #check if file exist,but size zero(download fail)
#         if( (! file.exists(destfile)) || (file.exists(destfile) && (file.size(destfile) ==              0)) ) {
        zero <- env(name="crawl.date", value=gsub(".csv","",destfile), mode="w")

        if (! file.exists(destfile) )  {                
                #check ignore if holiday <<
                if((datestr <= stop.date)) break #till stop date
                if( as.character(raw.date) %in% extra.holiday.date ) { next } #ignore holiday
                
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

