rm(list=ls())

#Basic Configure <<
LIBRS <- c('quantmod','stringr','fTrading','xts','TTR')
sapply(LIBRS,library,character.only=TRUE)
setwd("/home/linus/ProjectStock/ExtraPackages/linus/stock.Analyze")
library('roxygen2')
roxygenize()
library("stock.Analyze")
data.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stock.price/"
setwd(data.dir)
#Basic Configure >>
#Param Configure <<
url.head <- "https://query1.finance.yahoo.com/v7/finance/download/"
url.tail <- "?period1=1199059200&period2=1585267200&interval=1d&events=history"
crawl.check <- ".crawl.check.csv"
extension <- ".TW"
file.extension <- ".csv"
crawl.delay.seconds <- c(5:10) #default
fake.header <- ""
stock.code.list.path <- m_paste(c(getwd(),"0_Info","all.codes.tw.csv"),op="/")
#Param Configure >>
ignore.file <- ".ignore.stock.file.csv"
if(! file.exists(ignore.file) ) { 
    processed.list <- c("0")
    write.table(processed.list, ignore.file) 
}

#read all code of taiwan stocks
stock.code.list <- read.csv(stock.code.list.path,header=FALSE,sep=",")
list.code <- as.character(stock.code.list[,1])

for (i in 1:length(list.code))  { 

    check.code <- function(x) {
            string.l <- nchar(x)
            if(string.l  <  4) {
                x <- m_paste(c(rep("0",(4-string.l)),x),op="")
                }
            return(x)
        }
        
        processed.list <- read.table(ignore.file)
        processed.code <- as.numeric(processed.list)
        
        if(i > processed.code ) {
        
                 write.table(as.character(i),ignore.file) 

                #add .TW prepare to download
                target.stock <- paste(check.code(list.code[i]),extension,sep="")
                url <- m_paste(c(url.head ,target.stock ,url.tail),op="")
                destfile <- paste(target.stock, file.extension,sep="")
                if.exist.check <- paste(getwd(),destfile,sep="/")

                #compare if current stock code was downloaded before
                    if(! file.exists(if.exist.check) ) {

                        tryit <- crawl.file(destfile ,url )
                            if(inherits(tryit, "try-error")){ 
                                next 
                                }else{
                                    sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                                    print(paste("_Sleep.Second ",sle.num,sep=":"))
                                    Sys.sleep(sle.num)
                                    print(paste("_File download completely ",destfile,sep=":"))
                                }
                            }else{
                            print(m_paste(c("_Checked File ",as.character(i),"and Jump to Next File, Ignore Procssed File: ",destfile),op=""))
                        }
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

