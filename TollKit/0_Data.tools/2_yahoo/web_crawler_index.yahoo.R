rm(list=ls())

#Basic Configure <<
LIBRS <- c('quantmod','stringr','fTrading','xts','TTR')
sapply(LIBRS,library,character.only=TRUE)

setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library(roxygen2)
roxygenize()
library("stock.Analyze")
#

research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)
#Basic Configure >>
#Param Configure <<
# url.head <- "https://query1.finance.yahoo.com/v7/finance/download/"
# url.tail <- "?period1=1199059200&period2=1585267200&interval=1d&events=history"
# crawl.check <- ".crawl.check.csv"
# extension <- ".TW"
file.extension <- ".csv"
crawl.delay.seconds <- c(2:8) #default
fake.header <- ""
index.code.list.path <- "/home/linus/Project/1_R/Analysis.of.trading.strategies/world.wide.broad.index.csv"
data.dir <- "/home/linus/Project/9.Shared.Data/0_Global/Index/"
#Param Configure >>

research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)

ignore.file <- ".ignore.index.file.csv"
if(! file.exists(ignore.file) ) { 
    processed.list <- c("0")
    write.table(processed.list, ignore.file) 
}

#read all code of taiwan stocks
index.code.list <- read.csv(index.code.list.path,header=TRUE, sep=",")
list.code <- as.character(index.code.list[,1])

for (i in 1:length(list.code))  { 

        processed.list <- read.table(ignore.file)
        processed.code <- as.numeric(processed.list)
#         browser()
        target.index <- list.code[i]
#         url <- m_paste(c(url.head ,target.index ,url.tail),op="")
        destfile <- paste(target.index, file.extension,sep="")
        distfile.path <- paste(data.dir, destfile, sep="")
        if(i > processed.code ) {
        
                 write.table(as.character(i),ignore.file) 

                        get.data <- getSymbols(target.index,auto.assign=FALSE)
                            if(inherits(get.data, "try-error")){ 
                                next 
                                }else{
#                                     write.csv(get.data, distfile.path)
                                    write.zoo(get.data, file = distfile.path, sep = ",")
                                    sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                                    print(paste("_Sleep.Second ",sle.num,sep=":"))
                                    Sys.sleep(sle.num)
                                    print(paste("_File download completely ",destfile,sep=":"))
                                }
                }else{
                print(m_paste(c("_Checked File ",as.character(i)," and Jump to Next File, Ignore Procssed File: ",destfile),op=""))
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

