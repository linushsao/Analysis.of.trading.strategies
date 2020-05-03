rm(list=ls())

#Basic Configure <<
LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','rvest')
sapply(LIBRS,library,character.only=TRUE)
setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")

data.dir <- m_env(name="research.path.of.linus",mode="r")
setwd(data.dir)

#Basic Configure >>
target.path <- "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/Company.relatived/"

#Param Configure <<
url.head <- c("https://tw.stock.yahoo.com/d/s/company_","https://tw.stock.yahoo.com/d/s/major_")
url.tail <- ".html"
# crawl.check <- ".crawl.check.csv"
prefix.dp <- c("cp","ma")
extension <- ".TW"
file.extension <- ".csv"
crawl.delay.seconds <- c(3:10) #default
fake.header <- ""
stock.code.list.path <- "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/Stock/all_codes.csv"

ignore.file <- ".ignore.stock.file.csv"
if(! file.exists(ignore.file) ) { 
    processed.list <- c("0")
    write.table(processed.list, ignore.file) 
}
#read all code of taiwan stocks
stock.code.list <- read.csv(stock.code.list.path,header=TRUE,sep=",")
list.code <- as.character(stock.code.list[,1])

main <- function() {
    for (i in 1:length(list.code))  { 

        target.stock <- m_check.code(list.code[i])
        
        processed.list <- read.table(ignore.file)
        processed.code <- as.numeric(processed.list)
        
        if(i > processed.code ) {
 
            for(j in 1:length(url.head)){
            
                write.table(as.character(i),ignore.file)
                    
                url <- m_paste(c(url.head[j] ,target.stock ,url.tail),op="")
                destfile <- m_paste(c(target.path, prefix.dp[j], target.stock, file.extension),op="")

                #compare if current stock code was downloaded before
                if(! file.exists(destfile)) {
                    tryit <- crawl.web(destfile,url)
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
    }
}
    # Sub Function
    
    crawl.web <- function(destfile,url) {

#         title <- c("year","Cash.Dividend","Earnings.rights.issue","Public reserve","Stock dividend", "Total")
        webpage <- read_html(url)
        stock.table <- webpage %>%
                html_nodes("table") %>%
                .[4] %>%
                html_table(fill = TRUE)
        stock.table <- stock.table[[1]][-c(1),]
#         names(stock.table) <- title
        rownames(stock.table) <- NULL
        
        write.csv(stock.table, file=destfile)
    }   
                

# Engage
# https://tw.stock.yahoo.com/d/s/dividend_2207.html
main()

    
    




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

