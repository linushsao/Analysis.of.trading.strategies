rm(list=ls())

LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2','rvest','XML')
sapply(LIBRS,library,character.only=TRUE)

setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library(roxygen2)
roxygenize()
library("stock.Analyze")
#
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)

# before 102
# https://mops.twse.com.tw/nas/t21/sii/t21sc03_96_1.html
# url.head <- "https://mops.twse.com.tw/nas/t21/sii/t21sc03_"
# url.tail <- ".html"
# encoding <- "Big5"
# css.header <- "td td , tr+ tr .tt"

# after 102
# https://mops.twse.com.tw/nas/t21/sii/t21sc03_102_1_0.html
stock.code.list.path <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/stock.price/twse_all.code.tw.csv"

# Company.Basic.Information <- list(
url.head <- "https://goodinfo.tw/StockInfo/BasicInfo.asp?STOCK_ID="
url.tail <- ""
encoding <- "UTF-8"
css.header <- ".solid_1_padding_4_6_tbl td , .solid_1_padding_4_6_tbl nobr"
# )

crawl.check <- ".crawl.check.csv"
extension <- ""
file.extension <- ".csv"
fake.header <- "user-agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.122 Safari/537.36"
crawl.delay.seconds <- c(3:10)
table.target.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/goodinfo.tw/01_Company.Basic.Information/01_raw.data/"

page2csv <- function(page.url, page.encoding="UTF-8", export.file, css.selector)  {

    page.source <- read_html(page.url,encoding=page.encoding)
    version.block <- html_nodes(page.source, css.selector)
    content <- html_text(version.block)
    result <- unique(content)
    write.csv(result, file=export.file)
    return(length(result))
}
# Sub Function <<<
    
# Main Program <<

# main <- function() {
    #step1 download html
#     repeat {


#read all code of taiwan stocks
stock.code.list <- read.csv(stock.code.list.path,header=TRUE,sep=",")[,-c(1)]
list.code <- as.character(stock.code.list[,1])
list.name <- as.character(stock.code.list[,2])
list.l <- length(list.code)

for (i in list.l:1)  { 

                symbol <- m_check.code(list.code[i])
                stock.name <- paste(symbol, list.name[i], sep=" ")
                target.stock <- paste(symbol, extension, sep="")
                destfile <- m_paste(c(table.target.dir, target.stock, file.extension),op="")
                url <- m_paste(c(url.head, symbol, url.tail),op="")
                #compare if current stock code was downloaded before

                if(! file.exists(destfile) ) {
                    z.null <- page2csv(page.url=url , page.encoding=encoding, export.file=destfile, css.selector=css.header)
                        if(inherits(z.null, "try-error")){ 
                            next 
                            }else{
                                print(m_paste(c("_File download completely :", stock.name," size:",file.size(destfile)),op=""))
                                sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                                print(paste("_Sleep.Second ",sle.num,sep=":"))
                                Sys.sleep(sle.num)
                            }
                        }else{
                        print(m_paste(c("_Checked File ",stock.name,"and Jump to Next File, Ignore Procssed File: ",stock.name),op=""))
                    }
}

