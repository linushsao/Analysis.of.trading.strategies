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
fake.header <- ":authority: goodinfo.tw
:method: GET
:path: /StockInfo/BasicInfo.asp?STOCK_ID=9958
:scheme: https
accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9
accept-encoding: gzip, deflate, br
accept-language: zh-TW,zh;q=0.9,en-US;q=0.8,en;q=0.7
cache-control: max-age=0
cookie: CLIENT%5FID=20200303180300054%5F101%2E137%2E104%2E100; _ga=GA1.2.57923180.1583258628; __gads=ID=60effbd9ae126b43:T=1583229814:S=ALNI_MZJ-ewWYs0XfcQqxb8qwMw8OYjssQ; SCREEN_SIZE=WIDTH=1366&HEIGHT=768; LOGIN=EMAIL=linushsao%40gmail%2Ecom&USER%5FNM=Linus+hsao&ACCOUNT%5FID=111972340513654340090&ACCOUNT%5FVENDOR=Google&NO%5FEXPIRE=T; _gid=GA1.2.626579366.1586509635; GOOD%5FINFO%5FSTOCK%5FBROWSE%5FLIST=12%7C9910%7C1319%7C6283%7C0050%7C00635U%7C020004%7C03071X%7C03428B%7C04232P%7C1907%7C1102%7C1443
sec-fetch-dest: document
sec-fetch-mode: navigate
sec-fetch-site: none
sec-fetch-user: ?1
upgrade-insecure-requests: 1
user-agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.122 Safari/537.36"
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

