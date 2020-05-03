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
url.head <- "https://mops.twse.com.tw/nas/t21/sii/t21sc03_"
url.tail <- "_0.html"
encoding <- "Big5"
css.header <- "td td td , td td td , td td .tt"

crawl.check <- ".crawl.check.csv"

fake.header <- ""

date.start <- "2013-01-01"
date.endby.month <- substring(seq(as.Date( date.start ),length=(2020-2013)*12+3,by="months") ,1,7) #date.end.month
table.target.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/Business.income.statistics/after.102(included)/"
html.target.dir <- "/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/Business.income.statistics/raw.data.html/"

page2table_1 <- function(page.url, page.encoding="UTF-8", export.file, css.selector)  {


    page.source <- read_html(page.url,encoding=page.encoding)
    version.block <- html_nodes(page.source, css.selector)
    content <- html_text(version.block)
    result <- html2table(content)
    result.all <- list(page.source=content , page.table=result)
    write.csv(result, file=export.file)
    return(result.all)
}
# Sub Function <<<
    html2table <- function(content=content) {
        
        vector.insert <- function(position, v.insert, v.be_insert) {
            l <- length(v.be_insert)
            result <- c( v.be_insert[1:position], v.insert, v.be_insert[(position+1):l])
            return(result)
            }
        
        vector.shift <- function(be.shift, position="left") {
            l <- length(be.shift)
            if( position == "left") {
                result <- c(be.shift[2:l], be.shift[1])
            }else if( position == "right") {
                result <- c(be.shift[l], be.shift[1:(l-1)])
            }
            return(result)
            }

        # z.table <- html_table(version.block)
        spl.key <- " "
        replace.total.key <- "t000t" #for TOTAL
        replace.key <- "c000c"
        replace.memo.key <- "x000x"
        replace.title.key <- "i000i"
        rowdata.count <- 11
        cache.title <- "公司代號 公司名稱 去年當月營收 當月營收 上月營收 上月比較增減 去年同月增減 當月累計營收 去年累計營收 前期比較增減 "
        c1.title <- strsplit(cache.title, spl.key)[[1]]
        c1t.title <- c("company.code","company.name","revenue.month","Last.month.revenue","Last.year.month.revenue","Last.month.revenue(%)","Last.year.month.revenue(%)","cumulative.revenue.month","last.year.cumulative.revenue","early.inNdecrease")

        c2.title <- c(" ",'\\(%\\)',"營業收入","累計營業收入","合計","不適用","備註","無","-","此為本公司自結數")
        c2t.title <- c(NA,"",NA,NA,replace.total.key,"00",replace.key,replace.key,replace.memo.key,replace.key)

        replace.collect <- c(replace.total.key, replace.key, replace.memo.key, replace.title.key, c1t.title)
        content.1 <- na.omit(m_gsub(c2.title, c2t.title, content))
        # write.csv(content.1, file="test1.csv")
        content.2 <- m_gsub(c1.title, c1t.title, content.1)
        # write.csv(content.2, file="test2.csv")

        content.2 <- gsub(" ", "", content.2, fixed = TRUE)

        c.mark <- c()
        code.mark <- c()
        k <- 0
        L <- length(content.2)
        
#         content.2 <- na.omit(content.2)
# 

        for(i in 11:L){ #replace total related
            
                if(! is.na(as.numeric(content.2[i])) ) { 
                    code.mark <- c( code.mark, c(i, content.2[i]) )
                    }
                if(content.2[i] == replace.total.key) {
                        for(j in 1:10) {
                        re.num <- i+j-1
                        content.2[re.num] <- replace.key
                        }
                        content.2 <- vector.insert(i, replace.key, content.2)
                    i <- i+10
                    }
                if(content.2[i] == c1t.title[length(c1t.title)]) {
                        content.2 <- vector.insert(i, replace.title.key, content.2)
                    i <- i+1
                    }  
  
                }

                content.3 <- matrix(content.2,ncol=rowdata.count,byrow=TRUE)

                L <- length(content.3[,1])
                for(ii in 1:10){
                    r.key <- c(1:10)
                    for(i in 1:length(r.key)) {
                        r.1 <- r.key[i]
                        i<-0
                        repeat{ 
                            i <- i+1
                            if(i > L) break
                            if(content.3[i, r.1] %in% c(replace.key, replace.memo.key, replace.title.key)) {
                            
                                    content.3[i,] <- vector.shift(be.shift=content.3[i,])
                                }   
                            }
                        }
                    }
                    
            content.3[ content.3 %in% replace.collect ] <-  NA
            
            all.name <- read.csv("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/Stock/all_codes.csv",header=TRUE,sep=",")
            all.name.1 <- all.name$STOCK_NAME
            
            L <- length(content.3[,1])
            i <- 0
            repeat{
                 i <- i+1
                if(i > L) break
                    for(j in 1:rowdata.count){
                        if(j != 2) {
                            if(grepl(replace.memo.key, content.3[i, j], fixed=TRUE)) {content.3[i, j] <- gsub(replace.memo.key,"-",content.3[i, j])  }
                            if(content.3[i, j] %in% all.name.1) { #shift vector
                                shift.num <- ifelse((j<2),2-j,(rowdata.count-(j-2)) )
                                for(k in 1:shift.num) {content.3[i,] <- vector.shift(be.shift=content.3[i,],position="right")}
                                }
                            }
                        } 
            }
 
            i <- 0
            repeat{
                i <- i+1
                L <- length(content.3[,1])
#                 print(c(L,i))
                if(i>L){break}
                    j <- rowdata.count
                    repeat{ 
                        j <- j-1
                        if(j<1) {break}
                        if(is.na(content.3[i,j])) {
#                             print("---delete---")
                            content.3 <- content.3[-c(i),]
#                             print(c(L,i,j))
#                             print("------------")
                            i <- i-1
                            break}
                        }

                }
                        
        #remove not complete data of company
#         code.mat <- matrix(code.mark,ncol=2,byrow=TRUE)
#         code.mat[,2] <- gsub("\\.",NA,code.mat[,2])
#         code.mat <- na.omit(code.mat)
#         for(i in 2:dim(code.mat)[1]) {
# 
#             num.before <- as.numeric(code.mat[i-1,1])
#             num.current <- as.numeric(code.mat[i,1])
#             if( (num.current-num.before) != rowdata.count ) {
#                 content.2[num.before:(num.current-1)] <- replace.key
#                 }
#         }
        # write.csv(content.2,file="test3.csv")
#         browser()
#         content.3 <- content.2[! (content.2 == replace.key)]
#         content.3 <- content.2

#         a <- matrix(content.3,ncol=rowdata.count,byrow=TRUE)
        content.3 <- content.3[,-c(rowdata.count)]
        #colnames(a) <- c1t.title
        return(content.3)
    }
    


# Main Program <<

# main <- function() {
    #step1 download html
#     repeat {

        for(i in 1:length(date.endby.month)) {
        
            by.year <- as.character(as.numeric(substring(date.endby.month[i],1,4)) - 1911)
            by.month <- as.character(as.numeric(substring(date.endby.month[i],6,7)))
            
            url <- m_paste(c(url.head, by.year, "_",by.month, url.tail),op="")
            destfile <- m_paste(c(table.target.dir, date.endby.month[i], ".csv"), op="")
            destfile.raw <- m_paste(c(html.target.dir, date.endby.month[i], ".csv"), op="")

            z.dev.null <- env(name="crawl.date", value=date.endby.month[i], mode="w")

            if (! file.exists(destfile) )  {                

                    crawl.delay.seconds <- env(name="crawl.delay.seconds",mode="r")
                    if( is.null(crawl.delay.seconds) || (crawl.delay.seconds  == "")) { 
                        crawl.delay.seconds <- c(10:20) #default
                    }
                    z.dev.null <- page2table_1(page.url=url , page.encoding=encoding, export.file=destfile, css.selector=css.header)
#                     stop()
                    write.csv(z.dev.null[[1]], file=destfile.raw)
                    if(inherits(z.dev.null, "try-error")){ next 
                        }else{
                        sle.num  <- sample(crawl.delay.seconds,size=1,replace=TRUE)
                        print(paste("_Sleep.Second ",sle.num,sep=":"))
                        Sys.sleep(sle.num)
                        print(paste("_File download completely ",destfile,sep=":"))
                        }

                }else{
                print(paste("_Ignoer Completely Downloaded File ",destfile,sep=":"))
                }
                    
            }
#         }
# }
