    rm(list=ls())
    graphics.off() 
    #
    LIBRS <- c('stringr','xts','TTR','roxygen2','tseries','rlist','lubridate', 'ids')
    sapply(LIBRS,library,character.only=TRUE)
    # sapply(LIBRS,install.packages,character.only=TRUE)
    setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
    library('roxygen2')
    roxygenize()
    library("stock.Analyze")
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
    setwd(research.path.of.linus)

# 'data.frame':	3 obs. of  5 variables:
#  $ 買進        : chr  "335,082" "104,718" "5,442,547"
#  $ 賣出        : chr  "420,679" "81,219" "7,603,244"
#  $ 現金(券)償還: chr  "29,466" "19,556" "1,119,354"
#  $ 前日餘額    : chr  "5,828,986" "604,834" "107,945,457"
#  $ 今日餘額    : chr  "5,713,923" "561,779" "104,665,406"
    col.byday.names <- c( 'BoughtIn', 'SellingOut', 'Cash|Ticket.Pay', 'balance.yesterday', 'balance.today')
    
    #   股票代號	股票名稱	買進	        賣出	    現金償還	前日餘額	          今日餘額	        限額	    買進	        賣出	    現券償還	  前日餘額	            今日餘額	       限額	       資券互抵	     註記
    #   STOCK_CODE  STOCK_NAME  M.T.BoughtIn    M.T.SellOut M.T.CashPay M.T.balance.yesterday M.T.balance.today M.T.Limits  S.S.BoughtIn    S.S.SellOut S.S.TicketPay S.S.balance.yesterday S.S.balance.today  S.S.Limits  S.S.TicketPay Note
    col.bystock.names <- c( 'STOCK_CODE', 'STOCK_NAME', 'M.T.BoughtIn', 'M.T.SellOut', 'M.T.CashPay', 'M.T.balance.yesterday', 'M.T.balance.today', 'M.T.Limits', 
                    'S.S.BoughtIn', 'S.S.SellOut', 'S.S.TicketPay', 'S.S.balance.yesterday', 'S.S.balance.today', 'S.S.Limits', 'S.S.TicketPay', 'Note')
    fi.orig.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/Margin.Trading_Short.Selling/2_toUTF8/'
    fi.dest.bystock.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/Margin.Trading_Short.Selling/3_by.stock/'
    fi.dest.byday.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/Margin.Trading_Short.Selling/4_by.day/' 
    fi.dest.bysingle.stock.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/Margin.Trading_Short.Selling/5_by.single.stock/'
    
#     mentain.date.start <- as.Date('2018-01-02')
    mentain.date.start <- as.Date('2020-03-18')
    mentain.date.end <- as.Date(Sys.time())
#     tmp.df <- data.frame(t(col.bystock.names))
#     names(tmp.df) <- col.bystock.names
    
    byday.col <- c(2:5)
    bystock <- 10

    joint.row <- function(x, row)
    {
        tmp <- c()
        for(rowid in row)
        {
            tmp <- c(tmp, x[rowid, ])
        }
        return(tmp)
    }
            
    while(TRUE)
    {
        tmp.df <- data.frame(t(col.bystock.names))
        names(tmp.df) <- col.bystock.names
        tmp.df[1, ] <- NA
        filename.orig <- paste0(fi.orig.path, mentain.date.start, '.csv')
        
        if(file.exists(filename.orig) && (file.info(filename.orig)$size > 10))
        {
            m_msg(info=paste0('_Processing file: ', filename.orig))
            csv.file <- read.csv(filename.orig, header=FALSE, sep=',')
            csv.file_byday <- csv.file[byday.col, ]
            csv.file_bystock <- csv.file[(bystock:nrow(csv.file)), ]
            #for by.day
            csv.file_byday <- csv.file_byday[,-7]
            names(csv.file_byday) <- csv.file_byday[1,]
            rownames(csv.file_byday) <- csv.file_byday[,1]
            csv.file_byday <- csv.file_byday[-1, -1]
            names(csv.file_byday) <- col.byday.names
            
            #for by.stock
            row.id <- 1
            tmp.row.id <- 1
            while( row.id <= nrow(csv.file_bystock) )
            {
                tmp.df[tmp.row.id, ] <- joint.row(csv.file_bystock, c(row.id:(row.id+2)))
                tmp.row.id <- tmp.row.id + 1
                row.id <- row.id + 3
            }
            
            tmp.df <- tmp.df[-1, ]
            tmp.df$STOCK_CODE <- gsub('=', '', tmp.df$STOCK_CODE)
            #transfer numeric
            for(col.id in 3:ncol(tmp.df))
            {
                for(row.id in 1:nrow(tmp.df)) tmp.df[row.id , col.id] <- gsub(',', '', tmp.df[row.id , col.id], fixed=TRUE)
            }            
            rownames(tmp.df) <- NULL
            tmp.df <- tmp.df[,-ncol(tmp.df)]
            for(col.id in 3:ncol(tmp.df)) tmp.df[, col.id] <- as.numeric(tmp.df[, col.id])
            csv.file_bystock <- tmp.df
            #save file
            filename.byday.dest <- paste0(fi.dest.byday.path, mentain.date.start, '.csv')
            filename.bystock.dest <- paste0(fi.dest.bystock.path, mentain.date.start, '.csv')
            write.csv(csv.file_byday, filename.byday.dest)
            write.csv(csv.file_bystock, filename.bystock.dest)
            
            #seprate into single stock
            rm(tmp.df) #reset tmo.df
            col.bysingle.stock.names <- c('Index', col.bystock.names)
            tmp.df <- data.frame(t(col.bysingle.stock.names)) 
            names(tmp.df) <- col.bysingle.stock.names
            tmp.df[1, ] <- NA
#             
            for (row.id in 1:nrow(csv.file_bystock))
            {
                tmp.df[1,1] <- as.character(mentain.date.start)
                for (col.id in 1:ncol(csv.file_bystock))
                {
                    tmp.df[1, (col.id + 1)] <- csv.file_bystock[row.id, col.id]
                }
                filename.dest <- paste0(fi.dest.bysingle.stock.path, gsub(' ','',tmp.df$STOCK_CODE), '.TW.csv')
                if(file.exists(filename.dest))
                {
                    get.date.list <- read.csv(file=filename.dest, header=T, sep=',')[,1]
                    if(! (as.character(mentain.date.start) %in% get.date.list) ) #if date not avilable,append data
                    {
                        write.table(tmp.df, file=filename.dest, append=T, row.names=F, col.names=F,  sep=",")
                    }
                }else{
                    write.table(tmp.df, file=filename.dest, append=F, row.names=F, col.names=T,  sep=",")                
                }
            }
     }       
    if(mentain.date.start == mentain.date.end)  break
    mentain.date.start <- mentain.date.start + 1
    rm(tmp.df)
}
        
    
    
