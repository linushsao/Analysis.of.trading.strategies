    rm(list=ls())
    graphics.off() 
    #
    LIBRS <- c('quantmod','stringr','xts','TTR','roxygen2','tseries','rlist','lubridate', 'ids', 'rvest','XML')
    sapply(LIBRS,library,character.only=TRUE)
    # sapply(LIBRS,install.packages,character.only=TRUE)
    setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
    library('roxygen2')
    roxygenize()
    library("stock.Analyze")
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
    setwd(research.path.of.linus)
#                                        外資及陸資(不含外資自營商)			        外資自營商			               外資及陸資		
#     證券代號	    證券名稱	  (買進股數	   賣出股數	      買賣超股數)  (買進股數  賣出股數	    買賣超股數)  (買進股數	賣出股數	  買賣超股數)
#     'STOCK_CODE', 'STOCK_NAME', 'YL0_BUYIN', 'YL0_SELLOUT', 'YL0_DIFF', 'YJ_BUYIN', 'YJ_SELLOUT', 'YJ_DIFF',   'YL_BUYIN', 'YL_SELLOUT', 'YL_DIFF'
    col.names <- c('STOCK_CODE', 'STOCK_NAME', 'YL0_BUYIN', 'YL0_SELLOUT', 'YL0_DIFF', 'YJ_BUYIN', 'YJ_SELLOUT', 'YJ_DIFF', 'YL_BUYIN', 'YL_SELLOUT', 'YL_DIFF')
    col.names.df <- c('Index', col.names)
    fi.orig.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/foreign.investment.Sales.Summary/2_toUTF8/'
    fi.dest.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/foreign.investment.Sales.Summary/3_stock/'
    mentain.date.start <- as.Date('2018-01-02')
    mentain.date.end <- as.Date(Sys.time())
    tmp.df <- data.frame(t(col.names.df))
    names(tmp.df) <- col.names.df
    
    while(TRUE)
    {
        tmp.df[1, ] <- NA
        filename.orig <- paste0(fi.orig.path, mentain.date.start, '.csv')
        if(file.exists(filename.orig) && (file.info(filename.orig)$size > 10))
        {
            m_msg(info=paste0('_Processing file: ', filename.orig))
            csv.file <- read.csv(filename.orig, header=FALSE, sep=',')
            del.row <- c(1:2)
            del.col <- c(1,13)
            csv.file <- csv.file[-del.row, -del.col]
            names(csv.file) <- col.names
            csv.file <- csv.file[-1, ]
            rownames(csv.file) <- NULL
            csv.file$STOCK_CODE <- gsub('=', '', as.character(csv.file$STOCK_CODE))
            #transfer numeric
            for(col.id in 3:ncol(csv.file))
            {
                for(row.id in 1:nrow(csv.file)) csv.file[row.id , col.id] <- gsub(',', '', csv.file[row.id , col.id], fixed=TRUE)
            }
            for(col.id in 3:ncol(csv.file)) csv.file[, col.id] <- as.numeric(csv.file[, col.id])
            
            for (row.id in 1:nrow(csv.file))
            {
                tmp.df[1,1] <- as.character(mentain.date.start)
                for (col.id in 1:ncol(csv.file))
                {
                    tmp.df[1, (col.id + 1)] <- csv.file[row.id, col.id]
                }
                filename.dest <- paste0(fi.dest.path, gsub(' ','',tmp.df$STOCK_CODE), '.TW.csv')
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
    }
        
    
    
