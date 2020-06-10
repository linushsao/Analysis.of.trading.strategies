    rm(list=ls())
    # par(mfrow=c(1,1))
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

    if.only.basic <- as.logical(get.users.input(prompt='Pls Enter If only basic report(T/F)', index='if.only.basic'))
    # modify column & export
    remix.date <- get.users.input(prompt='Pls Enter Remix.date(y-m-d), or using (s)ys.time', index='remix.date')
    if(remix.date == 's') remix.date <- as.Date(Sys.time())
    
    filename=paste0('/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/remix.stock.2020_', remix.date,'.csv')
    filename_remix <- paste0(gsub('.csv', '', filename), '_.csv')
    data.raw <- read.csv(filename, header=TRUE, sep=',')[,-c(1)]
    period <- c(2017, 2020)
    for(id in period[1]:period[2])
    {
    year <- paste0('RATE.', id)
    data.raw <- m_remix.data(data=data.raw, mode=c('add.col','sort',year))
    }
#     write.csv(data.raw, file=filename_remix)

    # count cor & compare with World Index
    symbol.Market <- c('^TWII', '^GSPC', '^DJI', '^IXIC', '^N100', '^VIX')
    period.select <- paste0(period[1], '::', period[2])
    for(market.id in symbol.Market)
    {
        cor.collect <- c()
        Clprs.Market <- dataset.MGR(dataset.name=market.id, group=c('index','data'), request='info')
        Clprs.Market.xts <- Cl(xts(Clprs.Market[,-c(1)], order.by=as.Date(Clprs.Market$Index))[period.select])
        for(stock.id in 1:length(data.raw$STOCK_CODE))
        {
            Clprs.stock.data <- dataset.MGR(dataset.name=data.raw$STOCK_CODE[stock.id], group=c('stock','data'), request='info')
            Clprs.stock.xts <- Cl(xts(Clprs.stock.data[,-c(1)], order.by=as.Date(Clprs.stock.data$Index))[period.select])
            tmp.xts <- merge(lag(Clprs.Market.xts), Clprs.stock.xts)
            tmp.xts <- tmp.xts[complete.cases(tmp.xts),]
            cor.collect <- c(cor.collect, cor(tmp.xts)[2,1])
            m_msg(info=paste0('_Processing file ', stock.id, ' :', data.raw$STOCK_CODE[stock.id], ' .cor. ', market.id ))
        }
        cor.collect <- as.data.frame(cor.collect)
        new.col.name <- paste0(market.id, '_lag.', 1)
        data.raw[, new.col.name] <- cor.collect
    }

    leng.col <- ncol(data.raw)
    col.period <- c((leng.col - length(symbol.Market) + 1) : leng.col)
    remix.col <- c(col.period, (1:(leng.col -  length(symbol.Market))))
    data.raw <- data.raw[, remix.col]
    
    write.csv(data.raw, file=filename_remix)
    
    if(!if.only.basic)
    {
        #Compare with UP/DN 100 Rank of YAHOO
        page2csv <- function(page.url, page.encoding="UTF-8", export.file=NULL, css.selector='table')  {
            page.source <- read_html(page.url,encoding=page.encoding)
            version.block <- html_nodes(page.source, css.selector)
            return(version.block)
        }
        
        page.url <- c('https://tw.stock.yahoo.com/d/i/rank.php?t=up&e=TAI&n=100',
                    'https://tw.stock.yahoo.com/d/i/rank.php?t=down&e=TAI&n=100',
                    'https://tw.stock.yahoo.com/d/i/fgbuy_tse100.html',
                    'https://tw.stock.yahoo.com/d/i/fgsell_tse100.html')
        names(page.url) <- c('UP100', 'DN100', 'f.i.OverBought', 'f.i.OverSold')
        for(name.id in names(page.url))
        {
    #         name.id <- 'f.i.OverBought' #for test
            out.csv.raw <- page2csv(page.url=page.url[name.id], page.encoding='BIG5')

            out.csv <- out.csv.raw %>%
                .[3:4] %>%
                html_table(fill = TRUE)
            
            if( name.id %in% c('UP100', 'DN100')) 
            {
                data.grap <- c(1,9,1)
                out.csv <- out.csv[[data.grap[1]]][-c(1:2),-c(1)]
            }else{
                data.grap <- c(2,6,2)
                out.csv <- out.csv[[data.grap[1]]]
            }

            out.csv <- out.csv[,c(data.grap[3]:data.grap[2])]
            names(out.csv) <- out.csv[1,]
            out.csv <- out.csv[-1,]
            rownames(out.csv) <- NULL
            
            if(data.grap[1]==1) 
            {
                tmp.CodeName <- unlist(strsplit(out.csv[,1], ' '))
                leng.a <- length(tmp.CodeName) / 2
                mask <- rep(c(0,1), leng.a)
                tmp.df <- data.frame(STOCK_DATA=tmp.CodeName, MASK=mask)
                tmp.STOCK_CODE <- tmp.df[tmp.df$MASK==0,]
                tmp.STOCK_NAME <- tmp.df[tmp.df$MASK==1,]
                DnUp.check.df <- data.frame(STOCK_CODE=paste0(tmp.STOCK_CODE[,1], '.TW'), STOCK_NAME=tmp.STOCK_NAME[,1])
                DnUp.check.df[, paste0('rank_', name.id)] <- index(DnUp.check.df)
            }else{
            
                join.vector <- function(v)
                {
                    tmp.char <- ''
                    for(i in 1:length(v)) tmp.char <- paste0(tmp.char,v[i] )
                    return(tmp.char)
                }
                vec <- data.frame(v.num=c(0:9,rep(NA,16)),
                                v.eng=c(LETTERS[seq( from = 1, to = 26 )]) )
                tmp.code <- c()
                for(list.row in 1:nrow(out.csv)) 
                {
                    target <- out.csv[list.row, 1]
                    tmp.str <- c()
                    flag.jump <- FALSE
                    for(char.id in 1:nchar(target))
                    {
                        flag.match <- FALSE
                        for(col.id in 1:ncol(vec))
                        {
                            for(row.id in 1:nrow(vec))
                            {
                                valu <- vec[row.id, col.id]
                                if(is.na(valu)) next            
    #                             print(c(list.row, target, valu, substr(target, char.id, char.id)))
                                if(valu == substr(target, char.id, char.id) && !flag.jump) 
                                {
                                    tmp.str <- c(tmp.str, valu)
                                    flag.match <- TRUE
                                }
                            }
                        }
                        if(!flag.match) flag.jump <- TRUE
                    }
                    out.csv[list.row, 1] <- gsub(join.vector(tmp.str), '', target)
                    tmp.code <- c(tmp.code, join.vector(tmp.str))
                }
                DnUp.check.df <- data.frame(STOCK_CODE=paste0(tmp.code, '.TW'), STOCK_NAME=out.csv[,1])
                DnUp.check.df[, paste0('rank_', name.id)] <- index(DnUp.check.df)
            }
            
            data.raw.tmp <- data.raw[,c('STOCK_CODE', 'STOCK_NAME', 'LAST_CLOSE.average', 'oscillate.brown', 'oscillate.blue', 'oscillate.red', 'oscillate.tenstion', 'oscillate.shrink')]
            data.raw.DnUp.check <- merge(data.raw.tmp, DnUp.check.df, by='STOCK_CODE', all.y=TRUE)
            data.raw.DnUp.check$STOCK_NAME.x <- data.raw.DnUp.check$STOCK_NAME.y
            data.raw.DnUp.check$STOCK_NAME.y <- NULL
            
            leng.col <- ncol(data.raw.DnUp.check)
            remix.col <- c(leng.col, (1:(leng.col-1)))
            data.raw.DnUp.check <- data.raw.DnUp.check[, remix.col]
            
            filename_remix <- paste0(gsub('.csv', '', filename), '_', name.id,'.csv')
            write.csv(data.raw.DnUp.check, file=filename_remix)
        }
    }

