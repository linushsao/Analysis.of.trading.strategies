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

    # modify column & export
    filename=paste0('/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/remix.stock.2020_', as.Date(Sys.time()),'.csv')
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
    
    #Compare with UP/DN 100 Rank of YAHOO
    page2csv <- function(page.url, page.encoding="UTF-8", export.file=NULL, css.selector='table')  {
        page.source <- read_html(page.url,encoding=page.encoding)
        version.block <- html_nodes(page.source, css.selector)
#         content <- html_text(version.block)
#         result <- unique(content)
#         if(! is.null(export.file)) write.csv(result, file=export.file)
        return(version.block)
    }
    
    page.url <- c('https://tw.stock.yahoo.com/d/i/rank.php?t=up&e=TAI&n=100',
                  'https://tw.stock.yahoo.com/d/i/rank.php?t=down&e=TAI&n=100',
                  'https://tw.stock.yahoo.com/d/i/fgbuy_tse100.html',
                  'https://tw.stock.yahoo.com/d/i/fgsell_tse100.html')
    names(page.url) <- c('UP100', 'DN100', 'f.i.OverBought', 'f.i.OverSold')
    for(name.id in names(page.url))
    {
        out.csv.raw <- page2csv(page.url=page.url[name.id], page.encoding='BIG5')

        out.csv <- out.csv.raw %>%
            .[3:4] %>%
            html_table(fill = TRUE)
        
        if( name.id %in% c('UP100', 'DN100')) 
        {
            data.grap <- c(1,9)
        }else{
            data.grap <- c(2,6)
        }
#         out.csv <- out.csv[[1]][-c(1:2),-c(1)]
#         out.csv <- out.csv[,c(1:9)]
        out.csv <- out.csv[[data.grap[1]]][-c(1:2),-c(1)]
        out.csv <- out.csv[,c(1:data.grap[2])]
        names(out.csv) <- out.csv[1,]
        out.csv <- out.csv[-1,]
        rownames(out.csv) <- NULL
        tmp.CodeName <- unlist(strsplit(out.csv[,1], ' '))
        
        leng.a <- length(tmp.CodeName) / 2
        mask <- rep(c(0,1), leng.a)
        tmp.df <- data.frame(STOCK_DATA=tmp.CodeName, MASK=mask)
        tmp.STOCK_CODE <- tmp.df[tmp.df$MASK==0,]
        tmp.STOCK_NAME <- tmp.df[tmp.df$MASK==1,]
        DnUp.check.df <- data.frame(STOCK_CODE=paste0(tmp.STOCK_CODE[,1], '.TW'), STOCK_NAME=tmp.STOCK_NAME[,1])
        DnUp.check.df[, paste0('rank_', name.id)] <- index(DnUp.check.df)
        
        data.raw.tmp <- data.raw[,c('STOCK_CODE', 'STOCK_NAME', 'oscillate.brown', 'oscillate.blue', 'oscillate.red')]
        data.raw.DnUp.check <- merge(data.raw.tmp, DnUp.check.df, by='STOCK_CODE', all.y=TRUE)
        data.raw.DnUp.check$STOCK_NAME.x <- data.raw.DnUp.check$STOCK_NAME.y
        data.raw.DnUp.check$STOCK_NAME.y <- NULL
        
        leng.col <- ncol(data.raw.DnUp.check)
        remix.col <- c(leng.col, (1:(leng.col-1)))
        data.raw.DnUp.check <- data.raw.DnUp.check[, remix.col]
        
        filename_remix <- paste0(gsub('.csv', '', filename), '_', name.id,'.csv')
        write.csv(data.raw.DnUp.check, file=filename_remix)
    }
# 
# list.path <- '/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/remix.stock.2020_2020-05-25.csv'
# stock.list <- read.csv(list.path, header=T, sep=',')[,-c(1)]
# stock.path <- dataset.MGR(group=c('stock','data'), request='conf')
# stock.list$STOCK_PATH <- paste0(stock.path, stock.list$STOCK_CODE, '.csv')
# range <- 250*3
# col.names <- names(stock.list)
# 
# tmp.data <- data.frame()
# for(stock.id in 1:length(stock.list$STOCK_CODE))
# {
#     data.raw <- read.csv(stock.list$STOCK_PATH[stock.id], header=T, sep=',')
#     data.xts <- xts(data.raw[,-c(1)], order.by=as.Date(data.raw[,c(1)]))
#     data.xts <- data.xts[complete.cases(data.xts), ]
#     data.nrow <- nrow(data.xts)
#     data.pre <- data.xts[(data.nrow - range):(data.nrow) ,]
# 
#     Clprs <- Cl(data.pre)
#     
#     per <- periodogram(na.omit(ROC(Clprs)), plot=FALSE)
#     per_df <- data.frame(freq=per$freq, spec=per$spec)
#     per.sort <- per_df[order(-per_df$spec),]
#     per.sort$seasonal <- 1 / per.sort$freq
#     stop()
#     stock.mean   <- mean(Clprs) #平均數
#     stock.median <- median(Clprs)  #中位數
# #     stock.range <- range(Clprs) #全距
# #     stock.quantile<- quantile(Clprs) #四分位數
#     stock.IQR <- IQR(Clprs) #四分位差
#     stock.sd<- sd(Clprs) #標準差
#     stock.var <- var(Clprs) #變異數 
#     stock.skewness <- skewness(Clprs) #偏度
#     stock.kurtosis <- kurtosis(Clprs) #峰度   
#     
#     stock.summary <- c(as.character(stock.list$STOCK_CODE[stock.id]),  
#                         stock.mean, stock.median, 
#                         stock.IQR, stock.sd, stock.var, stock.skewness, stock.kurtosis )
#     col.names.tmp <- c(col.names[1], 'mean', 'median', 'IQR', 'sd', 'var', 'skewness', 'kurtosis')
#     tmp.data <- rbind(tmp.data, stock.summary)
#     names(tmp.data) <- col.names.tmp
#     stop()
# }
# write.zoo(res, file='test1.csv')
# # etf.list <- dataset.MGR(group=c('etf','list'), request='info')[,c(1:2)]
# # 
# # tmp.s <- na.omit(merge(Cl(main.stock.xts), Cl(Hedge.funds.xts)))[period]
# # res <- data.frame(cor(tmp.s))
# # res
# # write.zoo(res, file='test1.csv')
# 
# a <- read.csv('/home/linus/Project/1_R/Analysis.of.trading.strategies/all_codes.csv', header=T, sep=',')
# b <- read.csv('/home/linus/Project/0_Comprehensive.Research/09_理論及研究資料/aa.csv', header=T, sep=',')
# 
# c <- merge(a,b,all=TRUE, by='STOCK_CODE')
# c$STOCK_NAME.x <- c$STOCK_NAME.y
# c$STOCK_NAME.y <- NULL
# 
# col.name <- names(c)
# col.name <- gsub('STOCK_NAME.x', 'STOCK_NAME', col.name)
# names(c) <- col.name
# write.csv(c, '/home/linus/Project/1_R/Analysis.of.trading.strategies/all_codes.csv')


