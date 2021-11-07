    rm(list=ls())
    # par(mfrow=c(1,1))
    graphics.off() 
    #
    LIBRS <- c('quantmod','stringr','xts','TTR','roxygen2','tseries','rlist','lubridate', 'ids', 'rvest','XML')
    sapply(LIBRS,library,character.only=TRUE)
    
        winpath.trans.local <- function(x, home.dir=NULL)
    {
       if(!is.null(home.dir))  tmp <- gsub(home.dir[1], '', x)
        tmp <- gsub('/', '\\\\', tmp)
       if(!is.null(home.dir))  tmp <- paste0(home.dir[2], tmp)
        
        return(tmp)
    }
    # sapply(LIBRS,install.packages,character.only=TRUE)
    my.wd <- "/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/"
    my.wd <- winpath.trans.local(my.wd, home.dir=c('/home/linus/', 'C:\\Users\\linus\\Documents\\'))
    setwd(my.wd)
    library('roxygen2')
    roxygenize()
    library("stock.Analyze")
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
    research.path.of.linus <- winpath.trans.local(research.path.of.linus, home.dir=c('/home/linus/', 'C:\\Users\\linus\\Documents\\'))
    setwd(research.path.of.linus)

#     fi.dest.path <- '/home/linus/Project/9.Shared.Data/1_Taiwan/www.twse.com.tw/foreign.investment.Sales.Summary/3_stock/'
    fi.dest.mix.path <- '/home/linus/Project/0_Comprehensive.Research/04_price.mixed/'
   fi.dest.mix.path <- winpath.trans.local(fi.dest.mix.path, home.dir=c('/home/linus/', 'C:\\Users\\linus\\Documents\\')) 
#     na.filter <- function(x) {return(x[complete.cases(x),])}
    get.input <- get.users.input() #get basic data of stock/eft/index
    get.path <- as.character(get.users.input(prompt='Pls Enter If Change file.path?', index='get.path'))
    fi.dest.mix.path <- ifelse((length(get.path) == 1 || !get.path), fi.dest.mix.path, get.path)
    
    stock.code <- paste0(get.input[1], '.TW.csv')
    testSet.period <- get.input[2]
    analyze.group <- get.input[3]
#     stock.code <- '2492.TW.csv'
    filename <- paste0(fi.dest.mix.path, stock.code)
    sel.raw <- na.filter(read.csv(filename, header=T, sep=',')[,-1])
    sel.xts <- xts(sel.raw[,-1], order.by=as.Date(sel.raw$Index))
    sel.xts$cum.ret <- cumprod(1 + sel.xts$Ret)
    sel.xts$cum.yldiff <- cumsum(sel.xts$YL_DIFF)

    ma.VALUE <- c(3,10,20)
    select.col.ma <- c()
    for(ma.id in 1:length(ma.VALUE))
    {
        ma.value <- ma.VALUE[ma.id]
        tmp <- m_smaCal(sel.xts$cum.yldiff, n=ma.value, increase=FALSE)$samCal
        name.tmp <- paste0('ma_', ma.value)
        names(tmp) <- name.tmp
        sel.xts <- merge(sel.xts, tmp)
        select.col.ma <- c(select.col.ma, name.tmp)
    }
    
    ma <- sel.xts$Close
    length.col.ma <- length(select.col.ma)
    ma$shrink <- xts(apply(sel.xts[, select.col.ma], 1, sd), order.by=(index(sel.xts)))
    ma$momentum <- sel.xts[, select.col.ma[1]] - sel.xts[, select.col.ma[length.col.ma]]
    ma <- ma[complete.cases(ma), ]

    sel.xts$SUM.YL_DIFF <- cumsum(sel.xts$YL_DIFF)
    
    #filrer
    sel.xts <- sel.xts[testSet.period]
    ma <- ma[testSet.period]
    stock.list <- data.frame( dataset.MGR(group=c(analyze.group,'list'), request='info') )
    list.id <- match(gsub('.TW.csv', '', stock.code), stock.list$STOCK_CODE)
    
    title.s <- paste0(stock.list$STOCK_CODE[list.id], ' ', stock.list$STOCK_NAME[list.id])
    #virtual
    graphics.off() 
    par(mfrow=c(3,2))
#     plot(sel.xts$Close)
    plot(sel.xts$Close, main=title.s)
    plot(sel.xts$cum.ret, main=title.s)
    barplot(sel.xts$SUM.YL_DIFF, main='Momentum of Bought & Sold')
    barplot(sel.xts$YL_DIFF, main='Daily Bought & Sold')
    plot(sel.xts[,select.col.ma], col=c('red', 'orange', 'blue', 'green'), main='Bought/Sold stocks of F.I.')
    plot(merge(ma[,c(2,3)], 0), col=c('green', 'brown', 'darkgray'), main='trend of Momentum & Shrink')
    
    
