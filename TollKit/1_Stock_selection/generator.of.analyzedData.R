rm(list=ls())
par(mfrow=c(1,1))
graphics.off() 
#
LIBRS <- c('quantmod','stringr','xts','TTR','roxygen2','tseries','rlist','lubridate')
sapply(LIBRS,library,character.only=TRUE)

setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")
#
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)

#generate analyze data
analyze.group.id <- 2
if(analyze.group.id==0)
{
    analyze.group.conf <- c('index', 'stock', 'etf')
    }else{
    analyze.group.conf <- c('index', 'stock', 'etf')[analyze.group.id]
    }
# analyze.group.conf <- c('stock')
get.research.period <- 5
price.limits <- c("stock")
tranSet.period <-  c(2008:year(Sys.Date()))
testSet.period <- c(2020)

env.config <- data.frame(
    data=c( "/home/linus/Project/9.Shared.Data/0_Global/Index/",
            "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stock.price/",
            "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/etf.price/"),
    list=c( "/home/linus/Project/1_R/Analysis.of.trading.strategies/world.wide.broad.index.csv",
            "/home/linus/Project/1_R/Analysis.of.trading.strategies/all_codes.csv",
            "/home/linus/Project/1_R/Analysis.of.trading.strategies/all.ETF.code.csv"),
    remix=c(    "/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/03_index/",
                "/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/",
                "/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/02_etf/"),
    ma=c(    "/home/linus/Project/0_Comprehensive.Research/02_Logarithmic.table/02_Index/",
             "/home/linus/Project/0_Comprehensive.Research/02_Logarithmic.table/01_stock/",
             "/home/linus/Project/0_Comprehensive.Research/02_Logarithmic.table/03_etf/"),
    row.names=c('index', 'stock', 'etf')
)

for( group.id in 1:length(analyze.group.conf))
{

    analyze.group <- analyze.group.conf[group.id]
#     analyze.group <- analyze.group.conf[1]
    
    data.path <- as.character(env.config[analyze.group, 'data'])
    list.path <- as.character(env.config[analyze.group, 'list'])
    ma.path <- as.character(env.config[analyze.group, 'ma'])
    prefix.raw.data.name <- m_env(name="prefix.raw.data.name",mode="r")
    file.extension <- ".csv"
    list.file.extension <- '.RData'

    #generating basic data for analyze
    raw.data.List <- generator.of.stockData(tranSet.period=tranSet.period, data.path=data.path, list.path=list.path, group=analyze.group)

    #select data for analyze
    # select.col <- c(2,3,7,5,6,9,10,12)
    select.col.maininfo <- c( 'STOCK_NAME', 'GROUP')
    select.col <- c('STOCK_CODE', select.col.maininfo, 'LAST_CLOSE', 'Clspr.ret.Stdev', 'RATE', 'RATE.max', 'MaxDrawDOWN', 'SHARP_RATIO')
    select.col.addYear <- c(4:9)
    remix.data <- data.frame()

    #combind all selected data of years
    for(i in 1:length(raw.data.List)) 
    {
        file.name <- raw.data.List[i]
        year.name <- m_gsub(c(research.path.of.linus, analyze.group, prefix.raw.data.name, file.extension, '.'),c("",'','','',''), file.name,fixed=TRUE)
        if(as.numeric(year.name) > testSet.period) next
        
        tmp.data <- read.csv(file.name, header=TRUE, sep=",")
        select.data <- tmp.data[, select.col]
        tmp.name <- names(select.data)

        for(rowid in select.col.addYear) {tmp.name[rowid] <- paste(tmp.name[rowid], year.name,sep='.')}
        names(select.data) <- tmp.name
        if(i == 1) { 
            remix.data <- select.data 
            }else{
            if.keep <- ifelse(analyze.group == 'stock', FALSE, TRUE)
#             remix.data.1 <- merge(remix.data, select.data[,c(1:3,select.col.addYear)], by='STOCK_CODE', all=if.keep)
            remix.data.1 <- merge(remix.data, select.data, by='STOCK_CODE', all=if.keep)
            tmp.name <- c()
            for( name.id in select.col.maininfo)
            {
                name.x <- paste0(name.id,'.x')
                name.y <- paste0(name.id,'.y')
                remix.data.1[,name.x] <- remix.data.1[,name.y]
                remix.data.1[,name.y] <- NULL
                tmp.name <- c(tmp.name, name.x)
            }
#             remix.data.1[,'STOCK_NAME.x'] <- remix.data.1[,'STOCK_NAME.y']
#             remix.data.1[,'GROUP.x'] <- remix.data.1[,'GROUP.y']
#             remix.data.1[,'LAST_CLOSE.x'] <- remix.data.1[,'LAST_CLOSE.y']
#             remix.data.1[,'STOCK_NAME.y'] <- NULL
#             remix.data.1[,'GROUP.y'] <- NULL
#             remix.data.1[,'LAST_CLOSE.y'] <- NULL
            tmp.name <- m_gsub(tmp.name, select.col.maininfo, names(remix.data.1))
            names(remix.data.1) <- tmp.name
            remix.data <- remix.data.1; rm(remix.data.1)
            }

    }

    # remove duplicated code of column
    remix.data <- remix.data[!duplicated(remix.data$STOCK_CODE), ]

    # addition information: oscillation.indicator
    tmp.stock.data <- data.frame( 
                            STOCK_CODE=rep(NA, nrow(remix.data)) ,
                            oscillate.brown=rep(NA, nrow(remix.data)) ,
                            oscillate.blue=rep(NA, nrow(remix.data)) ,
                            oscillate.red=rep(NA, nrow(remix.data)) ,
                            oscillate.tenstion=rep(NA, nrow(remix.data)) ,
                            oscillate.shrink=rep(NA, nrow(remix.data)) ,
                            oscillate.shrink.ret=rep(NA, nrow(remix.data))
                            ) 
                            
    for(rowid in 1:nrow(remix.data)) {
    
        stock.code <- as.character(remix.data$STOCK_CODE[rowid])
        target.stock <- m_paste(c(data.path, stock.code, file.extension), op='')
        #data prepare
        tmp.stock.data$STOCK_CODE[rowid] <- stock.code
        
        if(file.exists(target.stock)) {

            stock.data.raw <- read.csv(target.stock,header=TRUE,sep=",")
            stock.data.xts <- xts(stock.data.raw[,-c(1)], order.by=as.Date(stock.data.raw[,1]))
            Clprs <- Cl(stock.data.xts)
#             if((analyze.group %in% price.limits) && (mean(Clprs, na.rm=TRUE) < 30) ) next
            #check recorders average of 5 years
            if((analyze.group %in% price.limits) && (mean(tail(Clprs,(get.research.period*250)), na.rm=TRUE) < 30) ) next
            }

        m_msg(info=paste('processing', rowid, '.of.', nrow(remix.data), '(', stock.code, ')'))
        Clprs <- Clprs[complete.cases(Clprs), ]
        names(Clprs) <- 'close'
        record.path <- m_paste(c(ma.path, stock.code, list.file.extension), op='')
        trade.summary <- oscillation.indicator(Clprs=Clprs, record.path=record.path, auto.detect=TRUE)

        # resummary for function surge.indicator
        get.period <- sapply(tranSet.period, function(v) ifelse(v <= testSet.period[1], return(v),NA))
        trade.signal <- m_rebind.xts(trade.summary[[1]], get.period)
        stock.ma <- m_rebind.xts(trade.summary[[2]], get.period)
        trigger.action <- trade.summary[[3]]
        indicator.color <- trade.summary[[4]]
        trigger.line <- trade.summary[[5]]
        trigger.mat <- trade.summary[[6]]
        # added indicator
        stock.ma$tension <- stock.ma$brown - stock.ma$red
        stock.ma$shrink <- apply(merge(stock.ma$brown, stock.ma$blue, stock.ma$red), 1,sd)
        stock.ma$shrink.ret <-  ROC(stock.ma$shrink)
        
        ma.nrow <- nrow(stock.ma)
        tmp.stock.data$oscillate.brown[rowid] <- stock.ma$brown[ma.nrow]
        tmp.stock.data$oscillate.blue[rowid] <- stock.ma$blue[ma.nrow]
        tmp.stock.data$oscillate.red[rowid] <- stock.ma$red[ma.nrow]
        tmp.stock.data$oscillate.tenstion[rowid] <- stock.ma$tension[ma.nrow]
        tmp.stock.data$oscillate.shrink[rowid] <- stock.ma$shrink[ma.nrow]
        tmp.stock.data$oscillate.shrink.ret[rowid] <- stock.ma$shrink.ret[ma.nrow]
    }

    remix.data <- merge(remix.data, tmp.stock.data, by='STOCK_CODE') 

    col.num <- ncol(remix.data)
    remix.data <- remix.data[,c(1:3, ((col.num-5):col.num), 4:(col.num-6))]
    #insert 0 into NA
#     remix.data <- remix.data[complete.cases(remix.data),]
    remix.data[is.na(remix.data)] <- 0
    all.colname <- names(remix.data)
    #re-combind column
    all.colname <- c(all.colname[c(1:8)], sort(all.colname[-c(1:8)], decreasing=FALSE))
    remix.data <- remix.data[, all.colname]
    
    ### add analyzing column
        get.research.end <- as.numeric(testSet.period)
        get.research.start <- get.research.end - (get.research.period -1)
        get.header <- c('Clspr.ret.Stdev', 'MaxDrawDOWN', 'RATE', 'RATE.max', 'SHARP_RATIO')
        all.header <- c()
        all.select.header.average <- c()
        all.select.header.weight <- c()
        
        for(header.id in 1:length(get.header))
        {
            for(year.id in get.research.start:get.research.end)
            {
                select.header <- paste(get.header[header.id], as.character(year.id), sep='.')
                tmp.data <- as.data.frame(remix.data[,select.header])
                names(tmp.data) <- select.header
                all.header <- c(all.header, select.header)
                if(year.id == get.research.start)
                {
                    tmp.research <- tmp.data
                    }else{
                    tmp.research <- cbind(tmp.research, tmp.data)
                }        
            }
                select.header.average <- paste(gsub(as.character(year.id), '', select.header), 'average', sep='')
                select.header.weight  <- paste(gsub(as.character(year.id), '', select.header), 'weight', sep='')
                tmp.research[select.header.average] <- apply(tmp.research, 1, mean)
                tmp.research[select.header.weight]  <- apply(tmp.research, 1, function(v) {
                                            weight <- 1:get.research.period
                                            return(sum(v[1:length(get.header)] * weight) / sum(1:length(get.header)) )
                                                    })
                all.select.header.average <- c(all.select.header.average, select.header.average)
                all.select.header.weight  <- c(all.select.header.weight, select.header.weight)
                
            if( header.id == 1 )
            {
                remix.investing.data <- tmp.research
                }else{
                remix.investing.data <- cbind(remix.investing.data, tmp.research)
            }
        
        }
        remix.investing.data <- remix.investing.data[, c(all.select.header.average, all.select.header.weight, all.header)]
        remix.remain.col <- c(  'STOCK_CODE', select.col.maininfo, 
                                'oscillate.brown', 'oscillate.blue', 'oscillate.red', 
                                'oscillate.tenstion', 'oscillate.shrink', 'oscillate.shrink.ret')
#         remix.data <- cbind(remix.data[,c('STOCK_CODE','STOCK_NAME','GROUP')], remix.investing.data)
        remix.data <- cbind(remix.data[, remix.remain.col], remix.investing.data)
    ###
    rownames(remix.data) <- NULL
    #saveing...
    remix.filename <- m_paste(c(as.character(env.config[analyze.group, 'remix']), 'remix.', analyze.group, '.', m_gsub(c(analyze.group, '.'),c('',''), get.research.end, fix=TRUE), '_', as.character(as.Date(Sys.time())), '.csv'), op="")
    write.csv(remix.data, file=remix.filename )

}

stop()       
    # for performance
    selected.cols <- c('STOCK_CODE', 'STOCK_NAME', 'RATE.2019')
    selected.listname <- remix.filename
    selected.stock.raw <- read.csv(file=selected.listname, header=TRUE, sep=',')
    backtest_num <- c(nrow(selected.stock.raw), 5)
    selected.stock <- selected.stock.raw[, selected.cols]
    selected.stock$STOCK_CODE <- as.character(selected.stock$STOCK_CODE)
    
    #
    backtest.result <- m_performance.test(selected.stock=selected.stock, stock.data.path=data.path, testSet_period=testSet.period, tranSet_period=tranSet.period, trading.straregy_type=c(2,80,20,80,20), ef_goal_return=0.001, enable_reandom_stocks=FALSE, backtest_num=backtest_num, enable_fund_allocation=TRUE, ef_simulation_num=50, enable_ef_simulation.record=TRUE) 

    #
    all.stock.name <- backtest.result[["all.stock.name"]]
    names(all.stock.name) <- c("STOCK_CODE","STOCK_NAME")
    all.stock.tranSet.price <- backtest.result[["tranSet_data"]]
    all.stock.ret <- na.omit(backtest.result[["all.stock.ret"]])
    all.stock.traderet <- na.omit(backtest.result[["all.stock.traderet"]])
    all.return <- backtest.result[["all.return"]]
    all.assets <- backtest.result[["all.assets"]]
    all.sim.assets <- na.omit(backtest.result[["all.sim.assets"]])
    trading.straregy <- backtest.result[["trading.straregy"]]
    
    #Singal Stock performance
    for (i in 1:length(index(all.stock.name))) {
            windows()
            backtest(all.stock.ret[,i],all.stock.traderet[,i],paste(all.stock.name[i,1],all.stock.name[i,2],sep=""))
        }
    
    #group trading performance
    title <- m_paste(c(ifelse("(F.A.)",""),"BACKTEST of Stock: ",m_paste(c(as.character(all.stock.name$STOCK_NAME)),op=" ")),op="")
                                
        windows()
        backtest(all.return[,1], all.return[,2], title)
        windows()
        backtest(all.return[,1], all.return[,3], title)

    #
    windows()
    par(mfrow=c(2,1))
    plot(all.return,col=c("black","blue","green"),main="",sep="")
    plot(all.assets,col=c("black","blue","green"),main="",sep="")

    summary(all.return)
    summary(all.assets)

    windows()
    t1 <- "FA.enable /"
    t2 <- m_paste(trading.straregy,":")
    tm <- m_paste(c("Strategy.profit_RATE v.s. Random.profit_RATE (",t1,t2,")"))
    plot.xts(merge(all.assets, all.sim.assets),col=c("black","blue","green",rep("lightgray",100)),screens=1,main=tm)


    
    
    
