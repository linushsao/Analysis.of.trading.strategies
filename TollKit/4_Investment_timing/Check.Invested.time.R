    rm(list=ls())
    graphics.off() 

    # load.library
    LIBRS <- c('quantmod','stringr','xts','TTR','roxygen2','rlist','PerformanceAnalytics','tseries', 'stats')
    sapply(LIBRS,library,character.only=TRUE)

    setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
    library('roxygen2')
    roxygenize()
    library("stock.Analyze")

    #
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
    setwd(research.path.of.linus)

    trigger.action.mat <- data.frame(
        ind=c('blue','blue','blue' ),
        level=c('green','purple','purple' ),
        row.names=c('index', 'stock', 'etf')
    )
    auto.down <- TRUE
    file.extension <- ".csv"

    ## 
    # stock.custom <- NULL #stock.code
    get.input <- get.users.input() #get basic data of stock/eft/index

    stock.custom <- get.input[1]
    testSet.period <- get.input[2]
    analyze.group <-  get.input[3]
        
    trigger.action.custom <- c('blue','purple')
    #for index: blue over green
    #for stock: blue over (purple | orange)
    #for etf  : blue over (purple | orange)
    #for common: red over orange
    #for Serious global impact(ex. 2020COVID, 2008FinancialCrisis) brown over orange

    data.path <- dataset.MGR(group=c(analyze.group, 'data', request='conf'))
    list.pat <- dataset.MGR(group=c(analyze.group, 'list', request='conf'))
    ma.path <- dataset.MGR(group=c(analyze.group, 'ma', request='conf'))
    data.extension <- dataset.MGR(group=c(analyze.group, 'extension', request='conf'))
    get.research.years <- 5

    #
    price.limits.value <- as.numeric(get.conf(name='price.average.limited'))
    data.length <- 150
    data.end <-as.character(as.Date(Sys.time()))
    data.start <- as.character(as.Date(Sys.time()) - data.length)
    testSet.period <- ifelse(is.null(testSet.period), paste(data.start, data.end, sep='::'), testSet.period)
    code.list <- paste(research.path.of.linus, analyze.group, '.RAW.', as.numeric(testSet.period)-1, file.extension, sep='')

    stock.code.list <- read.csv(code.list, header=TRUE, sep=",")
#     stock.code <- gsub('.TW','',as.character(stock.code.list[,2]))
#     stock.cname <- as.character(stock.code.list[,3])
#     stock.group <-  as.character(stock.code.list[,7])
    stock.code <- ifelse(analyze.group != 'index', gsub('.TW','',as.character(stock.code.list$STOCK_CODE)), as.character(stock.code.list$STOCK_CODE))
    stock.cname <- as.character(stock.code.list$STOCK_NAME)
    stock.group <-  as.character(stock.code.list$GROUP)
    stock.type <- as.character(stock.code.list$TYPE)

    ##

    while(TRUE) {
        
        #stock selecting <<
        if(is.null(stock.custom ) || (stock.custom == '')) {
        
            stock.id <- sample(1:length(stock.code), 1,replace=FALSE)
            if( analyze.group != 'index' ) {
            
                check.stock.code <- m_check.code(stock.code[stock.id])
                }else{
                check.stock.code <- stock.code[stock.id]
                }
            } else {
            check.stock.code <- ifelse(analyze.group != 'index', gsub('.TW', '', stock.custom), stock.custom)
            stock.id <- match(check.stock.code, stock.code)
            }
        target.stock <- m_paste(c(data.path,  check.stock.code, data.extension, file.extension), op="")
        #stock selecting >>

        #data prepare
        if(file.exists(target.stock)) {

            stock.data.raw <- read.csv(target.stock,header=TRUE,sep=",")
            stock.data.raw <- stock.data.raw[complete.cases(stock.data.raw),]
            stock.data.xts <- xts(stock.data.raw[,-c(1)], order.by=as.Date(stock.data.raw[,1]))
            stock.data.monthly <- to.monthly(na.omit(stock.data.xts))
            names(stock.data.monthly) <- names(stock.data.xts)
            Clprs <- Cl(stock.data.xts[testSet.period])
            len.Clprs <- nrow(Clprs)
            if( (analyze.group != 'stock') || (mean(Clprs) > price.limits.value) || (!is.null(get.input))) {break}
            
            }else{
                m_msg(info=paste('file: ',target.stock,' not founded',sep=''))
            }
        }
        
        m_msg(info=paste('processing data',':',target.stock))
        names(Clprs) <- 'close'

        if(is.null(trigger.action.custom)) trigger.action.custom <- as.vector(t(trigger.action.mat[analyze.group,]))
        trade.summary <- oscillation.indicator(Clprs=Clprs, trigger.action=trigger.action.custom)

        # resummary for function surge.indicator <<
        trade.signal <- trade.summary[[1]][testSet.period]
        stock.ma <- trade.summary[[2]][testSet.period]
        trigger.action <- trade.summary[[3]]
        indicator.color <- trade.summary[[4]]
        trigger.line <- trade.summary[[5]]
        trigger.mat <- trade.summary[[6]]
        # resummary for function surge.indicator >>
        
        # added indicator
        stock.ma$tension <- stock.ma$brown - stock.ma$red
        stock.ma$shrink <- apply(merge(stock.ma$brown, stock.ma$blue, stock.ma$red), 1,sd)
        stock.ma$shrink.ret <-  ROC(stock.ma$shrink)
        shrink.stdRet <- m_std.data(stock.ma$shrink.ret)[['data']]
        
        # prepare for presentation
        trade.signal.xts <- trade.signal
        trade.signal.xts <- merge(trade.signal.xts, stock.ma[, trigger.line])
        trade.signal.xts <- merge(trade.signal.xts, stock.ma$clpr.ret)
        trade.signal.xts$trade.signal.xts <- lag(trade.signal.xts$trade.signal.xts, 1)
        trade.signal.xts$trade.signal.xts[is.na(trade.signal.xts$trade.signal.xts)] <- 0
        
        # count for profit
        trade.signal.xts$day.ret <- apply(trade.signal.xts, 1, function(x) return(x[1]*x[3])) #earning per day
        trade.signal.xts$cum.ret <- cumprod( 1 + trade.signal.xts$day.ret )  #cumsum earning
        trade.signal.xts <- merge(trade.signal.xts, Clprs)
        trade.signal.xts <- trade.signal.xts[complete.cases(trade.signal.xts)]

        #saveing...
        ma.filename <- m_paste(c(ma.path, stock.custom, '.', testSet.period, '_', as.character(as.Date(Sys.time())), '.csv'), op="")
        write.csv(stock.ma, file=ma.filename )
        
        #buy in timmer
        if( sum(trade.signal.xts$trade.signal.xts) != 0 )
        {
            buyin.date <- as.character(index(trade.signal.xts$trade.signal.xts[trade.signal.xts$trade.signal.xts == 1][1]))
            buyin.price <- round(Clprs[buyin.date],4)
            }else{
            buyin.date <- ''
            buyin.price <- ''           
        }
        title.basic <- m_paste(c(check.stock.code, data.extension, " ",stock.cname[stock.id],'(',stock.group[stock.id],')',  ' sd:', round(sd(na.omit(ROC(Clprs))), 3),' /mdd:', round(maxdrawdown(exp(na.omit(ROC(Clprs))))$maxdrawdown,3)), op='')
        main.title <- m_paste(c(title.basic,' Buyin Date&Price :',buyin.date,' ',buyin.price, ' (', trigger.action[1], '.over.', trigger.action[2], ')' ), op='')
        
        x11()
        library(lubridate)
        tsdata <- ts( Cl(stock.data.monthly), frequency=12)
        decom <- decompose(tsdata, type='mult')
        plot(decom)
        
        x11()
        library(TSA)
        par.mat <- matrix(c(3,5,5,1,2,4), 3,2)
        layout(par.mat)
        acf.data <- acf(na.omit(ROC(Cl(stock.data.xts))), lag.max=data.length, plot=FALSE)
        pacf.data <- pacf(na.omit(ROC(Cl(stock.data.xts))), lag.max=data.length, plot=FALSE)
        plot(acf.data, main=title.basic)
        plot(pacf.data, main=title.basic)
        per <- periodogram(na.omit(ROC(Clprs)))
            per_df <- data.frame(freq=per$freq, spec=per$spec)
            per.sort <- per_df[order(-per_df$spec),]
            per.sort$seasonal <- 1 / per.sort$freq
        plot(density(per.sort$seasonal), main=m_paste(c('Seasonal :', round(per.sort$seasonal,2)[1:7]), op=' | '))
        barplot(per.sort$seasonal[1:15], names.arg=round(per.sort$seasonal[1:15],2), type='l',main=m_paste(c('Seasonal :', round(per.sort$seasonal,2)[1:7]), op=' | '))
        
        x11()
        par(mfrow=c(3,1))    
        plot(trade.signal.xts$close, main=paste('Graph 1A ', main.title))
        plot(merge(stock.ma$tension, stock.ma$shrink, 0,
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'] ),
            main=paste('Graph 1B ', main.title),col=c('brown','chartreuse4', 'black',m_dupli.vector(rownames(trigger.mat))))
        plot(merge(stock.ma[,-c(1,7:10)], 
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'] ),
            col=c(indicator.color, m_dupli.vector(rownames(trigger.mat))), main=paste('Graph 1C ', main.title))
        
        x11()
        par(mfrow=c(3,1))    
        plot(trade.signal.xts$close, main=paste('Graph 2A ', main.title))
        cum.ret <- trade.signal.xts$cum.ret
        title <- m_paste(c('max: ',c(round(max(cum.ret),3),' / average: ', round(mean(cum.ret),3)), op=""))
        plot(merge(trade.signal.xts[,c(1,5)], shrink.stdRet), col=c('black','green','lightblue'), main=paste('Graph 2B ', title))
        plot(merge(stock.ma[,-c(1,7:10)], 
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'] ),
            lwd=c(rep(1,5), rep(1,4)) , col=c(indicator.color, m_dupli.vector(rownames(trigger.mat))), main=paste('Graph 2C ', main.title ))

        x11()
        par(mfrow=c(3,1))
        plot(merge(stock.ma[,-c(1,7:10)], 
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'],0 ),
            lwd=c(rep(1.5,5), rep(1,4),1) , col=c(indicator.color, m_dupli.vector(rownames(trigger.mat))), main=m_paste(c('Graph 3A ', main.title, ' (', trigger.action[1], ' over ', trigger.action[2], ')'), op=''))
        plot(merge(stock.ma$tension, stock.ma$shrink, 0,
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'] ),
            main=paste('Graph 3B ', main.title),col=c('brown','chartreuse4', 'black',m_dupli.vector(rownames(trigger.mat))))
        plot(merge(stock.ma$shrink.ret, 0, 3, -3), col=c('darkslategrey', 'black', 'darkslategray3', 'darkslategray3'), main=paste('Graph 3C ', main.title))

        x11()
        par(mfrow=c(3,1))    
        plot(trade.signal.xts$close, main=paste('Graph 2A ', main.title))
        cum.ret <- trade.signal.xts$cum.ret
        title <- m_paste(c('max: ',c(round(max(cum.ret),3),' / average: ', round(mean(cum.ret),3)), op=""))
        plot(merge(stock.ma$tension, stock.ma$shrink, 0,
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'] ),
            main=paste('Graph 3B ', main.title),col=c('brown','chartreuse4', 'black',m_dupli.vector(rownames(trigger.mat))))
        plot(merge(stock.ma[,-c(1,7:10)], 
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'] ),
            lwd=c(rep(1,5), rep(1,4)) , col=c(indicator.color, m_dupli.vector(rownames(trigger.mat))), main=paste('Graph 2C ', main.title ))

       x11()
        par(mfrow=c(1,1))
        plot(merge(stock.ma[,-c(1,7:10)], 
            trigger.mat['green','max'], trigger.mat['green','min'], 
            trigger.mat['purple','max'], trigger.mat['purple','min'], 
            trigger.mat['orange','max'], trigger.mat['orange','min'], 
            trigger.mat['deep pink','max'], trigger.mat['deep pink','min'],0 ),
            lwd=c(rep(1.5,5), rep(1,4),1) , col=c(indicator.color, m_dupli.vector(rownames(trigger.mat))), main=m_paste(c('Graph 4A ', main.title, ' (', trigger.action[1], ' over ', trigger.action[2], ')'), op=''))
