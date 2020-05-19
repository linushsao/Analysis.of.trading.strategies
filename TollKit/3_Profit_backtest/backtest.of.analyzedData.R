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


    
    
    
