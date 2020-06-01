    rm(list=ls())
    par(mfrow=c(1,1))
    graphics.off() 
    #
    LIBRS <- c('quantmod','stringr','xts','TTR','roxygen2','tseries','rlist','lubridate', 'ids', 'PerformanceAnalytics')
    sapply(LIBRS,library,character.only=TRUE)

    setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
    library('roxygen2')
    roxygenize()
    library("stock.Analyze")
    #
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
    setwd(research.path.of.linus)

    #
    #(range.num, choice.num(s)):
    #range.num: 0                -> random choice
    #range.num: 1                -> custom choice
    #range.num: other num        -> random choice in custom range
    
    remove.e.letter <- function(v)
    {   
        le <- c(LETTERS[seq( from = 1, to = 26 )], letters[seq( from = 1, to = 26 )])
        result <- sapply(le, function(x) ifelse(grepl(x, v, fixed = TRUE), str_remove(v, x), NA))
        result <- result[! is.na(result)]
        names(result) <- NULL
        return(result)
    }
    
    while(TRUE)
    {
        user.input <- as.character(get.users.input(prompt='If Processing (U)serInput/(L)ist method ?', index='user.input'))
            if(user.input == 'U' || user.input == 'u') 
            {
                selected.stock <- '0000'
                get.input <- c()
                while(!is.na(as.numeric(remove.e.letter(selected.stock))))
                {
                    selected.stock <- as.character(get.users.input(prompt='Pls input STOCK_CODE(not .TW included): ', index='selected.stock'))
                    if((nchar(selected.stock) >= 4) && !(selected.stock %in% get.input)) 
                    {
                        get.input <- c(get.input, selected.stock)
                    }else{break}
                }
                
                break
            }
            
            if(user.input == 'L' || user.input == 'l') 
            {
                selected.listname.path <- as.character(get.users.input(prompt='Pls input STOCK_CODE List path: ', index='List.Path'))
                selected.backtest.num <- as.character(get.users.input(prompt='Pls Enter selected.backtest.num(range.num, choice.num(s))', index='selected.backtest.num'))
                backtest_num <- as.numeric(unlist(strsplit(selected.backtest.num, ',')))
                break
            }
        
    }
    
    testSet.period <- as.character(get.users.input(prompt='Pls Enter testSet.period', index='testSet.period'))
    tranSet.period <- as.character(get.users.input(prompt='Pls Enter tranSet.period', index='tranSet.period'))
    #data  preProcessing
#     selected.listname.path <- 'remix.stock.2020-05-23.csv'

## Prepare for performanace

    selected.listname.generator <- function(selected.listname.path=NULL , selected.listname=NULL, backtest_num, selected.cols= c('STOCK_CODE', 'STOCK_NAME'), stock.extension='.TW', backtest.file.extension='.csv')
    {
        if(! is.null(selected.listname.path))
        {
            selected.listname.raw <- read.csv(selected.listname.path, header=TRUE, sep=',')[, selected.cols]
            selected.listname.raw <- selected.listname.raw[selected.listname.raw != '', ]
            selected.listname.raw <- selected.listname.raw[complete.cases(selected.listname.raw),]
        }else if(! is.null(selected.listname)) 
        {   
            selected.listname.raw <- data.frame(STOCK_CODE=selected.listname)
            all.list.stock <- dataset.MGR(group=c('stock','list'), request='info')[, selected.cols]
            all.list.etf <- dataset.MGR(group=c('etf','list'), request='info')[, selected.cols]
            selected.listname.raw <- merge(selected.listname.raw, merge(all.list.stock, all.list.etf, all.y=FALSE))
        }
        
        selected.listname.raw$STOCK_PATH <- paste0(dataset.MGR(group=c('stock','data'), request='conf'), selected.listname.raw$STOCK_CODE, stock.extension, backtest.file.extension)
        
        if(backtest_num[1] == 0) 
        {                                   #random choice
            backtest_num[1]  <- nrow(selected.listname.raw)
            selected.Subject <- sample(backtest_num[1], backtest_num[2])
            }else if(backtest_num[1] == 1){ #custom choice from index 2
                selected.Subject <- c(1:nrow(selected.listname.raw)) #select all
            }
        selected.listname <- selected.listname.raw[selected.Subject, ]
#         selected.listname <- apply(selected.listname, 2, function(v) return(as.character(v)))
        return(selected.listname)
    }
    
    period.c2n <- function(x)
    {           
            x <- tranSet.period
            tmp.num <- as.numeric(unlist(strsplit(x, '::')))
            return(tmp.num)
    }

#     selected.cols <- c('STOCK_CODE', 'STOCK_NAME')
#     backtest.file.extension <- '.csv'

    if(user.input == 'U' || user.input == 'u')
    {
        selected.listname <- selected.listname.generator(selected.listname=get.input, backtest_num=c(1, length(get.input)))
        stop()
    }else if(user.input == 'L' || user.input == 'l') 
    {
        selected.listname <- selected.listname.generator(selected.listname.path=selected.listname.path, backtest_num=selected.backtest.num, selected.cols=selected.cols)
    }
        job.id <- random_id(bytes = 8)
        memo <- 1

        backtest.result <- m_performance.test(  selected.list=selected.listname, 
                                                testSet_period=testSet.period, tranSet_period=tranSet.period, 
                                                trading.straregy_type=c(3,80,20,80,20), 
                                                ef_goal_return=0.001, 
                                                enable_reandom_stocks=FALSE, enable_fund_allocation=TRUE, 
                                                ef_simulation_num=50, enable_ef_simulation.record=TRUE, 
                                                memo=memo, job.id=job.id) 

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

    windows()
    par(mfrow=c(2,1))
    plot(all.return,col=c("black","blue","green"),main="",sep="")
    plot(all.assets,col=c("black","blue","green"),main="",sep="")

    windows()
    t1 <- "FA.enable /"
    t2 <- m_paste(trading.straregy,":")
    tm <- m_paste(c("Strategy.profit_RATE v.s. Random.profit_RATE (",t1,t2,")"))
    plot.xts(merge(all.assets, all.sim.assets),col=c("black","blue","green",rep("lightgray",100)),screens=1,main=tm)

    summary(all.return)
    summary(all.assets)
    print(backtest.result[['portfolio']])

