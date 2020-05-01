#' A muti_backtestt Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))


muti_backtest <- function(default.testSet.period, stock.data.path, selected.stockname.list=NULL, default.backtest_num=c(50,5),  default.trading.straregy_type=c(2,80,20,80,20), default.ef_goal_return=0.001) {

    #
    prefix.raw.data.name <- m_env(name="prefix.raw.data.name",mode="r")
    # file for Analyze
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")

    #for select stocks
    selected.stock.name_csv <- ifelse(is.null(selected.stockname.list), m_env(name="backtest.name",mode="r"), selected.stockname.list)     #Codelist for chosen stocks
#     debug.selected.stock.name_csv <- c("600000.ss.csv","600016.ss.csv","600018.ss.csv","600028.ss.csv","600048.ss.csv")
    #for TestSET
#     testSet_period <- c(m_env(name="basktest.period",mode="r")) #for TestSET
    testSet_period <- default.testSet.period #for TestSET
    #for TrainSET
    tranSet_period_list  <- c(m_env(name="raw.data.Listname",mode="r"))
    tranSet_period <- as.vector(read.csv(tranSet_period_list,header=TRUE,sep=",")[,c(2)])
    tranSet_period <- m_gsub(c(research.path.of.linus, ".csv", prefix.raw.data.name),c("","",""),tranSet_period) #data format

#   tranSet_period <- c(m_env(name="raw.data.Listname",mode="r")) %>% read.csv(file=. ,header=TRUE,sep=",") %>% .[,2] %>% gsub("RAW.","", .) %>% gsub(".csv","", .) 
    
    
    #basic options
    #1.是否(TRUE/FALSE)使用除錯模式，將以testing data取代
    #2.what test data
    #3.kd type
    enable_debug_mode <- c(FALSE,1,3)   
    #
    enable_latestPrice<- TRUE          #是否(TRUE/FALSE)從網路下載股票資料

    enable_reandom_stocks <- get.conf(name="enable_reandom_stocks" ,default.conf=FALSE) #是否(TRUE/FALSE)使用亂數選擇一定支數股票為實驗組TESTING GROUP
    backtest_num <- get.conf(name="backtest_num" ,default.conf=default.backtest_num) #c(n,m),於報酬率前n名中取m名股票編號
#     enable_control_group <- get.conf(name="enable_control_group" ,default.conf=TRUE)        #是否(TRUE/FALSE)使用亂數對照組
#     enable_singal_chart <- get.conf(name="enable_singal_chart" ,default.conf=TRUE)        #是否(TRUE/FALSE)顯示個別股票分析圖表
#     enable_simple_chart <- get.conf(name="enable_simple_chart" ,default.conf=TRUE)        #只顯示走勢圖，無回測圖及資料表格

    trading.straregy_type <- get.conf(name="trading.straregy_type" ,default.conf=default.trading.straregy_type)  #(交易策略1=KD based R,2=KD based KDvalue,3=KD based KDJvalue,其他交易策略參數)

    enable_fund_allocation <- get.conf(name="enable_fund_allocation" ,default.conf=TRUE)     #是否(TRUE/FALSE)使用資產配置理論
    ef_goal_return <- as.numeric(get.conf(name="ef_goal_return" ,default.conf=default.ef_goal_return))              #效率前緣做資產配置之**每日**預期報酬率

    enable_ef_simulation <- get.conf(name="enable_ef_simulation" ,default.conf=TRUE)        #快速模擬n種配置
    ef_simulation_num <- as.numeric(m_env(name="ef_simulation_num",mode="r"))  #快速模擬n種配置之次數
    enable_ef_simulation.record <- get.conf(name="enable_ef_simulation.record" ,default.conf=TRUE)
    simulation.record.filename <- m_env(name="simulation.record.filename",mode="r")  #模擬配置儲存檔名
    #
    if( as.logical(enable_debug_mode[1]) ){
        if( enable_debug_mode[2] == 1) { 
            s = enable_debug_mode[3]
            data <- dataset.MGR("^GSPC")
            data <- data["2014-01-01::2015-04-30"]
        #    data <- testing_data(period="2014-01-02/2014-10-10")
            KDtrade <- stock.CumulativeRate(data,strategy_type=s)
            Ret <- KDtrade[,1]
            tradeRet <- KDtrade[,2]
            windows()
            backtest(Ret,tradeRet,title=paste("DEBUG_",s))
        }else if(as.logical(enable_debug_mode[2])) {    
        
    ###
            sh_return_original <- dataset.MGR("chap19.3.1")
            head(sh_return_original)
            #sh_return <- xts(sh_return[,-1],order.by=as.Date(sh_return[,1]))
            sh_return_original <- na.omit(sh_return_original)
            sh_return <- na.omit(ROC(sh_return_original))
            head(sh_return_original)
            head(sh_return)
            
            if( FALSE ) { #### EXAMPLE in BOOK for DEBUG <<
                cumreturn <- cumprod(1+sh_return)
                head(cumreturn)
                plot.zoo(sh_return["2009::2013"],main="Daily Return of 6 stocks(Y2009-Y2013)")
                plot.zoo(cumreturn["2009::2013"],main="Cumulative Return of 6 stocks(Y2009-Y2013)")
        #         plot(cumreturn,main="Cumulative Return of 6 stocks(Y2009-Y2013)",col=c("red","blue","green","black","orange"))

                (cor(sh_return))
                pairs(data.frame(sh_return),pch=20,col="darkblue",main="Correlations among 5 stocks Daily Returns")

                #
                min.var <- function(r_set,goal_return){

                    n <- dim(r_set)[2]
                    Q <- cov(r_set)
                    r <- apply(r_set,MARGIN=2,FUN=mean)
                    L1 <- cbind(Q,rep(1,n),r)
                    L2 <- rbind(c(rep(1,n),0,0),c(r,0,0))
                    L <- rbind(L1,L2)
                    b <- c(rep(0,n),1,goal_return)

                    solve.res <- solve(L,b)
                    #write.zoo(r_set,file="r_set.csv",sep=",")
                    #write.zoo(goal_return,file="goal_return.csv",sep=",")
                    #write.zoo(solve.res,file="solve_res.csv",sep=",")
                    #write.zoo(L,file="L.csv",sep=",")
                    #write.zoo(b,file="b.csv",sep=",")

                    wt <- solve.res[1:n]
                    return_mean <- r %*% wt
                    return_variance <- wt %*% Q %*% wt
                    return(c(return_mean,return_variance,wt))

                }

                    step <- seq(-0.0002,0.0012,by=0.000002)
                    frontier_curve <- t(sapply(step,FUN <- function(goal_return) min.var(sh_return,goal_return)))
                    head(frontier_curve)

                    plot(frontier_curve[,2:1],main="Frontier Curve",xlab="Variance",ylab="Goal Return",col="blue")
                    #plot(solve_RES[,2:1],main="My Frontier Curve",xlab="Variance",ylab="Goal Return",col="blue")
                    train_set <- sh_return["2009-01-01/2012-12-31"]
                    test_set <- sh_return["2013-01-01/2013-03-31"]
                    head(train_set)
                    head(test_set)

                    goal_return <- 0.001

                    portfolio <- min.var(train_set,goal_return)
                    head(portfolio)
                    portfolio_weight <- portfolio[3:length(portfolio)]
                    portfolio_weight

                    test_return <- test_set %*% portfolio_weight
                    test_return <- xts(test_return,order.by=index(test_set))
                    head(test_return)
                    test_cum_return <- cumprod(1+test_return)
                    plot.xts(test_cum_return)

                    L <- 100
                    sim_weight <- t(sapply(1:L,FUN=function(i){
                        weight <- runif(5,min=0,max=1)
                        weight <- weight/sum(weight)
                        return(weight)
                        }))
                    head(sim_weight)

                    sim_return <- sapply(1:L,FUN=function(i){
                        weight <- sim_weight[i,]
                        sim_ret <- test_set %*% weight
                        return(sim_ret)
                        })
                    head(sim_return)
                    sim_return <- xts(sim_return,order.by=index(test_set))
                    sim_cum_return <- cumprod( 1+sim_return )

                    plot.xts(merge(sim_cum_return,test_cum_return),col=c(rep("lightgreen",100),"red"),screens=1)               
            }
            #### EXAMPLE in BOOK for DEBUG >>
            train_set <- sh_return["2009-01-01/2012-12-31"]
            test_set <- sh_return["2013-01-01/2013-03-31"]
            head(train_set)
            head(test_set)

            ef_goal_return <- 0.001

            portfolio <-ef.fund.Allocation(train_set,ef_goal_return)

                    head(portfolio)
                    portfolio_weight <- portfolio[3:length(portfolio)]
                    portfolio_weight

                    test_return <- test_set %*% portfolio_weight
                    test_return <- xts(test_return,order.by=index(test_set))
                    head(test_return)
                    test_cum_return <- cumprod(1+test_return)
                    plot.xts(test_cum_return)

                    L <- 100
                    sim_weight <- t(sapply(1:L,FUN=function(i){
                        weight <- runif(5,min=0,max=1)
                        weight <- weight/sum(weight)
                        return(weight)
                        }))
                    head(sim_weight)

                    sim_return <- sapply(1:L,FUN=function(i){
                        weight <- sim_weight[i,]
                        sim_ret <- test_set %*% weight
                        return(sim_ret)
                        })
                    head(sim_return)
                    sim_return <- xts(sim_return,order.by=index(test_set))
                    sim_cum_return <- cumprod( 1+sim_return )

                    plot.xts(merge(sim_cum_return,test_cum_return),col=c(rep("lightgreen",100),"red"),screens=1) 
    ###
        }
    }

    #
    #Data prepare
    data_RAW_ALL <- read.csv(selected.stock.name_csv, header=TRUE)
    names(data_RAW_ALL) <- c("INDEX","STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE")
    get.stock_code <- as.vector(data_RAW_ALL$STOCK_CODE)
    get.stock_chinesename <- as.vector(data_RAW_ALL$STOCK_NAME)
    get.stock_lastclose<- as.vector(data_RAW_ALL$LAST_CLOSE)
    get.stock_rate <- as.vector(data_RAW_ALL$RATE)

    if(enable_reandom_stocks) {
    backtestSet_period <- sample(backtest_num[1],backtest_num[2]) #於報酬率前n名中取m名股票編號
    }else
    {
    backtestSet_period <- c(1:backtest_num[2]) #取前n名亂數對照組的股票來測試 
    }

    select_stock <- get.stock_code[backtestSet_period] #選股矩陣
    select_stock_lastclose <- get.stock_lastclose[backtestSet_period] #最新收盤價矩陣
    select_stock_rate <- get.stock_rate[backtestSet_period] #報酬率矩陣

    # main function

    data_RAW <- xts()
    all_RET <- xts()
    all_tradeRET <- xts()
    all_tradeRET_weight <- xts()
    all_stock_closeprice <- xts()

    for(i in 1:length(select_stock)){
        #DATA prepare
        stock_name <- select_stock[i]
        file_name_csv <-  gsub(" ","",paste(stock_name,".csv"))   
            if (enable_latestPrice) {
                z.temp <- m_msg(paste(stock_name," download latest data...",sep=""))
                #test if download successed
                tryit <- try(getSymbols(stock_name  ,auto.assign=FALSE))
                if(! inherits(tryit, "try-error") ){
                    data_RAW <- getSymbols(stock_name  ,auto.assign=FALSE)
                    data_RAW.xts <- na.omit(xts(data_RAW,order.by=as.Date(index(data_RAW))))
                    write.zoo(data_RAW.xts,file=file_name_csv,sep=",")
                    rm(data_RAW.xts)
                }else{
                    z.temp <- m_msg(paste(stock_name," download fail,ignore...",sep=""))
                }
            }

            
            #file_name <-  gsub(" ","",paste(stock_name,sub_name))
            data_RAW <- read.csv(file_name_csv,header=TRUE) #read selected stocks data

            names(data_RAW) <- c("Index","Open","High","Low","Close","Volume","Adjusted")
            data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))

            trade_type <- trading.straregy_type[1]
            trade_param <- trading.straregy_type[2:length(trading.straregy_type)]

    #        KDtrade <- stock.CumulativeRate(data,strategy_type=trade_type,trade_trigger=trade_param)[testSet_period[1]]
            KDtrade <- stock.CumulativeRate(data[testSet_period],strategy_type=trade_type,trade_trigger=trade_param)[testSet_period[1]]        
            close <- data$Close

            Ret <- KDtrade[,1] 
            tradeRet <- KDtrade[,2]
#             if (enable_singal_chart){
#                 windows()
#                 backtest(Ret,tradeRet,stock_name)
#             }
#           #record all rate of stocks
            return_ret <- Ret
            names(return_ret) <- paste(stock_name,"ret",sep="_")
            return_traderet <- tradeRet
            names(return_traderet) <- paste(stock_name,"traderet",sep="_")
            names(close) <- paste(stock_name,"close",sep="_")
            
        #for TestSet
        all_RET <- cbind(all_RET,return_ret) #raw rate,combine all selected stocks
        all_tradeRET <- cbind(all_tradeRET,return_traderet) #relatived to KDSignal

        #for TrainSet
        all_stock_closeprice <- cbind(all_stock_closeprice,close)
    
    }
#     write.csv(KDtrade,file=".check_KDTrade.csv")
    #End of Function
    #default portfolio =1
    # names(all_stock_closeprice) <- select_stock
    BuyHold <- xts(apply(all_RET,1,function(x) sum(x)/length(select_stock)),order.by=index(all_RET))  #買後放著至期末
    names(BuyHold) <- "AVERAGE"

    Trading.straregy_method <-  xts(apply(all_tradeRET,1,sum),order.by=index(all_tradeRET)) / length(select_stock) #加上使用交易策略（此為KD)
    names(Trading.straregy_method ) <- "AVERAGE"

#     write.csv(merge(BuyHold,Trading.straregy_method),file=".check_KDTrade1.csv")

    #使用資產配置理論(效率前緣)
    if (enable_fund_allocation) {
        #for tranSet Data
        tranSet_data <- xts()
        temp_data <- xts()
        for(ii in 1:length(tranSet_period)){
            temp_data  <-  all_stock_closeprice[tranSet_period[ii]] 
            if(ii == 1) { 
                tranSet_data <- temp_data
                }else{
                tranSet_data <- rbind(tranSet_data,temp_data)
                }
        }
        tranSet_data <- na.omit(tranSet_data)
        portfolio <- ef.fund.Allocation(tranSet_data, ef_goal_return)
        portfolio_weight <-  portfolio[3:length(portfolio)]
        all_tradeRET_weight <- all_RET %*% portfolio_weight
        }else{
        all_tradeRET_weight <- all_RET 
        }

        all_tradeRET_weight <- xts(all_tradeRET_weight ,order.by=index(all_tradeRET))
    # fund.Allocation_method <- as.data.frame(all_tradeRET_weight)
    fund.Allocation_method <- all_tradeRET_weight
    names(fund.Allocation_method) <- "AVERAGE" #加上使用資產配置理論

    all_return <- na.omit(merge(BuyHold$AVERAGE,Trading.straregy_method$AVERAGE,fund.Allocation_method$AVERAGE)[testSet_period])
    names(all_return) <- c("BuyHold","Trading.straregy_method","fund.Allocation_method")

#     title <- m_paste(c(ifelse(enable_fund_allocation,"(F.A.)",""),"BACKTEST of Stock:",as.character(m_paste(get.stock_chinesename[backtestSet_period]))),op="")
                                
#     if (enable_simple_chart){
#         windows()
#         backtest(all_return$BuyHold,all_return$Trading.straregy_method,title)
#         windows()
#         backtest(all_return$BuyHold,all_return$fund.Allocation_method,title)
#     }

    asset_1 <- cumprod(1+all_return$BuyHold)
    asset_2 <- cumprod(1+all_return$Trading.straregy_method)
    asset_3 <- cumprod(1+all_return$fund.Allocation_method)

    all_assets <- merge(asset_1,asset_2,asset_3)

#     windows()
#     par(mfrow=c(2,1))
#     plot(all_return,col=c("black","blue","green"),main=title_1,sep="")
#     #legend("topright",legend=c("BuyHold","Trading.straregy_method","fund.Allocation_method"),col=c("black","red","green"),lty=c(1,1,1))
#     plot(all_assets,col=c("black","blue","green"),main=title_2,title,sep="")
#     #legend("topright",legend=c("BuyHold","Trading.straregy_method","fund.Allocation_method"),col=c("black","red","green"),lty=c(1,1,1))

#     if (enable_simple_chart){
#         View(tail(all_return,20))
#         View(tail(cumsum(all_return),20))
#         View(tail(all_assets,20))
#     }
# 
#     summary(all_return)
#     summary(all_assets)


    #simulation for rate of others random portfolio
    if (enable_ef_simulation) {
        L <- ef_simulation_num
        sim_weight <- t(sapply(1:L,FUN=function(i){
            weight <- runif(length(select_stock),min=0,max=1)
            weight <- weight/sum(weight)
            return(weight)
            }))

        sim_return <- sapply(1:L,FUN=function(i){
            weight <- sim_weight[i,]
            sim_ret <- na.omit(all_RET) %*% weight
            return(sim_ret)
            })

        sim_return <- (xts(sim_return,order.by=index(na.omit(all_RET))))
        sim_cum_return <- cumprod( 1+sim_return )

#         windows()
#         t1 <- ifelse(enable_fund_allocation,"FA.enable /","")
#         t2 <- m_paste(trading.straregy_type,":")
#         tm <- m_paste(c("Strategy.profit_RATE v.s. Random.profit_RATE (",t1,t2,")"))
#         main.set <- merge(asset_1 ,merge(asset_2,asset_3))
#         plot.xts(merge(main.set,sim_cum_return),col=c("black","blue","green",rep("lightgray",100)),screens=1,main=tm)

    }

#     # if (enable_simple_chart){
#     windows()
#     pairs(as.data.frame(coredata(all_RET)))
#     # }
    z.cor <- cor(na.omit(all_RET))

    if( enable_ef_simulation.record ) {

        filename <- paste(research.path.of.linus, simulation.record.filename, sep="")
        title <- c("Date", "Strategy", "testSet_period", "tranSet_period", "Stocks", "Portfolio", "Min", "Median", "Max", "Range", "Sd", "Var", "Cor")
        
        if (! file.exists(filename) ) {
            z.temp <- as.data.frame(t(rep(NA,length(title))))
            names(z.temp) <- title
            write.table(z.temp, file=filename, sep=",", row.names=FALSE, col.names=FALSE) 
            } 

            z.file <- read.csv(filename,header=TRUE,sep=",")[,-c(1)] 
            
            all.assets.name <- names(all_assets)
            
            for(i in 1:length(all.assets.name)) {
                
                z.asset <- all_assets[,i]
                
                z.name <- all.assets.name[i]
                z.date <- format(Sys.time(), "%Y-%m-%d")
                z.testSet_period <- m_paste(testSet_period,op=" ")
                z.tranSet_period <- m_paste(tranSet_period,op=" ")
                z.stocks <-  m_paste(c("(",select_stock,")"),op=" ")
                z.portfolio<- m_paste(c("(",portfolio_weight,")"),op=" ")
                z.min <-min(z.asset)
                z.median <- median(z.asset)
                z.max <- max(z.asset)
                z.range<- m_paste(c("(",m_paste(as.vector(range(z.asset)),op=" ~ "),")"),op=" ")
                z.sd <- sd(z.asset)       
                z.var <- var(z.asset)
                z.cor_ <- m_paste(c("(",as.vector(z.cor[1,]),")"),op=" ")
                
                z.temp <- c(z.date, z.name, z.testSet_period, z.tranSet_period, z.stocks, z.portfolio, z.min, z.median, z.max, z.range, z.sd, z.var, z.cor_)
    #             
                if(i == 1) {
                    z.collect <- z.temp
                    }else{
                z.collect <- rbind(z.collect,z.temp)
                    }
                }
                
                z.collect <- as.data.frame(z.collect)
                rownames(z.collect) <- NULL
                names(z.collect ) <- title

                z.file <- rbind(z.file, z.collect)
    #             names(z.file) <- title
                rownames(z.file) <- NULL
                write.csv(z.file,file=filename) 
        }

#     z.cor
    stock.name <- data.frame(select_stock, get.stock_chinesename[backtestSet_period])
    result <- list(all.stock.name=stock.name, trading.straregy=trading.straregy_type, tranSet_data=tranSet_data,all.stock.ret=all_RET,all.stock.traderet=all_tradeRET,all.return=all_return, all.assets=all_assets, all.sim.assets=sim_cum_return)
    return(result)
    
}
# 
