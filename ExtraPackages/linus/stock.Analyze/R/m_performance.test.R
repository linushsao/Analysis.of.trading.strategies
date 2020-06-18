#' A muti_backtestt Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_performance.test <- function( selected.list=NULL, 
                                testSet_period=NULL, tranSet_period=NULL, trading.straregy_type=c(2,80,20,80,20), 
                                ef_goal_return=0.001, 
                                enable_reandom_stocks=FALSE, enable_fund_allocation=TRUE, ef_simulation_num=50, enable_ef_simulation.record=TRUE, 
                                memo='',
                                job.id=NULL) {
    #basic options
    #1.是否(TRUE/FALSE)使用除錯模式，將以testing data取代
    #2.what test data
    #3.kd type

    #enable_fund_allocation 是否(TRUE/FALSE)使用資產配置理論
    #trading.straregy_type (交易策略1=KD based R,2=KD based KDvalue,3=KD based KDJvalue,其他交易策略參數)
    #ef_goal_return 效率前緣做資產配置之**每日**預期報酬率
    #ef_simulation_num 快速模擬n種配置之次數
    #enable_ef_simulation.record 儲存亂數配置結果
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
    simulation.record.filename <- m_env(name="simulation.record.filename",mode="r")  #模擬配置儲存檔名
    #
    #Data prepare
    get.stock_code <- selected.list[,1]
    get.stock_chinesename <- selected.list[,2]

    # main function
    data_RAW <- xts()
    all_RET <- xts()
    all_tradeRET <- xts()
    all_tradeRET_weight <- xts()
    all_stock_closeprice <- xts()
    for(rowid in 1:nrow(selected.list)){
        #DATA prepare
        stock_name <- paste(selected.list[rowid, 1], selected.list[rowid, 2])
        file_name_csv <-  as.character(selected.list[rowid, 3])
        data_RAW <- read.csv(file_name_csv,header=TRUE) #read selected stocks data
        data_RAW <- data_RAW[complete.cases(data_RAW),]
        names(data_RAW) <- c("Index","Open","High","Low","Close","Volume","Adjusted")
        data <- xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index))

        trade_type <- trading.straregy_type[1]
        trade_param <- trading.straregy_type[2:length(trading.straregy_type)]

        KDtrade <- stock.CumulativeRate(data[testSet_period],strategy_type=trade_type,trade_trigger=trade_param)[testSet_period[1]]        
        close <- data$Close

        Ret <- KDtrade[,1] 
        tradeRet <- KDtrade[,2]
        #record all rate of stocks
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
            
    #End of Function
    #default portfolio =1
    BuyHold <- xts(apply(all_RET,1,function(x) sum(x)/length(get.stock_code)),order.by=index(all_RET))  #買後放著至期末
    names(BuyHold) <- "AVERAGE"

    Trading.straregy_method <-  xts(apply(all_tradeRET,1,sum),order.by=index(all_tradeRET)) / length(get.stock_code) #加上使用交易策略（此為KD)
    names(Trading.straregy_method ) <- "AVERAGE"

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
    fund.Allocation_method <- all_tradeRET_weight
    names(fund.Allocation_method) <- "AVERAGE" #加上使用資產配置理論

    all_return <- na.omit(merge(BuyHold$AVERAGE,Trading.straregy_method$AVERAGE,fund.Allocation_method$AVERAGE)[testSet_period])
    names(all_return) <- c("BuyHold","Trading.straregy_method","fund.Allocation_method")

    asset_1 <- cumprod(1+all_return$BuyHold)
    asset_2 <- cumprod(1+all_return$Trading.straregy_method)
    asset_3 <- cumprod(1+all_return$fund.Allocation_method)

    all_assets <- merge(asset_1,asset_2,asset_3)

    #simulation for rate of others random portfolio
    if (ef_simulation_num != 0) {
        L <- ef_simulation_num
        sim_weight <- t(sapply(1:L,FUN=function(i){
            weight <- runif(length(get.stock_code),min=0,max=1)
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

    }

    z.cor <- cor(na.omit(all_RET))

    if( enable_ef_simulation.record ) {

        filename <- paste(research.path.of.linus, simulation.record.filename, sep="")
        title <- c("Date", 'Id', 'Stock_num', "Strategy", "testSet_period", "tranSet_period", "Stocks", "Portfolio", "Min", "Median", "Max", "Sd", "Var", "Cor", 'Memo')
        
        if (! file.exists(filename) ) {
            z.temp <- as.data.frame(t(rep(NA,length(title))))
            names(z.temp) <- title
            write.table(z.temp, file=filename, sep=",", row.names=FALSE, col.names=FALSE) 
            } 

            z.file <- read.csv(filename,header=TRUE,sep=",")[,-c(1)] 

            all.assets.name <- names(all_assets)
            z.id <- ifelse(is.null(job.id), random_id(bytes = 8), job.id)
            z.stock_num <- length(get.stock_code)

            for(i in 1:length(all.assets.name)) {
                
                z.asset <- all_assets[,i]
                
                z.date <- format(Sys.time(), "%Y-%m-%d")
                z.name <- all.assets.name[i]
                z.testSet_period <- m_paste(testSet_period,op=" ")
                z.tranSet_period <- m_paste(tranSet_period,op=" ")
                z.stocks <-  m_paste(c("(",get.stock_code,")"),op=" ")
                z.portfolio<- m_paste(c("(",portfolio_weight,")"),op=" ")
                z.min <-min(z.asset)
                z.median <- median(z.asset)
                z.max <- max(z.asset)
                z.sd <- sd(z.asset)       
                z.var <- var(z.asset)
                z.cor_ <- m_paste(c("(",as.vector(z.cor[1,]),")"),op=" ")
                z.memo <- memo
                z.temp <- c(z.date, z.id, z.stock_num, z.name, z.testSet_period, z.tranSet_period, z.stocks, z.portfolio, z.min, z.median, z.max, z.sd, z.var, z.cor_, z.memo)
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
                rownames(z.file) <- NULL
                write.csv(z.file,file=filename) 
        }

           
#     z.cor
    stock.name <- data.frame(get.stock_code, get.stock_chinesename)
    result <- list(all.stock.name=stock.name, trading.straregy=trading.straregy_type, tranSet_data=tranSet_data,all.stock.ret=all_RET,all.stock.traderet=all_tradeRET,all.return=all_return, all.assets=all_assets, all.sim.assets=sim_cum_return, portfolio=portfolio, sim.portfolio=sim_weight)
    return(result)
    
}
# 
