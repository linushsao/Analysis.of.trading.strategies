#    na.omit(), na.approx(), na.fill()
rm(list= ls())

library(fTrading)
library(TTR)
library(xts)
library(quantmod)
library(fPortfolio)
library(PerformanceAnalytics)
library(roxygen2)
#local devel fubction
setwd("/home/linus/ProjectStock/ExtraPackages/linus/stock.Analyze")
roxygenize()
library("stock.Analyze")
#

setwd("/home/linus/ProjectStock/all_stocks")
# file for Analyze
test_period <- c("2015-01-01::2015-12-31")
file_name_csv <- env(name="backtest.name",mode="r")

#basic options
enable_debug_mode <- FALSE          #是否(TRUE/FALSE)使用除錯模式，將以testing data取代
#
enable_latestPrice<- FALSE          #是否(TRUE/FALSE)從網路下載股票資料
backtest_num <- 5 #取報酬率前N名的股票編號

enable_reandom_stocks <- FALSE       #是否(TRUE/FALSE)使用亂數選擇一定支數股票為實驗組TESTING GROUP
enable_control_group <- TRUE        #是否(TRUE/FALSE)使用亂數對照組
enable_singal_chart <- FALSE        #是否(TRUE/FALSE)顯示個別股票分析圖表
enable_simple_chart <- FALSE        #只顯示走勢圖，無回測圖及資料表格

trading.straregy_type <- c(2,80,20,80,20)  #(交易策略1=KD based R,2=KD based KDvalue,3=KD based KDJvalue,其他交易策略參數)

enable_fund_allocation <- TRUE     #是否(TRUE/FALSE)使用資產配置理論
ef_goal_return <- 0.3               #效率前緣做資產配置之預期報酬率

enable_ef_simulation <- TRUE        #快速模擬n種配置
ef_simulation_num <- 100            #快速模擬n種配置之次數

#
if( enable_debug_mode ){
    s = 2
    data <- testing_data(period="2014-01-01/2015-04-30")
#    data <- testing_data(period="2014-01-02/2014-10-10")
    KDtrade <- stock.CumulativeRate(data,strategy_type=s)
    Ret <- KDtrade[,1]
    tradeRet <- KDtrade[,2]
    windows()
    backtest(Ret,tradeRet,title=paste("DEBUG_",s))

    stop()
}

#
#Data prepare
data_RAW_ALL <- read.csv(file_name_csv,header=TRUE)
names(data_RAW_ALL) <- c("INDEX","STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE")
#GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
all_stock_name <- as.vector(data_RAW_ALL$STOCK_CODE)
all_stock_chinesename <- as.vector(data_RAW_ALL$STOCK_NAME)
all_stock_lastclose <- as.vector(data_RAW_ALL$LAST_CLOSE)
all_stock_rate <- as.vector(data_RAW_ALL$RATE)

if(enable_reandom_stocks) {
backtest_period <- sample(length(all_stock_name),backtest_num) #取n名亂數對照組的股票來測試
}else
{
backtest_period <- c(1:backtest_num); 
}

select_stock <- all_stock_name[backtest_period] #選股矩陣
select_stock_lastclose <- all_stock_lastclose[backtest_period] #最新收盤價矩陣
select_stock_rate <- all_stock_rate[backtest_period] #報酬率矩陣

all_RET <- xts()
all_tradeRET <- xts()
all_tradeRET_weight <- xts()
all_stock_closeprice <- xts()


for(i in 1:length(select_stock)){
#DATA prepare
    stock_name <- select_stock[i]
    file_name_csv <-  gsub(" ","",paste(stock_name,".csv"))   
        if (enable_latestPrice) {
        data_RAW <- getSymbols(file_name_csv ,auto.assign=FALSE);
        write.zoo(data_RAW,file=file_name_csv,sep=",");
        }
    #file_name <-  gsub(" ","",paste(stock_name,sub_name))
    data_RAW <- read.csv(file_name_csv,header=TRUE) 

    names(data_RAW) <- c("Index","Open","High","Low","Close","Volume","Adjusted")
    data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))

    trade_type <- trading.straregy_type[1]
    trade_param <- trading.straregy_type[2:length(trading.straregy_type)]

    KDtrade <- stock.CumulativeRate(data,strategy_type=trade_type,trade_trigger=trade_param)[test_period[1]]
    close <- data$Close[test_period[1]]

 #

Ret <- KDtrade[,1] 
tradeRet <- KDtrade[,2]

if (enable_singal_chart){
windows()
backtest(Ret,tradeRet,stock_name)
}
#record all rate of stocks
return_ret <- Ret
names(return_ret) <- stock_name
return_traderet <- tradeRet
names(return_traderet) <- stock_name
names(close) <- stock_name

all_RET <- cbind(all_RET,return_ret) #raw rate
all_tradeRET <- cbind(all_tradeRET,return_traderet) #relatived to KDSignal
all_stock_closeprice <- cbind(all_stock_closeprice,close)

}
#End of Function

names(all_stock_closeprice) <- select_stock

temp <- xts(apply(all_RET,1,sum),order.by=index(all_RET)) / length(select_stock) #average price of BuyHold
names(temp) <- "AVERAGE"
#temp <- exp(cumsum(temp))
BuyHold <- merge(all_RET,temp) #買後放著至期末

temp <- xts(apply(all_tradeRET,1,sum),order.by=index(all_tradeRET)) / length(select_stock) #average price of after KDTrade
names(temp) <- "AVERAGE"
#temp <- exp(cumsum(temp))
Trading.straregy_method <- merge(all_tradeRET,temp) #加上使用交易策略（此為KD)

#使用資產配置理論(效率前緣)
if (enable_fund_allocation) {
    #all_tradeRET_weight <- all_tradeRET * portfolio_weight
    portfolio_weight <- ef.fund.Allocation(all_stock_closeprice,ef_goal_return)
    all_tradeRET_weight <- all_tradeRET %*% portfolio_weight
    }else{
    all_tradeRET_weight <- all_tradeRET
}

fund.Allocation_method <- xts(apply(all_tradeRET_weight,1,sum),order.by=index(all_tradeRET)) / length(select_stock) #average price of after KDTrade and adjusted by price
names(fund.Allocation_method) <- "AVERAGE" #加上使用資產配置理論

all_returen <- na.omit(merge(BuyHold$AVERAGE,Trading.straregy_method$AVERAGE,fund.Allocation_method$AVERAGE))
names(all_returen) <- c("BuyHold","Trading.straregy_method","fund.Allocation_method")

title <- m_paste(c(ifelse(enable_fund_allocation,"(F.A.)",""),"BACKTEST of Stock:",as.character(m_paste(all_stock_chinesename[backtest_period]))),op="")
    
if (enable_simple_chart){
    windows()
    backtest(all_returen$BuyHold,all_returen$Trading.straregy_method,title)
    windows()
    backtest(all_returen$BuyHold,all_returen$fund.Allocation_method,title)
}

asset_1 <- exp(cumsum(all_returen$BuyHold))
asset_2 <- exp(cumsum(all_returen$Trading.straregy_method))
asset_3 <- exp(cumsum(all_returen$fund.Allocation_method))

all_assets <- merge(asset_1,asset_2,asset_3)

temp <- c()
for(i in 1:dim(all_returen)[2]) { 
    temp[i] <- round( sd(all_returen[,i]), digits = 4)
    }
sdd <- m_paste(temp,op="/ ")

temp <- c()
for(i in 1:dim(all_returen)[2]) { 
    temp[i] <- round( maxDrawDown(all_returen[,i])$maxdrawdown, digits = 4)
    }
mdd <- m_paste(temp,op="/ ")

title_1 <- m_paste(c(ifelse(enable_fund_allocation,"(F.A.)",""),"Interest rate of [ ",m_paste(all_stock_chinesename[backtest_period],op="/ ")," ] / 風險值: ",sdd))
title_2 <- m_paste(c(ifelse(enable_fund_allocation,"(F.A.)",""),"Increasing Rate of total assets [ ",m_paste(all_stock_chinesename[backtest_period],op="/ ")," ] / 最大回徹: ",mdd))

windows()
par(mfrow=c(2,1))
plot(all_returen,col=c("black","blue","green"),main=title_1,sep="")
#legend("topright",legend=c("BuyHold","Trading.straregy_method","fund.Allocation_method"),col=c("black","red","green"),lty=c(1,1,1))
plot(all_assets,col=c("black","blue","green"),main=title_2,title,sep="")
#legend("topright",legend=c("BuyHold","Trading.straregy_method","fund.Allocation_method"),col=c("black","red","green"),lty=c(1,1,1))

if (enable_simple_chart){
    View(tail(all_returen,20))
    View(tail(cumsum(all_returen),20))
    View(tail(all_assets,20))
}

summary(all_returen)
summary(all_assets)


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
        sim_ret <- (all_tradeRET ) %*% weight
        return(sim_ret)
        })

    sim_return <- xts(sim_return,order.by=index(all_tradeRET))
    sim_cum_return <- cumprod( 1+sim_return )

    windows()
    t1 <- ifelse(enable_fund_allocation,"FA.enable /","")
    t2 <- m_paste(trading.straregy_type,":")
    tm <- m_paste(c("Strategy.profit_RATE v.s. Random.profit_RATE (",t1,t2,")"))
    main.set <- merge(asset_1 ,merge(asset_2,asset_3))
    plot.xts(merge(main.set,sim_cum_return),col=c("block","blue","green",rep("lightgray",100)),screens=1,main=tm)

}

# if (enable_simple_chart){
windows()
pairs(as.data.frame(coredata(all_stock_closeprice[,1:backtest_num])))
# }
View(cor(na.omit(all_stock_closeprice)))

