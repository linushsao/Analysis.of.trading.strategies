rm(list = ls())

LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2')
sapply(LIBRS,library,character.only=TRUE)

#local devel fubction
setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
roxygenize()
library("stock.Analyze")

#stocks data path
setwd("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/")
#CONFIG
generate.debug.data <- as.logical(m_env(name="generate.debug.data",mode="r"))
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")

all.code.List <- env(name="all.code.List",mode="r")
prefix.stock.extension <- env(name="prefix.stock.extension",mode="r")
prefix.raw.data.name <- env(name="prefix.raw.data.name",mode="r")
prefix.debug.data.name <- "debug."
raw.data.Listname <- env(name="raw.data.Listname",mode="r")
debug.data.Listname <- "debug.data.List.csv"
select.tranSet.period <- c("2019-01-01::2019-04-30","2019-08-01::2019-12-31","2019","2018","2017")
debug.selected.stock.name <- c("600000.ss","600016.ss","600018.ss","600028.ss","600048.ss")
safe_rate <- 0.015 #無風險投資報酬率,此為定存利率
raw.data.List <- c()


# main function

if (generate.debug.data) {

        KK_RAW <- c()
        
        name <- debug.selected.stock.name
        name_c <- debug.selected.stock.name
        name_group <- "for.Debug"
        name_type <- "for.Debug"
         
        for(i in 1:length(name)) {
            name[i]-> symbol
            file_name <- paste(symbol,"csv", sep = ".") 	
            
            tryit <- try(read.csv(file_name,header=TRUE))
            
            if(inherits(tryit, "try-error") ){
                i <- i+1
            } else {
                data_RAW <- read.csv(file_name,header=TRUE)
                data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))
                names(data) <- c("Open","High","Low","Close","Volume","Adjusted")
                
                l <- length(data)
                if( l == 0 ){
                i <- i+1
                } else {
                data_1 <- data
                return_1 <- na.omit(ROC(data_1$Close))
                return <- exp(cumsum(return_1))
                mdd = maxDrawDown(cumsum(return_1))
                l <- length(return)

                temp <- c(symbol,name_c[i],data_1$Close[l],return$Close[l],name_type,name_group,mdd$maxdrawdow,((return$Close[l]-1-safe_rate)/mdd$maxdrawdown),as.character(index(data)[1]))
                KK_RAW <- rbind(KK_RAW,temp)
                browser()
                }
            }
        }

        debug.data.List <- paste(prefix.debug.data.name, name[1], ".csv", sep="")
        KK_RAW <- as.data.frame(KK_RAW)
        names(KK_RAW) <- c("STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO","Record_Start")
        write.zoo(KK_RAW, file=debug.data.List, sep=",")
        
    #save debug file list
    write.csv(debug.data.List,file=debug.data.Listname)

    }else{

        KK_RAW <- c()
        
        if( length(select.tranSet.period) != 0) { #reset raw.data.Listname
            env(name="raw.data.Listname",value=raw.data.Listname,mode="w")
            write.csv(select.tranSet.period,file=raw.data.Listname)
            }
            
        select.tranSet.period <- as.vector(read.csv(raw.data.Listname , header=TRUE, sep=",")[,2])
        stock_ignore <- c("1729","2025","2384","6131","6205","8201","9106") #忽略不計的股票編號
         #讀入所有股票的編號
        m_data1 <- read.csv(all.code.List, header=TRUE, sep=",")
        m_data <- m_data1
        name <- paste(m_data[,1],prefix.stock.extension, sep = "") 
        name_c <- as.vector(m_data[,2])
        name_group <- as.vector(m_data[,5])
        name_type <- as.vector(m_data[,6])

        for( range in 1:length(select.tranSet.period) )  {
            selected_period <- select.tranSet.period[range]
            KK_RAW <- c()
            
            for(i in 1:length(name)) {
                name[i]-> symbol
                file_name <- paste(symbol,"csv", sep = ".") 	
                tryit <- try(read.csv(file_name,header=TRUE))
                
                if(inherits(tryit, "try-error") | (gsub(prefix.stock.extension,"",symbol) %in% stock_ignore) ){
                    i <- i+1
                } else {
                    data_RAW <- read.csv(file_name,header=TRUE)
                    data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))
                    names(data) <- c("Open","High","Low","Close","Volume","Adjusted")
                    
                    l <- length(data[selected_period])
                    if( l == 0 ){
                    i <- i+1
                    } else {
                    data_1 <- data[selected_period]
                    return_1 <- na.omit(ROC(data_1$Close))
                    return <- exp(cumsum(return_1))
                    mdd = maxDrawDown(cumsum(return_1))
                    l <- length(return)

                    temp <- c(symbol,name_c[i],data_1$Close[l],return$Close[l],name_type[i],name_group[i],mdd$maxdrawdow,((return$Close[l]-1-safe_rate)/mdd$maxdrawdown),as.vector(data_RAW$Index)[1])
                    KK_RAW <- rbind(KK_RAW,temp)
                    }
                }
            }

            raw.data.List[range] <- m_paste(c(prefix.raw.data.name,selected_period,".csv"),op="")
            colnames(KK_RAW) <- c("STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO","Record_Start")
            write.zoo(KK_RAW,file=paste(research.path.of.linus, raw.data.List[range],sep=""),sep=",")
            #View(KK_RAW)
        }
    #save raw file list
    write.csv(raw.data.List,file=paste(research.path.of.linus, raw.data.Listname, sep=""))
}






