rm(list = ls())

LIBRS <- c('quantmod','stringr','fTrading','xts','TTR')
sapply(LIBRS,library,character.only=TRUE)

setwd("/home/linus/ProjectStock/all_stocks/")

#CONFIG
#selected_period <- "2019-08-01::2019-12-31" #欲估計股價的時期
selected_period <- "2018" #欲估計股價的時期

#
stock_ignore <- c("1729","2025","2384","6131","9106") #忽略不計的股票編號
safe_rate <- 0.015 #無風險投資報酬率,此為定存利率

#讀入所有股票的編號
m_data1 <- read.csv("all_codes.csv", header=TRUE, sep=",")
#m_data2 <- as.data.frame(c('0050','0056'))
#names(m_data2) <- 'STOCK_CODE'

#m_data <- rbind(m_data1,m_data2)
m_data <- m_data1

#head(m_data1)
#head(m_data2)
#head(m_data)
#tail(m_data)

name <- paste(m_data[,1],"TW", sep = ".") 
name_c <- as.vector(m_data[,2])
name_group <- as.vector(m_data[,5])
name_type <- as.vector(m_data[,6])
#head(name)
#tail(name)
#write.zoo(name,file="temp.csv",sep=",")

#for test
#for(i in 1:length(name)) {
#  name[i]-> symbol
#  file_name <- paste(symbol,"csv", sep = ".") 	
#  write.table(c(1:10), file = file_name, sep = ",")
#}
KK_RAW <- c()

for(i in 1:length(name)) {
  name[i]-> symbol
  file_name <- paste(symbol,"csv", sep = ".") 	
  tryit <- try(read.csv(file_name,header=TRUE))
  
  if(inherits(tryit, "try-error") | (gsub(".TW","",symbol) %in% stock_ignore) ){
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

    temp <- c(symbol,name_c[i],data_1$Close[l],return$Close[l],name_type[i],name_group[i],mdd$maxdrawdow,((return$Close[l]-1-safe_rate)/mdd$maxdrawdown))
    KK_RAW <- rbind(KK_RAW,temp)
    }
}
}

file_name <- paste(selected_period,"_ANALYZE.csv",sep="")
names(KK_RAW) <- c("INDEX","STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO")
write.zoo(KK_RAW,file=file_name,sep=",")
#View(KK_RAW)



