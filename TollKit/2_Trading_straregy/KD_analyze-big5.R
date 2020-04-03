
rm(list = ls())

library("quantmod")
library("xts")

setwd("/home/linus/ProjectStock/all_stocks")
#setwd("d:/R") #windows底下執行請拿掉"#"

#2382廣達 /0050台灣50 /^TWII台灣大盤指數 /00677UVIX /00632R
#^GSPC(SP500) ^DJI(道瓊) YM=F(mini道瓊)

#個股參數調整
stock_name <- "9910.TW" 
sub_name <- ""                          #上市.TW / 上櫃 .TWO / 空白表示為指數
to_period <- "2020-03-25"
emable_stock_local <- FALSE              #是否(TRUE/FALSE)使用目前資料,或是從網路下載股票資料 

#選擇性調整
time_period <- "2000::2020"             #月移動線顯示範圍
time_period_daily <- "2015::2020" #日線顯示範圍
count_period <- paste("::",to_period,sep="")

#以下請勿調整===========================================
value_fastK <- 9
value_fastD <- 3
value_slowD <- 3

SHIFT_period <- 20 #monthly
selected_period <- gsub(" ","",paste("::",to_period))
selected_period

file_TWII_name <-  "^TWII"
file_TWII_name_csv <-  gsub(" ","",paste(file_TWII_name,".csv"))
file_name <-  gsub(" ","",paste(stock_name,sub_name))
file_name_csv <-  gsub(" ","",paste(file_name,".csv"))
#Stock data Download & Save data
if (emable_stock_local == FALSE ) {
    data_RAW <- getSymbols(file_name,auto.assign=FALSE);
    data_TWII_RAW <- getSymbols(file_TWII_name,auto.assign=FALSE);
    write.zoo(data_RAW,file=file_name_csv,sep=",");
    write.zoo(data_TWII_RAW,file=file_TWII_name_csv,sep=",");
}
#Read saved stock data
data_RAW <- read.csv(file_name_csv,header=TRUE)
data_TWII_RAW <- read.csv(file_TWII_name_csv,header=TRUE)

#Data washing
names(data_RAW) <- c("Index","Open","High","Low","Close","Volume","Adjusted")
data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))
data <- data[selected_period]

data_TWII <- na.omit(xts(data_TWII_RAW[c(2:7)],order.by=as.Date(data_TWII_RAW$Index)))
data_TWII <- data_TWII[selected_period]

#class(data)
head(data,3)
tail(data,3)

# 準備計算踐芭樂的移動月k ///
KK_RAW <- c()
ndate <- length(index(data)) #計算股票資料總筆數
ndate
j <- ndate
i <- 0
while(j > SHIFT_period){
    Op <- data$Open[j - SHIFT_period + 1]
    Cl <- data$Close[j]
    Hi <- sort(coredata(data$High[(j - SHIFT_period +1):j]))[SHIFT_period]
    Lo <- sort(coredata(data$Low[(j - SHIFT_period +1):j]))[1]
    Vo <- sum(data$Volume[(j - SHIFT_period +1):j])
    Ad <- sum(data$Adjusted[(j - SHIFT_period +1):j])
    In <- index(data[j])
    temp <- c(In,Op,Hi,Lo,Cl,Vo,Ad,j)
    KK_RAW <- rbind(KK_RAW,temp)
    j <- j - SHIFT_period
    i <- i+1
}
head(KK_RAW,3)
tail(KK_RAW,3)
length(KK_RAW)

KK <- as.data.frame(KK_RAW)
names(KK) <- c("Date","Open","High","Low","Close","Volume","Adjusted","Order_Start")
head(KK,3)
K_mon <- na.omit(xts(KK[c(2:7)],order.by=as.Date(KK$Date)))
head(K_mon,3)
tail(K_mon,3)
length(index(K_mon))

ma_5<-runMean(data$Close,n=5)
ma_10<-runMean(data$Close,n=10)
ma_20<-runMean(data$Close,n=20)
ma_60<-runMean(data$Close,n=60)
ma_120<-runMean(data$Close,n=120)

ma_10y<-na.omit(runMean(na.omit(K_mon$Close),n=120)) #大盤10年線

#scale for TWII to fit into chart
s_data <- sort(coredata(Cl(data)[time_period_daily]))
s_data_TW <- sort(coredata(Cl(data_TWII)[time_period_daily]))
l_data <- length(Cl(data)[time_period_daily])
l_data_TW <- length(Cl(data_TWII)[time_period_daily])

scale_n <- (s_data[l_data] ) / (s_data_TW[l_data_TW])
data_TWII_scaled <- Cl(data_TWII)*scale_n

KD_RAW <- na.omit(stoch(HLC(K_mon[count_period]),nFastK = value_fastK, nFastD = value_fastD, nSlowD = value_slowD))
KD_TREND <- KD_RAW$fastK-KD_RAW$fastD
names(KD_TREND) <- "K-D"
head(KD_TREND)

windows()
chartSeries(K_mon[time_period],theme="white",up.col="red",dn.col="green")
addTA(KD_RAW, col=c("red","green","white"))
addBBands()
title(paste(stock_name,"-MONTHLY"),col.main="red")

View(tail(KD_TREND,40))

#stop()

KD_RAW <- na.omit(stoch(HLC(data[paste("::",to_period,sep="")]),nFastK = value_fastK, nFastD = value_fastD, nSlowD = value_slowD))
KD_DAILY <- KD_RAW$fastK-KD_RAW$fastD
names(KD_DAILY) <- "K-D(DAILY)"
head(KD_DAILY)

windows()
chartSeries(data[time_period_daily],theme="white",up.col="red",dn.col="green")
addTA(ma_5,col="sienna",on=1)
addTA(ma_10,col="orange",on=1)
addTA(ma_20,col="purple",on=1)
addTA(ma_60,col="green",on=1)
addTA(ma_120,col="blue",on=1)
addTA(ma_10y,col="red",on=1)

addTA(Cl(data_TWII)*scale_n,col="red")
addTA(KD_RAW, col=c("red","green","grey"))
addBBands()
title(paste(stock_name,"-DAILY"),col.main="red")

# \\\結束







