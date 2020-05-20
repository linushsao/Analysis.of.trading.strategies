rm(list=ls())
graphics.off() 
#
LIBRS <- c('quantmod','xts','TTR','roxygen2','PerformanceAnalytics','forecast','astsa','TSA','tseries','rlist')
sapply(LIBRS,library,character.only=TRUE)
setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")

wd.dir <- "/home/linus/Project/0_Comprehensive.Research/"
setwd(wd.dir)

#
period <- '2020'
transet.years <- 5
price.year.limited <- 2
price.average.limited <- 35
indicator.Rescale <- c('roc') #roc / std
# stock.code.custom <- paste0('6230', '.TW')
stock.code.custom <- NULL

while(TRUE)
{
    if(! is.null(stock.code.custom))
    {
        stock.code <- stock.code.custom
        break
    }
    sample.stock <- list()
    sample.stock$all.list <- dataset.MGR(group=c("stock",'list'), request='info')
    list.id <- sample(nrow(sample.stock$all.list), 1)
    sample.stock$code <- paste0(sample.stock$all.list[list.id, 1], '.TW')
    sample.stock$name <- sample.stock$all.list[list.id, 2]
    stock.code <- sample.stock$code
    sample.stock$price <- dataset.MGR(dataset.name=stock.code, group=c('stock', 'data'), request='info')
    
    row.length <- nrow(sample.stock$price)
    price.average <- mean(na.omit(sample.stock$price[,5][row.length-(250*price.year.limited): row.length]))
    print(paste(sample.stock$code, sample.stock$name, price.average))
    if( (price.average > price.average.limited)) break
}

#system configure
t.stock <- dataset.MGR(dataset.name=stock.code, group=c('stock', 'data'), request='info')
t.xts.raw <- xts(t.stock[,-c(1)], order.by=as.Date(t.stock$Index))
names(t.xts.raw) <- c("Open","High","Low","Close","Volume","Adjusted")
t.xts.raw <- t.xts.raw[complete.cases(t.xts.raw),]

if(indicator.Rescale == 'std') t.xts <- m_std.data(t.xts.raw)[['data']]
if(indicator.Rescale == 'roc') t.xts <- ROC(t.xts.raw)

t.xts <- t.xts[complete.cases(t.xts), ]

ma.trend.data <- t.xts.raw$Close
ma.VALUE <- c(3,5,10,20)
select.col.ma <- c()
select.col.ma.s <- c()
for(ma.id in 1:length(ma.VALUE))
{
    ma.value <- ma.VALUE[ma.id]
    tmp <- m_smaCal(t.xts.raw$Close, n=ma.value, increase=FALSE)$samCal
    tmp.s <- m_smaCal(t.xts$Close, n=ma.value, increase=FALSE)$samCal
    name.tmp <- paste0('ma_', ma.value)
    name.tmp.s <- paste0('ma.s_', ma.value)
    names(tmp) <- name.tmp
    names(tmp.s) <- name.tmp.s
    ma.trend.data <- merge(ma.trend.data, tmp, tmp.s, all=T)
    
    select.col.ma <- c(select.col.ma, name.tmp)
    select.col.ma.s <- c(select.col.ma.s, name.tmp.s)
}
rm(list=c('tmp', 'tmp.s'))

all.ma <- ma.trend.data[, select.col.ma]
all.ma.s <- ma.trend.data[, select.col.ma.s]
#count adventure indicator
ma_shrink <- xts(apply(all.ma.s, 1, sd), order.by=(index(all.ma.s)))
ma_shrink <- ma_shrink[complete.cases(ma_shrink), ]
ma_tension <- all.ma.s$ma.s_20 - all.ma.s$ma.s_3 ; names(ma_tension) <- 'ma_tension'
ma_tension <- ma_tension[complete.cases(ma_tension), ]
ma_momentum <- cumsum(ma_tension)
ma_momentum.ret <- ROC(m_std.data(ma_momentum)[['data']])

#starting benefit counting
t.return <- t.xts.raw$Close
t.return$ret <- ROC(t.return$Close)
t.return$trade.signal  <- ifelse(ma_momentum.ret>0, 0, 1)

#using signal filter
t.return$trade.signal.softmax <- signal.filter(t.return$trade.signal, max.days=3)
t.return <- t.return[complete.cases(t.return), ]
t.return$trade.dailyEarn <- (t.return$ret * t.return$trade.signal.softmax)

result <- t.return$trade.dailyEarn[period]
result$return <- cumprod(1 + (result$trade.dailyEarn[period]))

write.zoo(merge(t.return, result$return, ma_shrink, ma_tension, all.ma, ROC(all.ma$ma_20)), file='test.csv')

x11()
chartSeries(t.xts.raw[period], up.col='red', dn.col='green') #data.from: RAW, others from scale
addTA(all.ma[period], col=c("red",'blue',"green","darkgray"), on=1)
addTA(merge(ma_tension, ma_shrink, 0, 0.005,-0.005,0.01,-0.01,0.05, -0.05)[period],col=c('blue', 'red', 'gray48', rep('gray1',2), rep('purple4',4)))
addTA(merge(t.return$trade.signal,na.omit(ROC(ma_shrink)/500), 0)[period], col=c('green', 'blue', 'gray'))
addTA(result$return, col='blue')

if(! is.null(stock.code.custom))
{
    stock.list <- dataset.MGR(group=c("stock",'list'), request='info')
    tmp.code <- c(gsub('.TW', '', stock.code.custom))
    tmp.name <- apply(stock.list, 1, function(v) return(ifelse(v[1]==tmp.code, v[2], NA)))
    tmp.name <- tmp.name[!is.na(tmp.name)]
    sample.stock <- data.frame(code=tmp.code, name=tmp.name)
}
title <- paste0(sample.stock$code, sample.stock$name, ' sd :', round(sd(na.omit(t.return$ret[as.character(as.numeric(period)-1)])),4))

x11()
par(mfrow=c(3,2))
plot(t.return$Close[period], main=title, col='red')
plot(t.return$Close[period], main=title, col='red')
plot(merge(t.return$trade.signal.softmax, t.return$trade.signal*0.5)[period], col=c('blue', 'red'))
plot(merge(ma_momentum, ma_tension, ma_shrink, 0)[period], col=c('purple', 'green', 'blue', 'black'))
plot(result$return, col='purple',main=paste0('max: ', round(max(result$return),4),' average: ', round(mean(result$return), 4)))
plot(merge(t.return$trade.signal*0.1, ma_momentum.ret, 0)[period], col=c('red', 'cyan3', 'black'))
