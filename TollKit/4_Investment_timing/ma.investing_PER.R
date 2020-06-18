rm(list=ls())
par(mfrow=c(1,1))
graphics.off() 
#
LIBRS <- c('quantmod','xts','TTR','roxygen2','PerformanceAnalytics','forecast','astsa','tseries','rlist', 'circlize')
sapply(LIBRS,library,character.only=TRUE)

setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")

wd.dir <- "/home/linus/Project/0_Comprehensive.Research/"
setwd(wd.dir)

#
color.plate <- c('chartreuse', 'chocolate1', 'cyan4', 'darkgoldenrod1', 'green', 'darkolivegreen2', 'gold', 'mediumpurple1')
get.input <- get.users.input() #get basic data of stock/eft/index
if.force.update <- as.logical(get.users.input(prompt='Pls Enter If Force Update(T/F)', index='if.force.update'))
fitler.type <- as.character(get.users.input(prompt='Pls Enter Filter mod( (d)efault / (f)ebo / (p)eri /((c)ustom )', index='fitler.type'))

stock.custom <- get.input[1]
testSet.period <- get.input[2]
analyze.group <- get.input[3]

period <- testSet.period
stock.extension <- dataset.MGR(group=c(analyze.group, 'extension', request='conf'))
get.research.period <- as.numeric(get.conf(name='get.research.period'))
transet.years <- paste0(as.character(as.numeric(period)-get.research.period+1), '::', as.character(as.numeric(period)))
price.year.limited <- 2
price.average.limited <- as.numeric(get.conf(name='price.average.limited'))
indicator.Rescale <- c('roc') #roc / std

stock.custom <- c(stock.custom, stock.extension, analyze.group)
stock.code.custom <- paste0(stock.custom[1], stock.custom[2])
# stock.code.custom <- NULL

while(TRUE)
{
    if(! is.na(stock.custom[1]))
    {
        stock.code <- stock.code.custom
        break
    }
    sample.stock <- list()
    sample.stock$all.list <- dataset.MGR(group=c(stock.custom[3],'list'), request='info')
    list.id <- sample(nrow(sample.stock$all.list), 1)
    sample.stock$code <- paste0(sample.stock$all.list[list.id, 1], '.TW')
    sample.stock$name <- sample.stock$all.list[list.id, 2]
    stock.code <- sample.stock$code
    sample.stock$price <- dataset.MGR(dataset.name=stock.code, group=c(stock.custom[3], 'data'), request='info')
    
    row.length <- nrow(sample.stock$price)
    price.average <- mean(na.omit(sample.stock$price[,5][row.length-(250*price.year.limited): row.length]))
    print(paste(sample.stock$code, sample.stock$name, price.average))
    if( (price.average > price.average.limited)) break
}

#system configure
t.stock <- dataset.MGR(dataset.name=stock.code, group=c(stock.custom[3], 'data'), request='info', force.update=if.force.update)
t.xts.raw <- xts(t.stock[,-c(1)], order.by=as.Date(t.stock[,c(1)]))[transet.years]

if(stock.custom[3] == 'etf')
{
    #to numeric
    t.names <- names(t.xts.raw)
    col.num <- ncol(t.xts.raw)
    for(col.id in 1:col.num) t.xts.raw <- merge(t.xts.raw, as.numeric(t.xts.raw[,col.id]))
    t.xts.raw <- t.xts.raw[,-c(1:col.num)]
    names(t.xts.raw) <- t.names
}
 

names(t.xts.raw) <- c("Open","High","Low","Close","Volume","Adjusted")
t.xts.raw <- t.xts.raw[complete.cases(t.xts.raw),]

if(indicator.Rescale == 'std') t.xts <- m_std.data(t.xts.raw)[['data']]
if(indicator.Rescale == 'roc') t.xts <- ROC(t.xts.raw)

ma.value.generator <- function(Clprs, level=5, drop=0)
{   
    per <- periodogram(na.omit(ROC(Clprs)), plot=FALSE)
    per_df <- data.frame(freq=per$freq, spec=per$spec)
    per.sort <- per_df[order(-per_df$spec),]
    per.sort$seasonal <- 1 / per.sort$freq
    per.sort <- per.sort[(1:level), ]    
    per.sort$seasonal.round <- round(per.sort$seasonal, 0)
    ma.VALUE.per <- sort(unique(per.sort$seasonal.round ))    
    if(drop != 0) ma.VALUE.per <- ma.VALUE.per[ma.VALUE.per < drop]
    rownames(per.sort) <- NULL
    return(list(data=per.sort, ma.value=ma.VALUE.per))
}
# ma.value.raw <- ma.value.generator(Cl(t.xts.raw))
t.xts <- t.xts[complete.cases(t.xts), ]
ma.trend.data <- t.xts.raw$Close
ma.VALUE <- c(3,5,10,20)
if(fitler.type == 'f') ma.VALUE <- m_series(n=8)[-c(1:3)] # F series
if(fitler.type == 'p') 
{
ma.VALUE <- ma.value.generator(Cl(t.xts.raw), drop=40)[['ma.value']]
if(! ma.VALUE >20) ma.VALUE <-c(ma.VALUE, 20)
}
if(fitler.type == 'c')
{
ma.value.raw <- as.character(get.users.input(prompt='Pls Enter ma.value(sep with ,)', index='ma.value.raw'))
ma.VALUE <- as.numeric(unlist(strsplit(ma.value.raw, ",", fixed = TRUE)))    
}

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

# data.clean <- function(data, replace=NA)
# {
# data[is.infinite(data)] <- replace
# return(data)
# }

all.ma <- ma.trend.data[, select.col.ma]
all.ma.s <- ma.trend.data[, select.col.ma.s]
#count adventure indicator
rescale <- function(v)
{
    result <- v
    for(i in 1:ncol(result)) {
        v <- max(abs(result[,i]))
        for(j in 1:nrow(result)) result[j,i] <- result[j,i] / v
    }
    return(result)
}

ncol.all.ma.s <- ncol(all.ma.s)
ma_shrink <- xts(apply(all.ma.s, 1, sd), order.by=(index(all.ma.s)))
ma_shrink <- ma_shrink[complete.cases(ma_shrink), ]
ma_tension <- all.ma.s[, ncol.all.ma.s] - all.ma.s[, 1] ; names(ma_tension) <- 'ma_tension'
ma_tension <- ma_tension[complete.cases(ma_tension), ]
ma_momentum <- cumsum(ma_tension); names(ma_momentum) <- 'ma_momentum'
ma_roc.shrink <- ROC(ma_shrink); names(ma_roc.shrink) <- 'ma_roc.shrink'
ma_roc.shrink <- ma_roc.shrink[complete.cases(ma_roc.shrink),]

#starting benefit counting
t.return <- t.xts.raw$Close
t.return$ret <- ROC(t.return$Close)
t.return$ma_shrink <- rescale(ma_shrink)
t.return$ma_tension <- rescale(ma_tension)
t.return$ma_momentum <- rescale(ma_momentum)
t.return$ma_roc.shrink <- rescale(ma_roc.shrink)
t.return <- t.return[complete.cases(t.return), ]

#fix ma_momentum by ma_momentum.ret
fixed.indicator <- data.frame(t.return$ma_momentum, t.return$ma_tension)
fixed.indicator$ma_tension.logic <- ifelse(fixed.indicator$ma_tension >0 , 1, -1)
fixed.indicator$trade.signal.raw  <- abs(fixed.indicator$ma_momentum) * fixed.indicator$ma_tension.logic

t.return$trade.signal.raw  <- fixed.indicator$trade.signal.raw
t.return$trade.signal  <- ifelse(t.return$trade.signal.raw  < 0, 1, 0)
rm(fixed.indicator)

#using signal filter
t.return$trade.signal.softmax <- signal.filter(t.return$trade.signal, max.days=3)
t.return$trade.dailyEarn <- (t.return$ret * t.return$trade.signal.softmax)

result <- t.return$trade.dailyEarn[period]
result$return <- cumprod(1 + (result$trade.dailyEarn[period]))

# write.zoo(merge(t.return, result$return, all.ma, ROC(all.ma$ma_20)), file=paste0('/home/linus/Project/0_Comprehensive.Research/00_raw.data/', 'ma.investingData_', analyze.group, '.',as.Date(Sys.time()), '.csv'))

x11()
chartSeries(t.xts.raw[period], up.col='red', dn.col='green') #data.from: RAW, others from scale
addTA(all.ma[period], col=c('white', color.plate[sample(length(color.plate), (ncol(all.ma)-2))], 'red'), on=1)
# addTA(all.ma[period], col=c("red",'blue',"green","darkgray"), on=1)
addTA(merge(ma_tension, ma_shrink, 0)[period],col=c('brown','chartreuse4', 'darkgray', rep('gray1',2), rep('purple4',4)))
addTA(merge(ma_momentum, 0)[period], col=c('cyan1','darkgray'))
addTA(result$return, col='blue')

if(! is.na(stock.custom[1]))
{
    stock.list <- dataset.MGR(group=c(stock.custom[3],'list'), request='info')
    string.leng <- as.numeric(dataset.MGR(group=c(stock.custom[3],'string.leng'), request='conf'))
    stock.list$STOCK_CODE <- sapply(as.character(stock.list$STOCK_CODE) , function(v) m_check.code(v, num=string.leng))
    tmp.code <- ifelse((analyze.group != 'index'), gsub('.TW', '', stock.code.custom), stock.code.custom)
    tmp.name <- apply(stock.list, 1, function(v) return(ifelse(v[1]==tmp.code, v[2], NA)))
    tmp.name <- ifelse(length(tmp.name[!is.na(tmp.name)]) == 0, tmp.code, tmp.name[!is.na(tmp.name)])
    sample.stock <- data.frame(code=tmp.code, name=tmp.name)
}
title <- paste0(sample.stock$code, ' ',sample.stock$name, ' sd :', round(sd(na.omit(t.return$ret[as.character(as.numeric(period)-1)])),4),' transet.years: ',get.research.period)

x11()
par(mfrow=c(3,2))
plot(t.return$Close[period], main=title, col='red')
plot(t.return$Close[period], main=title, col='red')
plot(merge(t.return$trade.signal.softmax, t.return$ma_momentum, t.return$trade.signal.raw)[period], col=c('blue', 'cyan4', 'coral1'))
plot(merge(t.return$ma_tension, t.return$ma_momentum, t.return$ma_shrink, 0)[period], col=c('brown', 'cyan4', 'chartreuse4', 'darkgray'))
plot(result$return, col='purple',main=paste0('max: ', round(max(result$return),4),' average: ', round(mean(result$return), 4)))
plot(merge(t.return$ma_momentum, t.return$ma_tension, t.return$trade.signal.raw, 0)[period], col=c('cyan4', 'brown', 'coral1', 'darkgray'))
