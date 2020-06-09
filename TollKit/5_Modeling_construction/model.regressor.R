    rm(list=ls())
    graphics.off() 
    #
    LIBRS <- c('quantmod','xts','TTR','roxygen2','PerformanceAnalytics','forecast','astsa','TSA','tseries','rlist')
    sapply(LIBRS,library,character.only=TRUE)
    setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
    library('roxygen2')
    roxygenize()
    library("stock.Analyze")
    #

    wd.dir <- "/home/linus/Project/0_Comprehensive.Research/"
    setwd(wd.dir)

    predit.days.default = 28
    train.exten.ratio <- 2
    ma.VALUE <- c(5, 20, 60, 120, 252)
    ma.POWER <- c(1:5)
    lm.limits <- 100

    day.of.year=365
    tranSet.period.years <- 5

    stock.custom <- readline(prompt="Enter Code of object for Analyzation: ") #stock.code
    testSet.period <- as.character(readline(prompt="Enter test.Setperiod(or start) for object : "))
    analyze.group <- readline(prompt="Enter Group(index | stock | etf) of object for Analyzation: ")  #select analyze.group
    predit.days <- as.numeric(readline(prompt="Enter predit.days for object : "))
    
    if(stock.custom != '')
    {
        set.conf(name='stock.custom', value=stock.custom)
        }else{
        stock.custom <- get.conf(name='stock.custom')
    }
    if(testSet.period != '')
    {
        set.conf(name='testSet.period', value=testSet.period)
        }else{
        testSet.period <- get.conf(name='testSet.period')
    }  
    if(analyze.group != '')
    {
        set.conf(name='analyze.group', value=analyze.group)
        }else{
        analyze.group <- get.conf(name='analyze.group')
    }
    if(is.na(predit.days)) predit.days <- predit.days.default

    tran.Set.ratio <- 0.8

    tranSet.period.end <- as.Date(testSet.period) - (tranSet.period.years*day.of.year*(1-tran.Set.ratio))
    tranSet.period.start <- as.Date(tranSet.period.end) - tranSet.period.years*day.of.year
    tranSet.period <- m_paste(c(as.character(tranSet.period.start), '::', as.character(tranSet.period.end)),op="")

    testSet.period.start <- as.Date(testSet.period) - (tranSet.period.years*day.of.year*(1-tran.Set.ratio))
    testSet.period.end <- as.Date(testSet.period)
    testSet.period <- m_paste(c(as.character(testSet.period.start), '::', as.character(testSet.period.end)),op="")

    file.extension <- ".csv"
    list.extension <- '.RData'
    data.path <-as.character( dataset.MGR(group=c(analyze.group, 'data'), request='conf'))
    #read all code of taiwan stocks
    list.file <- as.character(dataset.MGR(group=c(analyze.group, 'list'), request='conf'))
    ma.path <- as.character( dataset.MGR(group=c(analyze.group, 'ma'), request='conf'))
    stock.extension <- as.character( dataset.MGR(group=c(analyze.group, 'extension'), request='conf') )
        
    stock.code.list <- read.csv(list.file, header=TRUE, sep=",")
    stock.code <- as.character(stock.code.list[,1])
    stock.cname <- as.character(stock.code.list[,2])
    stock.type <- as.character(stock.code.list[,6])

    l <- length(stock.code)
    set.seed(as.numeric(Sys.time()))
    
    while(TRUE) {
        if(is.null(stock.custom))
        {
            num <- sample(l,1)
            stock.name <- paste(stock.code[num], stock.extension, sep="")
            }else{ 
            stock.name <- paste(stock.custom, stock.extension, sep="")
            num <- unlist(sapply(1:l, function(v) 
                    {
                    if(stock.code[v] == stock.name) return(v)
                    }))
        }
        stock.path <- m_paste(c(data.path, stock.name, file.extension), op="")
        if(file.exists(stock.path))
        {
            stock.data.raw <- read.csv(stock.path, header=TRUE, sep=",")
            stock.data.xts <- xts(stock.data.raw[,-c(1)], order.by=as.Date(stock.data.raw[,1]))
            if( mean(na.omit(stock.data.xts[tranSet.period][,4])) > 30 || (!is.null(stock.custom)) ) break
        }
    }

   
   title.base <- paste(stock.name, stock.cname[num], sep=' ')

# r.lm <- function() {
    names(stock.data.xts) <- c('Open','High','Low','Close','Volumn','Adjusted') 
    title <- m_paste(c(stock.name, stock.cname[num], stock.type[num], '( tranSet.period ', tranSet.period, " ) / ( testSet.period ",testSet.period," )_",predit.days),op=" ")

    #data preProcess
    p <- paste(ma.path, stock.name, '.RData', sep='')
    stock.list <- list.load(p)
    stock.ma <- stock.list[['stock.ma']]
    stock.ma.lm <- stock.ma[,c(2:7)]
    stock.ma.lm$KeepSell <- apply(stock.ma$clpr.ret, 1,function(v) return(ifelse(v>0, 1, 0)))
    
    tmp.data <- c(rep(0, nrow(stock.ma.lm)))
    for(rowid in 2:nrow(stock.ma.lm))
    {
    tmp.data[rowid] <- stock.ma.lm$KeepSell[rowid] * (stock.ma.lm$KeepSell[rowid] + tmp.data[rowid-1])
    }

    stock.ma.lm$Keep <-  tmp.data
    
#     test.Data <- merge(stock.ma.lm$Keep, cumprod(1+stock.ma$clpr.ret))
#     test.Data.raw <- cumprod(1+stock.ma$clpr.ret)
    test.Data.raw <- cumprod(1+na.omit(ROC(stock.data.xts$Close)))
    test.Data.raw <- test.Data.raw[complete.cases(test.Data.raw),]
    names(test.Data.raw) <- c('Cum.ret')
#     test.Data <- test.Data[,c('Cum.ret','Keep')]

    add.transet.col <- function(test.Data=NULL, predit.days=28)
    {
        for(lag.num in 1:(predit.days))
        {
            c.name <- paste('Cum.ret',lag.num,sep='.')
        #     k.name <- paste('Keep',lag.num,sep='.')
            tmp.cum<- lag(test.Data$Cum.ret, lag.num)
        #     tmp.keep <- lag(test.Data$Keep, lag.num)
        #     tmp <- merge(tmp.cum, tmp.keep)
    #         tmp <- tmp.cum
        #     names(tmp) <- c(c.name, k.name)
            test.Data <- merge(test.Data, tmp.cum)
        }
        return(test.Data)
    }
    
    test.Data <- add.transet.col(test.Data=test.Data.raw, predit.days=predit.days * train.exten.ratio)

    test.Data <- test.Data[complete.cases(test.Data),]
    tran.Set <- test.Data[tranSet.period]
    test.Set <- test.Data[testSet.period]
    
    regressor <- lm(Cum.ret ~ .  , data=tran.Set)    
    summary(regressor)
    regressor.step <- step(regressor)
    summary(regressor.step)
    
    test.Set$Spred <- predict(regressor.step, test.Set[, -c(1)])
    test.Set$diff <- (test.Set$Cum.ret - test.Set$Spred ) / test.Set$Cum.ret    
#     test.Set.raw <- test.Set
    leng.row_orig <- nrow(test.Set)
    #start to predit
    for(predit.id in 1:predit.days)
    {
        test.Set.predit <- test.Set$Cum.ret
        leng.row <- nrow(test.Set.predit)
        predit.date <- as.Date(index(tail(test.Set,1)))+1
        tmp.set <- xts(0, order.by=predit.date)
        names(tmp.set) <- 'Cum.ret'
        test.Set.predit <- rbind(test.Set.predit, tmp.set)
        
        test.Set.predit <- add.transet.col(test.Data=test.Set.predit, predit.days=predit.days * train.exten.ratio)
        test.Set.predit$Spred <- predict(regressor.step, test.Set.predit[, -c(1)])
        test.Set.predit$Cum.ret[leng.row+1] <- test.Set.predit$Spred[leng.row+1] 
        test.Set <- test.Set.predit
        
    }
    leng.index <- index(test.Set)
    test.Set.predit.period <- paste0(leng.index[1], '::', leng.index[length(leng.index)])
    test.Set.predit$Cum.ret[(leng.row_orig+1):length(leng.index)] <- 0
    #backtest
    x11()
    par(mfrow=c(2,1))
    plot(merge(test.Set.predit$Cum.ret[test.Set.predit.period], test.Set.predit$Spred), col=c("black","green"), main=title)
    plot(test.Data$Cum.ret[test.Set.predit.period], main=m_paste(c('Resdiual ', '(m s v) = ','(',round(mean(test.Set$diff),3)," ",round(sd(test.Set$diff),3)," ",round(sd(test.Set$diff)^2,3),")") ,op=''), col=c("black","blue"))
    
#     library(lubridate)
#     stock.data.tmp <- na.omit(xts(stock.data.raw[,-c(1)], order.by=as.Date(stock.data.raw[,1])))[paste('::',tranSet.period.end,sep='')]
#     stock.data.mon <- to.monthly(stock.data.tmp)
#     stock.year <-  year(index(stock.data.tmp[1,]))
#     stock.month <- month(index(stock.data.tmp[1,]))
#     names(stock.data.mon) <- c('Open','High','Low','Close','Volumn','Adjusted') 
#     tsdata <- ts( stock.data.mon$Close, start=c(stock.year, stock.month), frequency=12)
#     decom <- decompose(tsdata, type='mult')
#     x11()
#     plot(decom)
#     x11()
#     acf(na.omit(ROC(tsdata)), lag.max=200)
    
#         }

# var.lm.in_book <- function() {
#         stock.data.pre <- stock.data.xts
#         stock.data.pre[,c(1,2,3,5)] <- lag(stock.data.pre[,c(1,2,3,5)], predit.days)
#         
#         stock.data.sel <- stock.data.pre[tranSet.period]
#         stock.data.sel <- data.frame(Date=index(stock.data.sel), coredata(stock.data.sel))
#         stock.dataframe <- na.omit(stock.data.sel)
#         rownames(stock.dataframe) <- NULL
#         tran.l <- nrow(stock.dataframe)
#         tran.Set.num <- tran.l - predit.days
#         testData.num <- predit.days
#         tran.Set <- stock.dataframe[c(1:tran.Set.num), ][,c(2:5)]
# 
#         p.value.check <- function(x) {
#             library('fUnitRoots')
# #             install.packages('fUnitRoots')
#             value.tmp <- NULL
#             subdata <- scale(tran.Set)
#             center.back <- attr(subdata,"scaled:center")
#             scale.back <- attr(subdata,"scaled:scale")
#             
#             library(fUnitRoots)
#             for(i in 1:ncol(subdata)) {
#                 pValue <- adfTest(subdata[,i])@test$p.value
#                 value.tmp <- c(value.tmp, pValue)
#             }
#             
#             return(list(std.data=subdata, p.value=value.tmp, p.value.mean=mean(value.tmp), center.back=center.back, scale.back=scale.back))
#         }
#         
#     tran.Set.pvc <- p.value.check(tran.Set)
#     tran.Set.pvc$p.value.mean
#     scale.back <- tran.Set.pvc$scale.back
#     center.back <- tran.Set.pvc$center.back
#     subdata <- tran.Set.pvc$std.data
#     
#     rowCol <- dim(subdata)
#     aicList <- NULL
#     lmList <- list()
#     for(p in 1:10) {
#         baseData <- NULL
#         for(i in (p+1):rowCol[1]) {
#             baseData <- rbind(baseData, c(as.vector(subdata[i, ]), as.vector(subdata[(i-1):(i-p), ])))
#         }
#     
#     X <- cbind(1, baseData[, (rowCol[2]+1):ncol(baseData)])
#     Y <- baseData[,1:rowCol[2]]
#     coefMatrix <- solve(t(X) %*% X) %*% t(X) %*% Y
#     aic <- log(det(cov(Y- X%*%coefMatrix))) + 2*(nrow(coefMatrix)-1)^2 *p/nrow(baseData)
#     aicList <- c(aicList, aic)
#     lmList <- c(lmList, list(coefMatrix))
#     }    
#     data.frame(p=1:10, aicList)
# 
#     p <- which.min(aicList)
#     n <- nrow(subdata)
#     preddf <- NULL
#     subdata <- data.frame(subdata)
# 
#     for(i in 1:testData.num) {
#         predData <- as.vector(subdata[(n+i-1):(n+i-p), ])
#         p.1.addnum <- nrow(lmList[[p]]) - nrow(t(predData))
#         predVals <- c(rep(1, p.1.addnum), t(predData)[,1]) %*% lmList[[p]]
#         predVals <- predVals * scale.back + center.back
#         preddf <- rbind(preddf, predVals)
#         subdata <- rbind(subdata, (stock.dataframe[n+i, 2:5] - center.back)/scale.back )
#     }
#    
#     rownames(preddf) <- NULL
#     subdata.test <- stock.dataframe[(tran.l - testData.num +1):tran.l, 2:5 ]
#     summary(as.vector(abs(preddf-subdata.test)*100 / subdata.test))
#     
#     index.xts <- stock.dataframe[(tran.l - testData.num +1):tran.l, 1 ]
#     preddf.xts <- xts(preddf, order.by=as.Date(index.xts))
#     subdata.test.xts <- xts(subdata.test, order.by=as.Date(index.xts))
#     
#     x11()
#     par(mfrow=c(2,2))
#     
#     plot(preddf[,1],type='l',ylab='Open')
#     lines(subdata.test[,1],lty=2,col='red')
# #     x11()
#     plot(preddf[,2],type='l',ylab='High')
#     lines(subdata.test[,2],lty=2,col='red')
# #     x11()
#     plot(preddf[,3],type='l',ylab='Low')
#     lines(subdata.test[,3],lty=2,col='red')
# #     x11()
#     plot(preddf[,4],type='l',ylab='Close')
#     lines(subdata.test[,4],lty=2,col='red')
#     
#     par(mfrow=c(1,1))
#     x11()
#     plot(preddf.xts[,4],type='l',ylab='Close')
#     lines(subdata.test.xts[,4],lty=2,col='red')    
#     
#     x11()
#     title <- m_paste(c(stock.name, stock.cname[num], stock.type[num], round(tran.Set.pvc$p.value.mean, 3), 'tran.Set period: ', tranSet.period),op=" ") 
#     plot(stock.data.xts[,4][tranSet.period],type='l',main=title,col='black')
#     
#     print(paste(colnames(subdata)[i], ', P.value of rootTest :', pValue))
# }
# 
LSTM.NEUREL <- function() {


#LSTM model
library(keras)
library(dplyr)

stock.data.pre <- stock.data.xts[,c(4,5)]
for(i in 1:predit.days) {
    for(j in (4:5)) {
        stock.data.pre <- cbind(stock.data.pre, lag(stock.data.xts[,j], i))
    }
}
stock.data.pre <- na.omit(na_if(stock.data.pre, 0))
myts <- stock.data.pre[tranSet.period]
# pre-processing
myts <- myts[complete.cases(myts), ]

library(plotly)
# plot_ly(myts, x=~index, y=~close, type='scatranSet.perioder', mode='markers', color=~vol)
myts.data <- m_std.data(myts)
myts <- myts.data$data

summary(myts)
# model.fir configure
(batch_size <- 4)

#
(l.myts <- nrow(myts))
(l.tran <- floor(l.myts * tran.Set.ratio))
(ra <- floor(l.tran / batch_size))
(l.tran <- ra * batch_size)
(l.test <- l.myts - l.tran)
(ra <- floor(l.test / batch_size))
(l.test <- ra * batch_size)

# datalags=predit.days

# data.l <- nrow(myts)
train <- myts[1:l.tran, ]
test <- myts[(l.tran+1):((l.tran+l.test)), ]

x.train <- train[,-c(1)]
y.train <- train[,c(1)]
dim(x.train) <- c(dim(x.train),1)
x.test <- test[,-c(1)]
y.test <- test[,c(1)]
dim(x.test) <- c(dim(x.test),1)

rm(model)
model <- keras_model_sequential()

model %>%
    layer_lstm(units = 100,
    input_shape = c(ncol(x.train), 1),
    batch_size = batch_size,
    return_sequences = TRUE,
    stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_lstm(units = 50,
    return_sequences = FALSE,
    stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)

summary(model)
model %>% compile(loss='mean_squared_error', optimizer='adam', metrics=c('accuracy'))

history <- model %>% fit(  
                x=x.train, 
                y=y.train,
                batch_size=batch_size,
                epochs=100,
                verbose=1,
                shuffle=FALSE,
                callback_early_stopping(monitor = "loss", min_delta = 0,patience = 20, verbose = 0, mode = c("auto"),baseline = NULL, restore_best_weights = FALSE)
                )
x11()
plot(history)
model %>% reset_states()

pred_out <- model %>% predict(x.test, batch_size=batch_size) 
result <- cbind(y.test, pred_out)

x11()
par(mfrow=c(2,2))
plot(result)
acf(myts[,1], lag.max=nrow(myts),main=title.base)
plot(myts[,1])

}

