#' A regressor.fit Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))


m_regressor.fit <- function(stock.code, tranSet.period, testSet.period, data.path, retma.data.path, lag.num=20) {
#     #generate analyze data
#     stock.name <- "9910.TW"
#     tranSet.period <- "::2010"
#     testSet.period <- c("2011")
#     file.extension <- ".csv"
#     data.path <- "/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/Stock/"
#     retma.data.path <- "/home/linus/Project/0_Comprehensive.Research/02_ma.for.ret/02_stock/"
# 
#     lag.num = 20
    #
        stock.path <- m_paste(c(data.path, stock.name, file.extension), op="")
        stock.data <- read.csv(stock.path, header=TRUE, sep=",")
        stock.data.xts <- xts(stock.data[,-c(1)], order.by=as.Date(stock.data[,1]))
        names(stock.data.xts) <- c('Open','High','Low','Close','Volumn','Adjusted') 
        stock.close <- stock.data.xts$Close[tranSet.period]

        retma.stock.path <- m_paste(c(retma.data.path, stock.name, file.extension), op="")
        stock.retma <- read.csv(retma.stock.path, header=TRUE, sep=",")
        stock.retma.xts <- xts(stock.retma[,-c(1)], order.by=as.Date(stock.retma[,1]))
        names(stock.retma.xts) <- c('Ratio','ma5','ma20','ma60','ma120','ma252')
        
        stock.retma.xts$ma5 <- lag( stock.retma.xts$ma5, lag.num)
        stock.retma.xts$ma20 <- lag( stock.retma.xts$ma20, lag.num)
        stock.retma.xts$ma60 <- lag( stock.retma.xts$ma60, lag.num)
        stock.retma.xts$ma120 <- lag( stock.retma.xts$ma120, lag.num)
        stock.retma.xts$ma252 <- lag( stock.retma.xts$ma252, lag.num)
        
        stock.retma.xts$annual <- cumprod(1+stock.retma.xts$Ratio)
        stock.retma <- na.omit(stock.retma.xts)
        
        # prepare for monthly data
    #     stock.data.xts.monthly <- to.monthly(stock.data.xts)
    #     names(stock.data.xts.monthly) <- c('Open','High','Low','Close','Adjusted') 
    #     stock.data.xts.monthly$Ratio<- complete.cases(ROC(stock.data.xts.monthly$Adjusted))
        

#         x11()
#         acf2(stock.retma$Ratio)
#         x11()
#         per <- periodogram(stock.retma$Ratio)
#     #     x11()
#     #     per <- periodogram(stock.data.xts.monthly$Ratio)
#         
#         per_df <- data.frame(freq=per$freq, spec=per$spec)
#         order <- per_df[order(-per_df$spec),]
#         time <- 1 / head(order,7)$f
#         time
        
        tranSet <- stock.retma[tranSet.period]
        testSet <- stock.retma[testSet.period]
        
        regressor <- lm(annual ~ . , data=tranSet[,-c(1)])
        regressor.step <- step(regressor)
#         summary(regressor.step)
    
        regressor.export <- lm(annual ~ ma20 + ma120 + ma252 + I(ma20^2) + I(ma20^3) + I(ma20^4) + I(ma20^5) + I(ma120^2) + I(ma120^3) + I(ma120^4) + I(ma120^5) + I(ma252^2) + I(ma252^3) + I(ma252^4) + I(ma252^5), data=tranSet[,-c(1)])
#         summary(regressor.export)
        
        testSet$Spred <- predict(regressor.export, testSet)
        testSet$diff <- (testSet$annual -  testSet$Spred) / testSet$annual
#         summary(testSet)

#         x11()
#         par(mfrow=c(2,1))
#         plot(merge(testSet$annual, testSet$Spred),col=c("black","green"))
#         plot(testSet$diff)
#         abline(h=0, col="blue")
        
    #     p.name <- 5
    #     stock.result <- scale(tranSet[,c(p.name,7)])
    #     title <- names(tranSet)[p.name]
        result <- list(regressor=regressor.export, exportSet=testSet)
    return(result)
    
}
