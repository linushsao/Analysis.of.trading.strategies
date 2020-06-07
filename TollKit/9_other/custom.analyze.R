rm(list=ls())
# par(mfrow=c(1,1))
graphics.off() 
#
LIBRS <- c('quantmod','stringr','xts','TTR','roxygen2','tseries','rlist','lubridate', 'ids')
sapply(LIBRS,library,character.only=TRUE)
# sapply(LIBRS,install.packages,character.only=TRUE)
setwd("/home/linus/Project/1_R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")
research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")
setwd(research.path.of.linus)

# modify column
filename=paste0('/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/remix.stock.2020_', as.Date(Sys.time()),'.csv')
filename_remix <- paste0(gsub('.csv', '', filename), '_.csv')
data.raw <- read.csv(filename, header=TRUE, sep=',')[,-c(1)]
for(id in 2017:2020)
{
year <- paste0('RATE.', id)
data.raw <- m_remix.data(data=data.raw, mode=c('add.col','sort',year))
}
write.csv(data.raw, file=filename_remix)
# 
# list.path <- '/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/remix.stock.2020_2020-05-25.csv'
# stock.list <- read.csv(list.path, header=T, sep=',')[,-c(1)]
# stock.path <- dataset.MGR(group=c('stock','data'), request='conf')
# stock.list$STOCK_PATH <- paste0(stock.path, stock.list$STOCK_CODE, '.csv')
# range <- 250*3
# col.names <- names(stock.list)
# 
# tmp.data <- data.frame()
# for(stock.id in 1:length(stock.list$STOCK_CODE))
# {
#     data.raw <- read.csv(stock.list$STOCK_PATH[stock.id], header=T, sep=',')
#     data.xts <- xts(data.raw[,-c(1)], order.by=as.Date(data.raw[,c(1)]))
#     data.xts <- data.xts[complete.cases(data.xts), ]
#     data.nrow <- nrow(data.xts)
#     data.pre <- data.xts[(data.nrow - range):(data.nrow) ,]
# 
#     Clprs <- Cl(data.pre)
#     
#     per <- periodogram(na.omit(ROC(Clprs)), plot=FALSE)
#     per_df <- data.frame(freq=per$freq, spec=per$spec)
#     per.sort <- per_df[order(-per_df$spec),]
#     per.sort$seasonal <- 1 / per.sort$freq
#     stop()
#     stock.mean   <- mean(Clprs) #平均數
#     stock.median <- median(Clprs)  #中位數
# #     stock.range <- range(Clprs) #全距
# #     stock.quantile<- quantile(Clprs) #四分位數
#     stock.IQR <- IQR(Clprs) #四分位差
#     stock.sd<- sd(Clprs) #標準差
#     stock.var <- var(Clprs) #變異數 
#     stock.skewness <- skewness(Clprs) #偏度
#     stock.kurtosis <- kurtosis(Clprs) #峰度   
#     
#     stock.summary <- c(as.character(stock.list$STOCK_CODE[stock.id]),  
#                         stock.mean, stock.median, 
#                         stock.IQR, stock.sd, stock.var, stock.skewness, stock.kurtosis )
#     col.names.tmp <- c(col.names[1], 'mean', 'median', 'IQR', 'sd', 'var', 'skewness', 'kurtosis')
#     tmp.data <- rbind(tmp.data, stock.summary)
#     names(tmp.data) <- col.names.tmp
#     stop()
# }
# write.zoo(res, file='test1.csv')
# # etf.list <- dataset.MGR(group=c('etf','list'), request='info')[,c(1:2)]
# # 
# # tmp.s <- na.omit(merge(Cl(main.stock.xts), Cl(Hedge.funds.xts)))[period]
# # res <- data.frame(cor(tmp.s))
# # res
# # write.zoo(res, file='test1.csv')
# 
# a <- read.csv('/home/linus/Project/1_R/Analysis.of.trading.strategies/all_codes.csv', header=T, sep=',')
# b <- read.csv('/home/linus/Project/0_Comprehensive.Research/09_理論及研究資料/aa.csv', header=T, sep=',')
# 
# c <- merge(a,b,all=TRUE, by='STOCK_CODE')
# c$STOCK_NAME.x <- c$STOCK_NAME.y
# c$STOCK_NAME.y <- NULL
# 
# col.name <- names(c)
# col.name <- gsub('STOCK_NAME.x', 'STOCK_NAME', col.name)
# names(c) <- col.name
# write.csv(c, '/home/linus/Project/1_R/Analysis.of.trading.strategies/all_codes.csv')


