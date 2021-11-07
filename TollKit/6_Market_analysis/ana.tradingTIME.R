# 
rm(list=ls())
#CUstom LIB.
#
library("beepr")
library("sound")
library("audio")
library("magrittr")
library("e1071")
#
setwd("C:/Temp/")
extra.lib.path <-"C:/Users/linus/Documents/Project/1.R/"
#### 仔入額外函式 #### 
source("C:/Users/linus/Documents/Project/8.Research.Material/NEW_GENERATION/PT.Tools.R")
#Analysis.of.trading.strategies
source(paste0(extra.lib.path, "Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/R/m_misc.R"))
source(paste0(extra.lib.path, "Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/R/m_env.R"))
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_custom.R")
source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_DataMGR.R")

##
# check.ifNull <-function(x, reg.name)
# {
#   if (length(x) ==0)#無輸入
#   { 
#     pre.param <-get.conf(name=reg.name) 
#     if(is.null(pre.param))#無前例
#     {
#       return(FALSE)
#     }else{
#       return(pre.param)#填入前例
#     }
#   }else{
#     return(x)#輸入正常
#   }  
# }
# 
# while(TRUE)
# {
#   .reg.name <-"risk_data.year"
#   .pre.data.year <-get.conf(name=.reg.name)
#   data.year <- readline(paste0("PLs. Enter the date(YYYYMMDD) ", .pre.data.year, " :"))
#   
#   .chick <-check.ifNull(x=data.year, reg.name=.reg.name)
#   if(!is.logical(.chick))
#   {
#     set.conf(name=.reg.name, value = data.year)
#     rm(list=c(".chick", ".reg.name")) #回收變數
#     break
#   }
#   
# }

.pattern <- "股票當沖_v0.01_"

msg.LongCreate <-" [訊息] 符合多頭建倉條件"
msg.LongCClose <-" [訊息] 符合多頭平倉條件"
msg.ShortCreate <-" [訊息] 符合空頭建倉條件"
msg.ShortClose <-" [訊息] 符合空頭平倉條件"
msg.result <- c(msg.LongCreate, msg.LongCClose, msg.ShortCreate, msg.ShortClose)

amount <-1
data.year.all <-c( "20211018", "20211019", "20211020", "20211021", "20211022"
                    ,"20211025", "20211026", "20211027", "20211028", "20211029")
source.type <-c("C:/Temp/case_study/enable.Sync.with.MARKET", "C:/Temp/case_study/disable.Sync.with.MARKET")

filter.price <-c(35, 45)
filter.vol <-c(1000, 2000)
filter.rate <- c(0.5, 1.5, 0.5, 3, 0.5, 4.5, 0.5, 6, 0.5, 7.5, 0.5, 10 )
d.name <-c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max")
LorS.vec <- c(1, -1)

for(.id.source in 1:length(source.type))
{
  source.path <-source.type[.id.source]
  
  file.list <- list.files(path=source.path, pattern =.pattern, full.names = TRUE )
  if(length(file.list) ==0)
  {
    print("無檔案")
    stop()
  }
  
  
  #主程式.
  
  for(.id.filter in 1:length(data.year.all))
  {
    
    data.year <-data.year.all[.id.filter]
    portfolio.result <-data.frame()
    
    for(.index in 1:length(file.list))
    {
      
      filename <-file.list[.index]
      stock.name <-m_gsub(c(paste0(source.path, "/", "股票當沖_v0.01_"), ".TW.log"), c(""), filename)
      
      raw.data <- read.csv(file=filename, header=TRUE)
      # data.year <- "20211105"
      raw.data.grep <- raw.data[raw.data$date ==data.year,]
      
      for(miu.1 in 1:2)
      {
        #設定建倉與平倉位置
        .index.Cr <- (miu.1-1)*2+1
        .index.Cl <- (miu.1-1)*2+2
        
        raw.data.Lportfolio <- raw.data.grep[raw.data.grep$X_info_1 
                                             %in% c(msg.result[.index.Cr], msg.result[.index.Cl]), ]
        raw.data.Lportfolio <- subset(raw.data.Lportfolio, select = c(date, time, close, X_volD, X_RateD, X_info_1))
        
        if(nrow(raw.data.Lportfolio) >0)
        {
          
          for(miu in 1:nrow(raw.data.Lportfolio))
          {
            if(raw.data.Lportfolio$X_info_1[miu] ==msg.LongCreate)
            {
              raw.data.Lportfolio$close[miu] <-raw.data.Lportfolio$close[miu] *-1
            }
            if(raw.data.Lportfolio$X_info_1[miu] ==msg.ShortClose)
            {
              raw.data.Lportfolio$close[miu] <-raw.data.Lportfolio$close[miu] *-1
            }
            
          }
          
          raw.data.Lportfolio
          
          # amount <-5
          
          portfolio <- round(sum(raw.data.Lportfolio$close)*1000*amount, digits = 0)
          portfolio.single <-data.frame(date=raw.data.Lportfolio$date[miu]
                                        , time=raw.data.Lportfolio$time[miu]
                                        , code=stock.name
                                        , close=abs(raw.data.Lportfolio$close[miu])
                                        , portfolio=portfolio
                                        , volD=raw.data.Lportfolio$X_volD
                                        , RateD=raw.data.Lportfolio$X_RateD*100
                                        , info=raw.data.Lportfolio$X_info_1[miu])
          portfolio.single
          
          portfolio.result <-rbind(portfolio.result, portfolio.single)
          
          print(paste(source.path, data.year, stock.name))
        }else{
          print(paste(source.path, data.year, stock.name, " contain NA,exit."))
        }
        
        
      }  
      
    }
    
    portfolio.result$PCL <-ifelse(portfolio.result$info ==msg.LongCClose, 1,-1)
    append.to.file(data=portfolio.result
                   , path=paste0(source.path, "/", data.year, "_portfolio.result.csv")
                   , m.append = FALSE, m.col.names = TRUE)
    
    #
    
    analyze.data.result <-data.frame()
    
    for(miu.vol in 1:(length(filter.vol)))
    {
      
      data.vol <-filter.vol[miu.vol]
      
      for(miu.1 in 1:(length(filter.rate)/2))
      {
        rate.start <- filter.rate[(miu.1-1)*2+1]
        rate.stop  <- filter.rate[(miu.1-1)*2+2]
        
        raw.data.str <- subset(portfolio.result
                               , (
                                 (close >=35 & close <=45)
                                 & (volD >=data.vol)
                                 & (RateD >=rate.start & RateD <=rate.stop)  
                               ) 
                               , select = c(date, time, code, close, portfolio, volD, RateD, PCL))
        
        
        for(.index in 1:length(LorS.vec))
        {
          
          LorS <-LorS.vec[.index]
          
          raw.data.str_obj <-raw.data.str[raw.data.str$PCL ==LorS,]
          data.summary <-as.vector(summary(raw.data.str_obj$portfolio))
          names(data.summary) <-d.name
          
          data.kur <-kurtosis(raw.data.str_obj$portfolio)
          data.ske <-skewness(raw.data.str_obj$portfolio)
          data.sum <- sum(raw.data.str_obj$portfolio)
          data.min <-as.numeric(data.summary["Min"])
          data.med <-as.numeric(data.summary["Median"])
          data.max <-as.numeric(data.summary["Max"])
          data.num <-nrow(raw.data.str_obj)
          
          analyze.data <- data.frame(
            kurtosis=round(data.kur, digits = 4),
            skewness=round(data.ske, digits = 4),
            min=data.min,
            med=data.med,
            max=data.max,
            sum=data.sum,
            sample.num=data.num,
            LorS=LorS,
            rate.start=rate.start,
            rate.stop=rate.stop, 
            vol=data.vol
          )  
          
          analyze.data.result <-rbind(analyze.data.result, analyze.data)
          
        } 
        
      }  
      
    }
    
    
    append.to.file(data=analyze.data.result
                   , path=paste0(source.path, "/", data.year, "_analyze.data.result.csv")
                   , m.append = FALSE, m.col.names = TRUE)
    # rm(analyze.data.result)  
    
  }  
}






