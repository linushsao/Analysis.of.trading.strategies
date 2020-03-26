rm(list = ls())

LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2')
sapply(LIBRS,library,character.only=TRUE)

#local devel fubction
setwd("/home/linus/ProjectStock/ExtraPackages/linus/stock.Analyze")
roxygenize()
library("stock.Analyze")

#
setwd("/home/linus/ProjectStock/all_stocks/")

#
# enable.Debug <- TRUE
prefix.stock.sxtension <- env(name="prefix.stock.sxtension",mode="r")
prefix.Washed <- env(name="prefix.Washed",mode="r")
prefix.Washed.AllListname <- env(name="prefix.Washed.AllListname",mode="r")
prefix.Filter <- env(name="prefix.Filter",mode="r")
prefix.Rlist <- env(name="prefix.Rlist",mode="r")
prefix.Finished <- env(name="prefix.Finished ",mode="r")
prefix.MsgFor.Delete <- env(name="prefix.MsgFor.Delete",mode="r")
mark.Deleted <- env(name="mark.Deleted",mode="r")
mark.Splite <- env(name="mark.Splite",mode="r")
prefix.washed.list <- env(name="prefix.washed.list",mode="r")
stock.selected.Files_Extension <- env(name="stock.selected.Files_Extension ",mode="r")
raw.data.Listname <- env(name="raw.data.Listname",mode="r")
#
stock.selected_Files <- as.vector(read.csv(raw.data.Listname , header=TRUE, sep=",")[,2])
# stock.selected_Files <- paste("RAW.",stock.selected_Files)



#import all dataset for washing
# name <- gsub(" ","",paste(stock.selected_Files, stock.selected.Files_Extension, sep=""))
name <- stock.selected_Files
filter.list <- merge.data_ByFile(name,period=2:3,order=1)
filter <- t(filter.list[,1])

result.List <- c(rep(NA,50))
washed.List <- c(rep(NA,50))
.r.list.num <- 0

fin.report <- function(result.List )    {
#     msg<- m_paste(c(prefix.Finished ,symbol ," ,index.max ",.max.Data.Num) ,op="")
# #     .r.list.num <- .r.list.num +1
#     result.List <- ifelse(TRUE,all.List(list ,msg ),0)
    write.zoo(result.List,file=paste(prefix.Rlist ,name[1],sep=""),sep=",")
}

for (i in 1:length(name)) {

    symbol <- name[i] 
    data_RAW <- read.csv(symbol,header=FALSE)
    replace.colum <- 2
    if(dbg(type="if.debug")){
        data_RAW[,replace.colum ] <- gsub(prefix.stock.sxtension,"",data_RAW[,replace.colum ] )
    }
#     filter <- gsub(prefix.stock.sxtension,"",as.vector(t(filter.list[1])))
        for (j in 1:length(data_RAW[,replace.colum])) {
            filted.stock_code <- data_RAW[,replace.colum ][j]
            if (!(filted.stock_code %in% filter)) {
                del.debug <- function(result.List){
                    data_RAW[j,] <- m_paste(c(mark.Deleted," code: ",filted.stock_code),op="")
                    msg <- m_paste(c(prefix.MsgFor.Delete ,"Index: " ,j ," Code: ",filted.stock_code ," in " ,symbol),op="")
                    .r.list.num <- .r.list.num +1
                    result.List[.r.list.num] <- msg
                    }
                del.real <- function(result.List ){              
                    data_RAW[j,] <- NA 
                    }
                zero <- ifelse(dbg(type="if.debug"),del.debug(result.List),del.real(result.List))
#                 if(dbg(type="if.debug")){
# #                     data_RAW[j,] <- m_paste(c(mark.Deleted," code: ",filted.stock_code),op="")
# #                     msg <- m_paste(c(prefix.MsgFor.Delete ,"Index: " ,j ," Code: ",filted.stock_code ," in " ,symbol),op="")
# #                     .r.list.num <- .r.list.num+1
# #                     result.List[.r.list.num] <- msg
#                 }else{
#                     #data_RAW[j,] <- NA 
            }else{
#                 pass.debug <- function(result.List ){
                    msg <- m_paste(c("filted.stock_code ",filted.stock_code ," the same with ",data_RAW[,replace.colum ][j]," in ",symbol),op="")
                    .r.list.num <- .r.list.num+1
#                     result.List <- ifelse(TRUE,all.List(result.List ,msg ),0)
#                 }
#                 zero <- ifelse(dbg(type="if.debug"),pass.debug(result.List),0)
                result.List[.r.list.num] <- msg
            }
        }

# zero <- ifelse(dbg(type="if.debug"),dbg(result.List,type="print", pause=TRUR),0)

if(dbg(type="if.debug")){
    data_RAW <- na.omit(data_RAW)
}

file_name <- m_paste(c(prefix.Washed ,i ,"_",symbol) ,op="")
#     V1      V2     V3         V4        V5               V6   V7          V8           V9
#1     1 1101.TW   台泥   39.34180 1.1830971         水泥工業 上市 0.038914674  4.319633977
names(data_RAW) <- c("INDEX","STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO")
if(dbg(type="if.debug")){
    #data_RAW <- na.omit(data_RAW)
    .range <-length(index(data_RAW[,1]))
    .max.Data.Num <- index(data_RAW)[.range]
    }else{
    .max.Data.Num <- ""
}

#save data washed file...
if(dbg(type="if.debug")){
data_RAW[,replace.colum] <- paste(data_RAW[,replace.colum],prefix.stock.sxtension,sep="")
}
write.zoo(data_RAW[,-c(1)],file=file_name,sep=",")

if(dbg(type="if.debug")){
# zero <- ifelse(dbg(type="if.debug"),fin.report(result.List),)
msg<- m_paste(c(prefix.Finished ,symbol ," ,index.max ",.max.Data.Num) ,op="")
.r.list.num <- .r.list.num+1
result.List[.r.list.num] <- msg 

#output filter file
file_name <- m_paste(c(prefix.Filter ,i ,name[1]) ,op="")
write.zoo(filter.list,file=file_name,sep=",")

}

washed.List[i] <- file_name

}

## END_OF LOOP

# .r.list.num  <- .r.list.num +1
# result.List[.r.list.num] <- " "
# for(.td in 1:length(name)){
#     .r.list.num  <- .r.list.num +1
#     result.List[.r.list.num ] <- result.List[.td]
# }

# dbg(na.omit(result.List))
#Result Print

ifelse(dbg(type="if.debug"),fin.report(result.List ),0)

washed.listname.Result <- m_paste(c(prefix.Washed.AllListname ,na.omit(washed.List)),op=mark.Splite)
file_name <- m_paste(c(prefix.washed.list,name[1]),op="")
zero <- env(name="washed.listname.Result",value=file_name ,mode="w")
write.zoo(washed.listname.Result,file=file_name,sep=",")
#fot testing
file_name <- name[2]
zero <- env(name="backtest.name",value=washed.List[1]  ,mode="w")

#file_name <- paste(selected_period,"_ANALYZE.csv",sep="")
#names(KK_RAW) <- c("INDEX","STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO")
#write.zoo(KK_RAW,file=file_name,sep=",")
#View(KK_RAW)






