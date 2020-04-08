#' A data_Washing Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

data.washing <- function() {

    #stocks data path
    setwd("/home/linus/Project/9.Shared.Data/1_Taiwan/finance.yahoo.com/stocks.preDownload/")
    prefix.subpath.stock <- "Stock"
    prefix.subpath.index <- "Index"
    prefix.subpath.currency <- "Currency"

    #
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")

    # enable.Debug <- TRUE
    prefix.raw.data.name <- m_env(name="prefix.raw.data.name",mode="r")
    prefix.stock.sxtension <- m_env(name="prefix.stock.sxtension",mode="r")
    prefix.Washed <- m_env(name="prefix.Washed",mode="r")
    prefix.Washed.List.name  <- m_env(name="prefix.Washed.List.name",mode="r")
    prefix.Filter <- m_env(name="prefix.Filter",mode="r")
    prefix.Rlist <- m_env(name="prefix.Rlist",mode="r")
    prefix.Finished <- m_env(name="prefix.Finished ",mode="r")
    prefix.MsgFor.Delete <- m_env(name="prefix.MsgFor.Delete",mode="r")
    mark.Deleted <- m_env(name="mark.Deleted",mode="r")
    mark.Splite <- m_env(name="mark.Splite",mode="r")
    prefix.washed.list <- m_env(name="prefix.washed.list",mode="r")
    stock.selected.Files_Extension <- m_env(name="stock.selected.Files_Extension",mode="r")
    raw.data.Listname <- m_env(name="raw.data.Listname",mode="r")
    #
    stock.selected_Files <- as.vector(read.csv(paste(research.path.of.linus, raw.data.Listname,sep=""), header=TRUE, sep=",")[,2])

    name <- stock.selected_Files
    filter.list <- merge.data_ByFile(paste(research.path.of.linus, name, sep=""),period=2:3,order=1,header=TRUE)
    filter <- t(filter.list[,1])

    result.List <- c(rep(NA,50))
    washed.List <- c(rep(NA,50))
    .r.list.num <- 0

    fin.report <- function(result.List )    {
        write.zoo(result.List,file=paste(prefix.Rlist ,name[1],sep=""),sep=",")
    }

    for (i in 1:length(name)) {

        symbol <- name[i] 
        data_RAW <- read.csv(paste(research.path.of.linus, symbol,sep=""),header=TRUE)
        replace.colum <- 2
        if(dbg(type="if.debug")){
            data_RAW[,replace.colum ] <- gsub(prefix.stock.sxtension,"",data_RAW[,replace.colum ] )
        }
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
                }else{
                        msg <- m_paste(c("filted.stock_code ",filted.stock_code ," the same with ",data_RAW[,replace.colum ][j]," in ",symbol),op="")
                        .r.list.num <- .r.list.num+1
                    result.List[.r.list.num] <- msg
                }
            }

    if(dbg(type="if.debug")){
        data_RAW <- na.omit(data_RAW)
    }

    file_name <- m_paste(c(prefix.Washed ,i ,"_",symbol) ,op="")

    names(data_RAW) <- c("INDEX","STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO","Record_Start")
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
    data_RAW.sort <- m_sort(data_RAW,key="RATE",decreasing=TRUE)[,-c(1)]
    rownames(data_RAW.sort) <- NULL
    write.csv(data_RAW.sort,file=paste(research.path.of.linus, file_name, sep=""))

    if(dbg(type="if.debug")){
        msg<- m_paste(c(prefix.Finished ,symbol ," ,index.max ",.max.Data.Num) ,op="")
        .r.list.num <- .r.list.num+1
        result.List[.r.list.num] <- msg 

        #output filter file
        file_name <- m_paste(c(prefix.Filter ,i ,name[1]) ,op="")
        write.csv(filter.list,file=paste(research.path.of.linus, file_name, sep=""))
    }

    washed.List[i] <- file_name

    }
    ## END_OF LOOP

    ifelse(dbg(type="if.debug"),fin.report(result.List ),0)

    file_name <- prefix.Washed.List.name
    zero <- m_env(name="prefix.Washed.List.name",value=file_name ,mode="w")
    write.zoo(washed.List,file=paste(research.path.of.linus, file_name, sep=""),sep=",")
    #fot testing
    file_name <- washed.List[1]
    zero <- m_env(name="backtest.name",value=file_name  ,mode="w")

    return( washed.List )
}




