#' A generator.of.stockDat Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))


    generator.of.stockData <- function( 
                                        select.tranSet.period=NULL, 
                                        stock.data.path=NULL, 
                                        list.path=NULL, 
                                        group=NULL, 
                                        auto.detect=TRUE) 
    {

        generate.debug.data <- as.logical(m_env(name="generate.debug.data",mode="r"))
        research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")

        all.code.List <- list.path
        prefix.stock.extension <- ifelse((group == 'global.index'), '', m_env(name="prefix.stock.extension",mode="r"))
        prefix.raw.data.name <- m_env(name="prefix.raw.data.name",mode="r")
        stock.selected.Files_Extension <- m_env(name="stock.selected.Files_Extension",mode="r")
        prefix.debug.data.name <- "debug."
        raw.data.Listname <- m_env(name="raw.data.Listname",mode="r")
        debug.data.Listname <- "debug.data.List.csv"
        debug.selected.stock.name <- c("600000.ss","600016.ss","600018.ss","600028.ss","600048.ss")
        safe_rate <- 0.015 
        max.record.num <- 60
        raw.data.List <- c()
        stock_ignore <- c("1729","2025","2384","6131","6205","6283","8201","9106") 
        all.colname <- c("STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","RATE.max","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO","Record_Start",'Clspr.ret.Stdev')

        KK_RAW <- c()
        if( is.null(select.tranSet.period) ) { #custom raw.data.Listname
            select.tranSet.period <- read.csv(raw.data.Listname, header=TRUE,sep=",")[,2]
            select.tranSet.period <- m_gsub(c(prefix.raw.data.name, stock.selected.Files_Extension),c(""),select.tranSet.period)
            }

        code.list <- read.csv(all.code.List, header=TRUE, sep=",")
        code.list[,1] <- sapply(as.character(code.list[,1]), m_check.code)
        name <- paste(code.list[,1],prefix.stock.extension, sep = "") 
        
        stock.cname <- as.vector(code.list[,2])
        stock.group <- as.vector(code.list[,5])
        stock.type <- as.vector(code.list[,6])

        for( range in 1:length(select.tranSet.period) )  {
            selected_period <- select.tranSet.period[range]
            raw.data.List[range] <- m_paste(c(research.path.of.linus, group, '.', prefix.raw.data.name,selected_period,".csv"),op="")
            if(file.exists(raw.data.List[range]) && auto.detect) next

            KK_RAW <- c()
            for(i in 1:length(name)) {
            
                if(name[i] %in% stock_ignore) next
                
                    name[i]-> symbol
                    file_name <- m_paste(c(stock.data.path, symbol,".csv"), op="") 	
                    if(! file.exists(file_name)) {
                        i <- i+1
                        } else {
                        data_RAW <- read.csv(file_name,header=TRUE)
                        data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))
                        names(data) <- c("Open","High","Low","Close","Volume","Adjusted")
                        l <- length(data[selected_period])
                            
                            if( l <= max.record.num ){
                            i <- i+1
                            } else {
                                data_1 <- data[selected_period]
                                Clspr.ret <- na.omit(ROC(data_1$Close))
                                Clspr.ret.sd <- sd(Clspr.ret)
                                return <- exp(cumsum(Clspr.ret))
                                mdd = maxDrawDown(cumsum(Clspr.ret))
                                l <- length(return)

                                temp <- c(symbol,stock.cname[i],data_1$Close[l], return$Close[l], max(return$Close), stock.type[i],stock.group[i], mdd$maxdrawdow, ((return$Close[l]-1-safe_rate)/mdd$maxdrawdown), as.vector(data_RAW$Index)[1], Clspr.ret.sd)
                                KK_RAW <- rbind(KK_RAW,temp)
                            }
                        }
                }
                title <- all.colname
                colnames(KK_RAW) <-  title
                write.zoo(KK_RAW,file=raw.data.List[range],sep=",")
            }
        #save raw file list
        write.csv(raw.data.List,file=raw.data.Listname)
        m_env(name="raw.data.Listname", value=raw.data.Listname, mode="w")
        
        return( raw.data.List )
    }






