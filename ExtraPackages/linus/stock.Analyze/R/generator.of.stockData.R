#' A generator.of.stockDat Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))


    generator.of.stockData <- function( 
                                        tranSet.period=NULL, 
                                        data.path=NULL, 
                                        list.path=NULL, 
                                        group=NULL, 
                                        auto.detect=TRUE) 
    {

        research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")

        all.code.List <- list.path
        prefix.stock.extension <- ifelse((group == 'index'), '', m_env(name="prefix.stock.extension",mode="r"))
        prefix.raw.data.name <- m_env(name="prefix.raw.data.name",mode="r")
        stock.selected.Files_Extension <- m_env(name="stock.selected.Files_Extension",mode="r")
        raw.data.Listname <- m_env(name="raw.data.Listname",mode="r")
        safe_rate <- 0.015 
        max.record.num <- 60
        raw.data.List <- c()
        all.colname <- c("STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","RATE.max","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO","Record_Start",'Clspr.ret.Stdev')

        KK_RAW <- c()
        if( is.null(tranSet.period) ) { #custom raw.data.Listname
            tranSet.period <- read.csv(raw.data.Listname, header=TRUE,sep=",")[,2]
            tranSet.period <- m_gsub(c(prefix.raw.data.name, stock.selected.Files_Extension),c(""),tranSet.period)
            }

        code.list <- read.csv(all.code.List, header=TRUE, sep=",")
        if(group != 'index') {code.list[,1] <- sapply(as.character(code.list[,1]), m_check.code)}
        name <- paste(code.list[,1],prefix.stock.extension, sep = "") 
        
        stock.cname <- as.vector(code.list[,2])
        stock.group <- as.vector(code.list[,5])
        stock.type <- as.vector(code.list[,6])

        for( range in 1:length(tranSet.period) )  {
            selected_period <- as.character(tranSet.period[range])
            raw.data.List[range] <- m_paste(c(research.path.of.linus, group, '.', prefix.raw.data.name,selected_period,".csv"),op="")
            if(file.exists(raw.data.List[range]) && auto.detect) next

            KK_RAW <- c()
            for(i in 1:length(name)) {
            
#                 if(name[i] %in% stock_ignore) next
                
                    name[i]-> symbol
                    file_name <- m_paste(c(data.path, symbol,".csv"), op="") 	
                    if(! file.exists(file_name)) {
                        next
                        } else {

                        data_RAW <- read.csv(file_name,header=TRUE)
                        data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))
                        names(data) <- c("Open","High","Low","Close","Volume","Adjusted")

                        l <- nrow(data[selected_period])
#                         m_msg(info=paste(selected_period, l, file_name))
                        
                        if( l <= max.record.num ){
                        next
                            } else {
                                data_1 <- data[selected_period]
                                Clspr.ret <- na.omit(ROC(data_1$Close))
                                Clspr.ret.sd <- sd(Clspr.ret)
                                return <- cumprod(1 + Clspr.ret)
                                mdd = maxdrawdown(return)
                                l <- length(return)
#                                 browser()
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






