#' A generator.of.stockDat Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))


generator.of.stockData <- function( select.tranSet.period=NULL, stock.data.path=NULL) {

    generate.debug.data <- as.logical(m_env(name="generate.debug.data",mode="r"))
    research.path.of.linus <- m_env(name="research.path.of.linus",mode="r")

    all.code.List <- m_env(name="all.code.List",mode="r")
    prefix.stock.extension <- m_env(name="prefix.stock.extension",mode="r")
    prefix.raw.data.name <- m_env(name="prefix.raw.data.name",mode="r")
    stock.selected.Files_Extension <- m_env(name="stock.selected.Files_Extension",mode="r")
    prefix.debug.data.name <- "debug."
    raw.data.Listname <- m_env(name="raw.data.Listname",mode="r")
    debug.data.Listname <- "debug.data.List.csv"
    debug.selected.stock.name <- c("600000.ss","600016.ss","600018.ss","600028.ss","600048.ss")
    safe_rate <- 0.015 
    raw.data.List <- c()
    stock_ignore <- c("1729","2025","2384","6131","6205","6283","8201","9106") 

    # main function

    if (generate.debug.data) {

            KK_RAW <- c()
            
            name <- debug.selected.stock.name
            name_c <- debug.selected.stock.name
            name_group <- "for.Debug"
            name_type <- "for.Debug"
            
            for(i in 1:length(name)) {
                name[i]-> symbol
                file_name <- m_paste(c(stock.data.path, symbol, ".csv"), op="") 	
                
                tryit <- try(read.csv(file_name,header=TRUE))
                
                if(inherits(tryit, "try-error") ){
                    i <- i+1
                } else {
                    data_RAW <- read.csv(file_name,header=TRUE)
                    data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))
                    names(data) <- c("Open","High","Low","Close","Volume","Adjusted")
                    
                    l <- length(data)
                    if( l == 0 ){
                    i <- i+1
                    } else {
                    data_1 <- data
                    return_1 <- na.omit(ROC(data_1$Close))
                    return <- exp(cumsum(return_1))
                    mdd = maxDrawDown(cumsum(return_1))
                    l <- length(return)

                    temp <- c(symbol,name_c[i],data_1$Close[l],return$Close[l],name_type,name_group,mdd$maxdrawdow,((return$Close[l]-1-safe_rate)/mdd$maxdrawdown),as.character(index(data)[1]))
                    KK_RAW <- rbind(KK_RAW,temp)
                    }
                }
            }

            debug.data.List <- paste(prefix.debug.data.name, name[1], ".csv", sep="")
            KK_RAW <- as.data.frame(KK_RAW)
            names(KK_RAW) <- c("STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO","Record_Start")
            write.zoo(KK_RAW, file=debug.data.List, sep=",")
            
        #save debug file list
        write.csv(debug.data.List,file=debug.data.Listname)

        }else{

            KK_RAW <- c()
            if( is.null(select.tranSet.period) ) { #custom raw.data.Listname
                select.tranSet.period <- read.csv(raw.data.Listname, header=TRUE,sep=",")[,2]
                select.tranSet.period <- m_gsub(c(prefix.raw.data.name, stock.selected.Files_Extension),c(""),select.tranSet.period)
                }
                
#             select.tranSet.period <- as.vector(read.csv(raw.data.Listname , header=TRUE, sep=",")[,2])

            m_data <- read.csv(paste(stock.data.path, all.code.List,sep=""), header=TRUE, sep=",")
#             m_data <- m_data1
            name <- paste(m_data[,1],prefix.stock.extension, sep = "") 
            name_c <- as.vector(m_data[,2])
            name_group <- as.vector(m_data[,5])
            name_type <- as.vector(m_data[,6])

            for( range in 1:length(select.tranSet.period) )  {
                selected_period <- select.tranSet.period[range]
                KK_RAW <- c()
                
                for(i in 1:length(name)) {
                    if(name[i] %in% stock_ignore) next
                    name[i]-> symbol
                    file_name <- m_paste(c(stock.data.path, symbol,".csv"), op="") 	
                    tryit <- try(read.csv(file_name,header=TRUE))

                    if(inherits(tryit, "try-error") | (gsub(prefix.stock.extension,"",symbol) %in% stock_ignore) ){
                        i <- i+1
                    } else {
                        data_RAW <- read.csv(file_name,header=TRUE)
                        data <- na.omit(xts(data_RAW[c(2:7)],order.by=as.Date(data_RAW$Index)))
                        names(data) <- c("Open","High","Low","Close","Volume","Adjusted")
                        
                        l <- length(data[selected_period])
                        if( l == 0 ){
                        i <- i+1
                        } else {
                        data_1 <- data[selected_period]
                        return_1 <- na.omit(ROC(data_1$Close))
                        return <- exp(cumsum(return_1))
                        mdd = maxDrawDown(cumsum(return_1))
                        l <- length(return)

                        temp <- c(symbol,name_c[i],data_1$Close[l],return$Close[l],name_type[i],name_group[i],mdd$maxdrawdow,((return$Close[l]-1-safe_rate)/mdd$maxdrawdown),as.vector(data_RAW$Index)[1])
                        KK_RAW <- rbind(KK_RAW,temp)
                        }
                    }
                }

                raw.data.List[range] <- m_paste(c(research.path.of.linus, prefix.raw.data.name,selected_period,".csv"),op="")
                title <- c("STOCK_CODE","STOCK_NAME","LAST_CLOSE","RATE","GROUP","TYPE","MaxDrawDOWN","SHARP_RATIO","Record_Start")
                colnames(KK_RAW) <-  title
                write.zoo(KK_RAW,file=raw.data.List[range],sep=",")
                #View(KK_RAW)
            }
        #save raw file list
#         raw.data.List.filename <- paste(research.path.of.linus, raw.data.Listname, sep="")
        write.csv(raw.data.List,file=raw.data.Listname)
    }
    m_env(name="raw.data.Listname", value=raw.data.Listname, mode="w")
    return( raw.data.Listname )
    
}






