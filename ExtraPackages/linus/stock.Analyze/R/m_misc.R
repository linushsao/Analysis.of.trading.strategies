#' A MISC Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

na.filter <- function(x) {return(x[complete.cases(x),])}

replace.colname <- function(x, pattern, replace)
{
    tmp.colname <- names(x)
    tmp.colname <- gsub(pattern, replace, tmp.colname)
    names(x) <- tmp.colname
    return(x)
}

data.clean <- function(data, replace=NA)
{
    data[is.infinite(data)] <- replace
    data <- na.filter(data)
    return(data)
}

append.col.xts <- function(data=NULL, col.data=NULL, col.name=NULL)
{
    
    tmp.data <- xts(data.frame(col.data), order.by=index(data))
    names(tmp.data) <- col.name
    data <- cbind(data, tmp.data)
    return(data)
}

m_rebind.xts <- function(x, v, byrow=TRUE)
{
    for(id in 1:length(v))
    {
        if(is.na(v[id]) || is.null(v[id])) next
        if(id==1) {tmp.data <- x[as.character(v[id])]
        }else{
            if(byrow) {
                tmp.data <- rbind(tmp.data, x[as.character(v[id])])
            }else{
                tmp.data <- cbind(tmp.data, x[as.character(v[id])])
            }
        }
    }
    return(tmp.data)
}

string.sep <- function(x, split=',')
{
    tmp.data <- unlist(strsplit(x, split=split, fixed = FALSE, perl = FALSE, useBytes = FALSE))
    return(tmp.data)
}

m_names <- function(x, c.names, bycol=TRUE)
{
    if(bycol) names(x) <- c.names
    return(x)
}

colname.shift <- function(x, as.from, to.be)
{
    all.colnames <- colnames(x)
    string.as.from <- all.colnames[as.from]
    all.colnames[as.from] <- NA
    all.colnames <- all.colnames[complete.cases(all.colnames)]
    leng.colnames <- length(all.colnames)
    all.colnames <- c(all.colnames[1:(to.be - 1)], string.as.from, all.colnames[to.be:leng.colnames])
    x <- x[, all.colnames]
    return(x)
}

winpath.trans <- function(x, home.dir=NULL)
{
    tmp <- gsub(home.dir[1], '', x)
    tmp <- gsub('/', '\\\\', tmp)
    tmp <- paste0(home.dir[2], tmp)
    
    return(tmp)
}


#' A dbg Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

dbg<- function(x=NULL,type="print",name,pause=FALSE,debug=FALSE)  {
    
    
    announce <- function(x,op){
        if(debug){
            print(msg)}
    }
    
    msg <- "[dbg] ####### Beginning to DEBUG PROCESS. #######" 
    announce(msg,op=debug)
    
    if(debug || (type == "if.debug"))   {
        #         msg <- paste("[Debug Msg] ",x,sep="")
        if(type == "print") {
            print(x)
        }else if(type == "head")  {
            print(head(x))
        }else if(type == "tail")  {
            print(tail(x,3))
        }else if(type == "msg")  {
            View(x)
        }else if(type == "file")  {
            write.zoo(x,file=name,sep=",")
        }else if(type == "custom")  {
            #custom funtion
            return("")
        }else if(type == "if.debug")  {
            return(debug)
        }else if(type == "exec")  {
            #custom funtion
            f <- function(y){
                y <- gsub(".TW","",y)
                return(y)
            }
            return(f(x))
        }else{
            View(x)
        }
    }
    
    msg <- "[dbg] ####### End of DEBUG PROCESS. ####### "
    announce(msg,op=debug)
    
    if( pause ) {
        stop()
    }
}


#' Am_lag Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_lag <- function(x, lag.num=1,shift=FALSE) {
    l <- length(x)
    l.shift <- (l-(lag.num-1))
    if(shift) {
        x.tmp <- x[l.shift:l]
    }else{
        x.tmp <- c(rep(NA, lag.num))
    }
    result <- c(x.tmp, x[1:(l-lag.num)])
    return(result)
}

#' A message Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

# m_msg <- function(info, order="%b%d %X",action="msg"){
m_msg <- function(info, order="%X",action="msg")
{
    result <- m_paste(c("[",format(Sys.time(), order),"]", info),op=" ")
    
    if( action == "msg" ) {
        print(result)
        return(TRUE)
    }else if(action == "log") {
        return(result)
    }
    
}


#' A page2table Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_page2table <- function(page.url, page.encoding="UTF-8", export.file=NULL, css.selector=NULL)  {
    
    
    page.source <- read_html(page.url,encoding=page.encoding)
    version.block <- html_nodes(page.source, css.selector)
    content <- html_text(version.block)
    version.block.1 <- html_nodes(page.source, 'table')
    table <- html_table(version.block.1)
    result.all <- list(page.source=content , page.table=table)
    if(! is.null(export.file)) {
        write.csv(content, file=paste("content_",export.file,sep=""))
        write.csv(table, file=paste("table_",export.file,sep=""))
    }
    
    return(result.all)
}

#' A multi paste Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_paste <- function(x,op=""){
    temp <- ""
    for(i in 1:length(x)){
        temp <- ifelse(i == 1,x[i],paste(temp,x[i],sep=op))
    }
    return(temp)
}


#' A m_price.utils Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

check.price <- function(x, end.days=NULL, ref.days=250, export=FALSE, dataset.MGR=FALSE)
{
    if(dataset.MGR[1]) 
    {
        tmp.data <- dataset.MGR(dataset.name=x, group=c(dataset.MGR[2], dataset.MGR[3]), request='info')
    }else{
        tmp.data <- x
    }
    
    if(is.null(end.days)) end.days <- as.Date(Sys.Date())
    start.days <- as.Date(end.days) - ref.days
    data.period <- paste0(start.days, '::', end.days)
    
    if(!export)
    {
        return(summary(tmp.data[data.period]))
    }else{
        return(list(summ=summary(tmp.data[data.period]), data=tmp.data[data.period]))
    }
}


#' A m_rebind.xtsFunction
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_rebind.xts <- function(x, v, byrow=TRUE)
{
    for(id in 1:length(v))
    {
        if(is.na(v[id]) || is.null(v[id])) next
        if(id==1) {tmp.data <- x[as.character(v[id])]
        }else{
            if(byrow) {
                tmp.data <- rbind(tmp.data, x[as.character(v[id])])
            }else{
                tmp.data <- cbind(tmp.data, x[as.character(v[id])])
            }
        }
    }
    return(tmp.data)
}


#' A m_remix.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_remix.data <- function(data=NULL, mode=NULL) 
{
    
    if(mode[1] == 'add.col')
    {
        if(mode[2]== 'sort')
        {
            obj.col <- mode[3]
            data <- data[order(-data[, obj.col]),]
            newcol.name <- paste0('rank.',mode[3])
            new.col <- data.frame(c(1:nrow(data)))
            names(new.col) <- newcol.name
            data <- cbind(data, new.col)
            data <- data[,c(ncol(data), (1:(ncol(data)-1)))]
            rownames(data) <- NULL
            result <- data
        }
    }
    
    return(result)
}


#' A m_series Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_series <- function(type='Fibo', n=15){
    
    
    if(type=='Fibo' || type=='Fi')
    {
        f.series <- c(1,1)
        for(i in 3:n) f.series[i] <- f.series[i-1] + f.series[i-2]
        result <- t(t(f.series))
    }
    
    return(result)
}


#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_smaCal <- function(ts, n=NULL, increase=TRUE){
    
    len.ts <- length(ts)
    
    if( increase ) 
    {
        n.start <- n
        n.end   <- len.ts
    }else{
        n.start <- len.ts
        n.end   <- n
    }
    
    for(rowid in n.start:n.end) ts$samCal[rowid] = mean(ts[,1][(rowid-n+1):rowid],na.rm=TRUE)
    return(ts)
}


#' A multi sort Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_sort <- function(x,key=NULL,decreasing = FALSE,reindex=FALSE){
    
    a <- x[(order(x[,key],decreasing = decreasing)),]
    if(reindex){
        l <- dim(a)[1]
        rownames(a) <- 1:l
    }
    return(a)
}


#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_std.data <- function(x, restore=NULL){
    
    if(is.null(restore)) {
        result <- x
        ret <- c()
        for(i in 1:ncol(x)) {
            v <- c(min(x[,i]), max(x[,i]))
            for(j in 1:nrow(x)){
                result[j,i] <- (x[j,i] - v[1])/(v[2]-v[1])
            }
            ret <- c(ret, v)
        }
        ret.array <- matrix(ret, nrow=2, ncol=ncol(x))
        rownames(ret.array) <- c('min', 'max')
        colnames(ret.array) <- colnames(x)
        
        return(list(data=result, ret=ret.array))
    } else{
        result <- c()
        ret <- restore #ret(min, max)
        result <- (x * (ret[2]-ret[1])) + ret[1]
        
        return(result)
    }
}


#' A for merge.data_ByFile  Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

merge.data_ByFile <- function(name,period=NULL,order=NULL,debugger=FALSE,header=FALSE) {
    
    l_name <- length(name)
    temp <- data.frame()
    
    for(i in 1:l_name) {
        
        name[i]-> symbol
        data_RAW <- read.csv(symbol,header=header)
        data <- data.frame(data_RAW[,period])
        data[,1] <- as.numeric(gsub(".TW","",data[,order]))
        names(data) <- c("code","name")
        
        if(i == 1) {
            temp <- data
        }else{
            temp <- merge(temp,data,by="code")
        }
        result <- temp
        
    }
    
    return(result)
}


#' A page2table Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

page2table <- function(page.url, page.encoding="UTF-8", export.file, css.selector)  {
    
    page.source <- read_html(page.url,encoding=page.encoding)
    version.block <- html_nodes(page.source, css.selector)
    content <- html_text(version.block)
    result <- html2table(content)
    result.all <- list(page.source=content , page.table=result)
    write.csv(result, file=export.file)
    return(result.all)
}
# Sub Function <<<
html2table <- function(content=content) {
    # z.table <- html_table(version.block)
    spl.key <- " "
    cache.title <- "?…¬?¸ä»???? ?…¬?¸??ç¨± ?Ž»å¹´ç•¶??ˆç?Ÿæ”¶ ?•¶??ˆç?Ÿæ”¶ ä¸Šæ?ˆç?Ÿæ”¶ ä¸Šæ?ˆæ?”è?ƒå?žæ?? ?Ž»å¹´å?Œæ?ˆå?žæ?? ?•¶??ˆç´¯è¨ˆç?Ÿæ”¶ ?Ž»å¹´ç´¯è¨ˆç?Ÿæ”¶ ??æ?Ÿæ?”è?ƒå?žæ??"
    c1.title <- strsplit(cache.title, spl.key)[[1]]
    c1t.title <- c("company.code","company.name","revenue.month","Last.month.revenue","Last.year.month.revenue","Last.month.revenue(%)","Last.year.month.revenue(%)","cumulative.revenue.month","last.year.cumulative.revenue","early.inNdecrease")
    
    c2.title <- c("??",'\\(%\\)',"??Ÿæ¥­?”¶?…¥","ç´¯è?ˆç?Ÿæ¥­?”¶?…¥","??ˆè??")
    c2t.title <- c(NA,"",NA,NA,"TOTAL")
    
    content.1 <- na.omit(m_gsub(c2.title, c2t.title, content))
    # write.csv(content.1, file="test1.csv")
    content.2 <- m_gsub(c1.title, c1t.title, content.1)
    # write.csv(content.2, file="test2.csv")
    
    content.2 <- gsub(" ", "", content.2, fixed = TRUE)
    mark.key <- "TOTAL"
    replace.key <- "---"
    c.mark <- c()
    code.mark <- c()
    k <- 0
    
    for(i in 1:length(content.2)){ #replace total related
        if(! is.na(as.numeric(content.2[i])) ) { 
            code.mark <- c( code.mark, c(i, content.2[i]) )
        }
        
        if(content.2[i] == mark.key) {
            for(j in 1:9) {
                re.num <- i+j-1
                content.2[re.num] <- replace.key
            }
            i <- i+9
        }
    }
    
    #remove not complete data of company
    code.mat <- matrix(code.mark,ncol=2,byrow=TRUE)
    code.mat[,2] <- gsub("\\.",NA,code.mat[,2])
    code.mat <- na.omit(code.mat)
    for(i in 2:dim(code.mat)[1]) {
        
        num.before <- as.numeric(code.mat[i-1,1])
        num.current <- as.numeric(code.mat[i,1])
        if( (num.current-num.before) != 10 ) {
            content.2[num.before:(num.current-1)] <- replace.key
        }
    }
    # write.csv(content.2,file="test3.csv")
    
    content.3 <- content.2[! content.2==replace.key]
    a <- matrix(content.3,ncol=10,byrow=TRUE)
    a <- a[-c(1),]
    colnames(a) <- c1t.title
    return(a)
}


#' A m_std.data Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

signal.filter <- function(signal, max.days=0, value.filted=NULL, fixed.indicator=NULL)
{
    row.num <- nrow(signal)
    tmp.data <- c(rep(0, row.num))
    signal[is.na(signal)] <- 0
    
    if(max.days < 0) 
    {   # if down max.days days, sell it.
        signal[signal == 0] <- -1
        #work only for value <0
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  signal[rowid]<0, 
                                                            signal[rowid] + ifelse(tmp.data[rowid-1]>0, 0, tmp.data[rowid-1]),
                                                            signal[rowid]) 
        #check if continue [max.days]days <0                                                         
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  tmp.data[rowid]<0, 
                                                            tmp.data[rowid] - (max.days-1),
                                                            tmp.data[rowid])
        for(rowid in 2:row.num) tmp.data[rowid] <- ifelse(  tmp.data[rowid]>0, 
                                                            1, 0);tmp1.data <- tmp.data   
    }
    
    if(max.days > 0)
    { # if up max.days days, buy it.
        for(rowid in 2:row.num) tmp.data[rowid] <- signal[rowid] * (signal[rowid] + tmp.data[rowid-1])
        
        while(TRUE)
        {
            #                 if(row.num < 2 || max.days==0) break
            if(row.num < 2) break
            if( tmp.data[row.num] == 0 || is.na(tmp.data[row.num]) ) { row.num <- row.num -1 ; next }
            
            t.range <- c((row.num - tmp.data[row.num] + 1) : row.num )
            t.fill <- ifelse(tmp.data[row.num] > max.days, 1, 0)
            t.step <- tmp.data[row.num]
            for( v.id in 1:length(t.range)) tmp.data[t.range[v.id]] <- t.fill
            
            row.num <- row.num - t.step 
        }
    }
    
    if(! is.null(fixed.indicator)) { #class(fixed.indicator) : list
        tmp.data <- signal
        co.indicator <- fixed.indicator[[1]]
        co.mode <- fixed.indicator[[2]]
        
        if( co.mode == 'and' ) tmp.data <- as.numeric(tmp.data & co.indicator)
        if( co.mode == 'not' ) tmp.data <- as.numeric(!(tmp.data & co.indicator))
        if( co.mode == 'or' ) tmp.data <-  as.numeric(tmp.data | co.indicator)
    }
    
    if(! is.null(value.filted))
    {
        tmp.data <- signal
        if(value.filted[1] == 'min') tmp.data <- ifelse(tmp.data < value.filted[2], 0, tmp.data)
        if(value.filted[1] == 'max') tmp.data <- ifelse(tmp.data > value.filted[2], 0, tmp.data)   
    }
    #     return(data.frame(a=tmp.data, b=tmp1.data, c=co.indicator))
    return(tmp.data)
}


#' A upbreak Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

upbreak <- function(Line,RefLine){
    data <- merge(Line,RefLine,lag(Line),lag(RefLine))
    data <- na.omit(data)
    signal <- apply(data,1,function(x){
        ifelse(x[1]>x[2] & x[3]<x[4],1,0)
    })
    signal <- xts(as.numeric(signal),order.by=index(data))
    return(signal)
}


#' A downbreak Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

downbreak <- function(Line,RefLine){
    data <- merge(Line,RefLine,lag(Line),lag(RefLine))
    data <- na.omit(data)
    signal <- apply(data,1,function(x){
        ifelse(x[1]<x[2] & x[3]>x[4],1,0)
    })
    signal <- xts(as.numeric(signal),order.by=index(data))
    return(signal)
}


#' A crawl.file Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

crawl.file <- function(destfile ,url ,fake.header=NULL,verbose=FALSE){
    
    if( verbose ) {
        print(paste("_Processing Date ",Sys.time(),sep=":"))
        print(paste("_crawling ",destfile,sep=":"))
    }
    download.file(url, destfile, mode="wb", method="auto")
    
}


#' A get sysinfo Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

get.sysinfo <- function(x=0, path=NULL){
    
    #get system info
    #x=0 path of home
    #x=1 sysnam
    #x=2 release
    #x=3 version 
    #x=4 nodename
    #x=5 machine
    #x=6 login 
    #x=7 user(default)
    #x=8 effective_user
    
    sys <- Sys.info()
    
    # if (x == 0 ) { result <- m_paste(c("/home/",sys[6],"/"),op="") }
    if (x == 0 ) 
    { 
        if(is.null(path)){workDIR <-getwd()}
        else{workDIR <-path}
        result <- m_paste(c(workDIR, "/",sys[6],"/"),op="") 
    }
    return(result)
}


#' A get.users.input Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

get.users.input <- function(prompt=NULL, index=NULL)
{
    if( is.null(prompt) )
    {
        stock.custom <- readline(prompt=paste0('Enter Code of object for Analyzation (', get.conf(name='stock.custom'),'): ')) #stock.code
        testSet.period <- as.character(readline(prompt=paste0('Enter period of object for Analyzation (', get.conf(name='testSet.period'),'): ')))
        analyze.group <- readline(prompt=paste0('Enter Group(index | stock | etf) of object for Analyzation  (', get.conf(name='analyze.group'),'): '))  #select analyze.group
        if(stock.custom != '')
        {
            set.conf(name='stock.custom', value=stock.custom)
        }else{
            stock.custom <- get.conf(name='stock.custom')
        }
        if(analyze.group != '')
        {
            set.conf(name='analyze.group', value=analyze.group)
        }else{
            analyze.group <- get.conf(name='analyze.group')
        }
        if(testSet.period != '')
        {
            set.conf(name='testSet.period', value=testSet.period)
        }else{
            testSet.period <- get.conf(name='testSet.period')
        }  
        
        return(c(stock.custom, testSet.period, analyze.group))
    }else{
        pre.input <- ifelse(is.null(index), ' ',ifelse(is.null(get.conf(name=index)), ' ', get.conf(name=index)))
        read.input <- as.character(readline(prompt=paste(prompt, ' (', pre.input,') :')))
        if(nchar(read.input) != 0)
        {
            if(! is.null(index)) set.conf(name=index, value=read.input)
        }else{
            #             read.input <- get.conf(name=index) # read pre-input
            read.input <- ifelse(!is.null(pre.input), pre.input, '')
        }
        
        return(read.input)
    }
}


#' A m_get.data Function
#'
#' This function allows you to get data from list.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_get.data <- function(list,header=TRUE,data.col=1) {
    #read all code for download
    name <- read.csv(list, header=TRUE, sep=",")[,data.col]
    name <- as.character(name)
    
    for(i in 1:length(name)) {
        name[i]-> symbol
        tryit <- try(getSymbols(symbol,auto.assign=FALSE))
        if(inherits(tryit, "try-error")){
            i <- i+1
        } else {
            temp <- xts()
            temp <- getSymbols(symbol,auto.assign=FALSE)
            
            file_name <- paste(symbol,"csv", sep = ".") 	
            write.zoo(temp, file = file_name, sep = ",")
        }
    }
}


#' A get.script.name Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

get.script.name <- function(){
    library("scriptName")
    x <- current_filename()
    write.table(x,file=".script_name.txt")
    a <- read.table(".script_name.txt")
    result <- gsub("./","",a[1,1])
    return(result)
}


#' A multi gsub Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_gsub <- function(be_replace, patten, v)
{
    
    be.num <- length(be_replace)
    pa.num <- length(patten)
    for(i in 1:be.num)
    {
        fixed.patten <- ifelse(i >pa.num, patten[pa.num], patten[i])
        v <- gsub(be_replace[i] ,fixed.patten ,v)
    }
    return(v)
}





