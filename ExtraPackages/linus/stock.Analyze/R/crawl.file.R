#' A crawl.file Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

crawl.file <- function(destfile ,url ,sleep=c(10:30) ,extra.holiday.date ,fake.header=NULL,verbose=FALSE){

#     url.head <- "https://www.twse.com.tw/exchangeReport/MI_INDEX?response=csv&date="
#     url.tail <- "&type=ALL"
#     crawl.check <- ".crawl.check.csv"
    # stop.date <- "2006-01-01"
#     stop.date <- as.Date(env(name="stock.data.earliest",mode="r"))
#     extra.holiday.date <- c(paste("2020-01-",as.character(c(21:29)),sep=""),"2020-10-10","2018-12-31","2019-01-01","2018-08-31") #Chinese New Year
#     raw.date <- as.Date(Sys.time()) + 1 # grep from today+1

#     crawl.file <- function(url,destfile,sleep=c(10:30),check=TRUE) {
            if( verbose ) {
                print(paste("_Processing Date ",Sys.time(),sep=":"))
                print(paste("_crawling ",destfile,sep=":"))
                }
            download.file(url, destfile, mode="wb", method="auto")
            
            sle.num  <- sample(sleep,size=1,replace=TRUE)
            if( verbose ) {
                print(paste("_Sleep.Second ",sle.num,sep=":"))
                }
            Sys.sleep(sle.num)
#     }
}
