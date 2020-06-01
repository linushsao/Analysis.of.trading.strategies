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
