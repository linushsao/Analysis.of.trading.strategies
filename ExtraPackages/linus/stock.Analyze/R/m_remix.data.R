#' A m_check.code Function
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

    
#
# stop()
# wd.dir <- "/home/linus/Project/0_Comprehensive.Research/"
# setwd(wd.dir)
# 
# filename='/home/linus/Project/0_Comprehensive.Research/03_Remixed.data/01_stock/remix.stock.2020_2020-06-02.csv'
# data <- read.csv(filename, header=TRUE, sep=',')[,-c(1)]
# data.1 <- m_remix.data(data=data.1, mode=c('add.col','sort','RATE.2017'))
# write.csv(data.1, file='test.csv')


