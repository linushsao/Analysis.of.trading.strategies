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
        cache.title <- "公司代號 公司名稱 去年當月營收 當月營收 上月營收 上月比較增減 去年同月增減 當月累計營收 去年累計營收 前期比較增減"
        c1.title <- strsplit(cache.title, spl.key)[[1]]
        c1t.title <- c("company.code","company.name","revenue.month","Last.month.revenue","Last.year.month.revenue","Last.month.revenue(%)","Last.year.month.revenue(%)","cumulative.revenue.month","last.year.cumulative.revenue","early.inNdecrease")

        c2.title <- c(" ",'\\(%\\)',"營業收入","累計營業收入","合計")
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


 
 
 
