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
