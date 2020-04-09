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


