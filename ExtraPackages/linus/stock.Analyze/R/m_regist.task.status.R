#' A get.script.name Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_regist.task.status <- function(task.value=NULL,mode="w") {

    script.name <- get.script.name()
    task.name <- paste("task.status",script.name,sep="_")
    
    if(mode == "w") {
        m_env(name=task.name, value=task.value, mode="w")
    }else if(mode == "r") {
        return(m_env(name=task.name, mode="r"))
    }
}


