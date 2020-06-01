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

#
# stop()
# testSet.period <- get.users.input(prompt='Pls Enter testSet.period', index='testSet.period')
