#' fund.Allocation Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    ef.fund.Allocation <- function(r_set,goal_return=0.000701,type=1) {
    
        if( type == 1 ) { #Efficient Frontier效率前緣
        min.var <- function(r_set,goal_return){
            n <- dim(r_set)[2]
            Q <- cov(r_set)
            r <- apply(r_set,MARGIN=2,FUN=mean)
            L1 <- cbind(Q,rep(1,n),r)
            L2 <- rbind(c(rep(1,n),0,0),c(r,0,0))
            L <- rbind(L1,L2)
            b <- c(rep(0,n),1,goal_return)

            solve.res <- solve(L,b)
            wt <- solve.res[1:n]

            return_mean <- r %*% wt
            return_variance <- wt %*% Q %*% wt
            return(c(return_mean,return_variance,wt))
        }
        ef <- function(r_set,goal_return) {
                    #goal_return日期望收益率
                    r_set <- as.data.frame(coredata(r_set))
                    portfolio <- min.var(r_set,goal_return)
                    portfolio_weight <- portfolio[3:length(portfolio)]
                    return(portfolio_weight)
                    }
    
    #portfolio_weight <- ef(all_stock_closeprice,goal_return=(ef_goal_return^(1/250)))
    portfolio_weight <- ef(r_set,goal_return=(goal_return^(1/250)))
    }
    
    return(portfolio_weight)
    }
