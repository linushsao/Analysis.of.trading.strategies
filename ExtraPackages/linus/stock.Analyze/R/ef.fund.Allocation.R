#' fund.Allocation Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

    ef.fund.Allocation <- function(r_set,goal_return=0.000701,type=1) {
    
        if( type == 1 ) { #Markowitz效率前緣
            min.var <- function(r_set,goal_return){
                    n <- dim(r_set)[2]
                    Q <- cov(r_set)
                    r <- apply(r_set,MARGIN=2,FUN=mean)
                    L1 <- cbind(Q,rep(1,n),r)
                    L2 <- rbind(c(rep(1,n),0,0),c(r,0,0))
                    L <- rbind(L1,L2)
                    b <- c(rep(0,n),1,goal_return)

                    solve.res <- solve(L,b)
                    #write.zoo(r_set,file="r_set.csv",sep=",")
                    #write.zoo(goal_return,file="goal_return.csv",sep=",")
                    #write.zoo(solve.res,file="solve_res.csv",sep=",")
                    #write.zoo(L,file="L.csv",sep=",")
                    #write.zoo(b,file="b.csv",sep=",")

                    wt <- solve.res[1:n]
                    return_mean <- r %*% wt
                    return_variance <- wt %*% Q %*% wt
                    return(c(return_mean,return_variance,wt))
                }
            result <- min.var(r_set,goal_return)
            }
    
    return(result)
    }
