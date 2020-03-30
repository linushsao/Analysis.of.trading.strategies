#' A kdj.Indicator Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))


kdj.Indicator <- function(data_Import,n=9) {

    close <- Cl(data_Import)
    high <- Hi(data_Import)
    low <- Lo(data_Import)

    date <- index(close)
    ndate <- length(date)

    m_msg(date)
    write.csv(data_Import,file="check_KDTrade3.csv")
    write.csv(c("ndate",ndate),file="check_KDTrade4.csv")
    periodHigh <- xts(rep(0,ndate-(n-1)),order.by=date[-(1:(n-1))])
    periodLow <- xts(rep(0,ndate-(n-1)),order.by=date[-(1:(n-1))])
    RSV <- xts(rep(0,ndate-(n-1)),order.by=date[-(1:(n-1))])

    for(j in (n:ndate)){
        period <- date[(j-(n-1)):j]
        i <- date[j]
        periodHigh[i] <- max(high[period])
        periodLow[i]  <- min(low[period])
        RSV[i] <- 100 * (close[i] - periodLow[i])/(periodHigh[i] - periodLow[i])
    }

    names(periodHigh) <- "periodHigh"
    names(periodLow)  <- "periodLow"
    names(RSV) <- "RSV"

    rsv <- c(50,50,as.numeric(RSV))

    ndate <- length(date)
    RSV1 <- na.omit(xts(rsv,order.by=date[7:ndate]))
    names(RSV1) <- "RSV"
    
    KValue <- EMA(RSV1,n=2,ratio=1/3)
    names(KValue) <- "KValue"

    KValue[1] <- 50
    DValue <- EMA(KValue,n=2,ratio = 1/3)
    names(DValue) <- "DValue"

    KValue <- KValue[-c(1:2)]
    DValue <- DValue[-c(1:2)]
    KDindicator <- merge(RSV,KValue,DValue)

    JValue <- 3 * KValue - 2 * DValue
    names(JValue) <- "JValue"
    
    names(periodHigh) <- "n_daysHigh"
    names(periodLow) <- "n_daysLow"

    result <- merge(KDindicator,JValue,periodHigh,periodLow)

    return(result)

}



