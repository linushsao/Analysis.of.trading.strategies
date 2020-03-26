#' A CumulativeRate Function
#'
#' This function allows you to calculate the median from a numeric vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

stock.CumulativeRate <- function(data,strategy_type=NULL,strategy_params=c(9,3,3),trade_trigger=c(85,20,80,20)) {

#strategy_type=2
#strategy_params=c(9,3,3)
#trade_trigger=c(85,20,80,20)
#type:default 1/KD , strategy_params=c(nFastK,nFastD,nSlowD)
    
    if(strategy_type == 1)  {
    #    KD_RAW <- na.omit(stoch(HLC(data[test_period[1]]),nFastK = value_fastK, nFastD = value_fastD, nSlowD = value_slowD))
        KD_RAW <- na.omit(stoch(HLC(data),nFastK = strategy_params[1], nFastD = strategy_params[2], nSlowD = strategy_params[3]))
 #       KD_TREND <- KD_RAW$fastK-KD_RAW$fastD
#        names(KD_TREND) <- "K-D"
        #API
        KValue <- KD_RAW$fastK
        DValue <- KD_RAW$fastD

        KDupbreak <- upbreak(KValue,DValue)
        KDdownbreak <- downbreak(KValue,DValue)
        #
        close <- Cl(data)
        difclose <- diff(close,1)

        prctrend <- ifelse(difclose >= 0,1,-1)
        prctrend <- prctrend[-1]
        #
        KDupSig <- apply(merge(KDupbreak,prctrend),1,function(x){
            ifelse(x[1] == 1 & x[2] == 1,1,0)
        })

        KDupSig <- xts(as.numeric(KDupSig),order.by=index(prctrend))

        KDdownSig <- apply(merge(KDdownbreak,prctrend),1,function(x){
            ifelse(x[1] == 1 & x[2] == (-1),-1,0)
        })

        KDdownSig <- xts(as.numeric(KDdownSig),order.by=index(prctrend))

        FULLSignal <- KDupSig + KDdownSig
        names(FULLSignal) <- "breakSig"
        
    }else if(strategy_type %in% c(2,3)) {
        KD_RAW <- na.omit(kdj.Indicator(data))

        #API RSV    KVale   DValue     JValue n_daysHigh n_daysLow
        KValue <- KD_RAW$KValue
        DValue <- KD_RAW$DValue
        JValue <- KD_RAW$JValue

        #K -> 85 / 20
        #D -> 80 / 20
        limit_k_up <- trade_trigger[1]
        limit_k_dn <- trade_trigger[2]
        limit_d_up <- trade_trigger[3]
        limit_d_dn <- trade_trigger[4]
        KSignal <- ifelse(KValue > limit_k_up , -1 , ifelse(KValue < limit_k_dn ,1,0))
        DSignal <- ifelse(DValue > limit_d_up,-1,ifelse(DValue < limit_d_dn,1,0))

        KDSignal <- apply(merge(KSignal,DSignal),1,sum)
        FULLSignal <- xts(KDSignal,order.by=index(KSignal))
        names(FULLSignal) <- "KDSignal"       
        if(strategy_type == 3){
            JSignal <- ifelse(JValue>100,-1,ifelse(KValue<0,1,0))
            #head(JSignal[JSignal == -1])
            signal <- merge(FULLSignal,JSignal)
            #head(signal)

            FULLSignal <- xts(apply(signal,1,sum),order.by=index(JSignal))
            names(FULLSignal) <- "KDJSignal"
            #head(KDJSignal[KDJSignal >= 2])
            #head(KDJSignal[KDJSignal <= -2])
            }
        }else if(strategy_type==4){
    
    ###
        KD_RAW <- na.omit(kdj.Indicator(data))

        #API RSV    KVale   DValue     JValue n_daysHigh n_daysLow
        KValue <- KD_RAW$KValue
        DValue <- KD_RAW$DValue
        JValue <- KD_RAW$JValue
        
        KDupbreak <- upbreak(KValue,DValue)
        KDdownbreak <- downbreak(KValue,DValue)
        #
        close <- Cl(data)
        difclose <- diff(close,1)

        prctrend <- ifelse(difclose >= 0,1,-1)
        prctrend <- prctrend[-1]
        #
        KDupSig <- apply(merge(KDupbreak,prctrend),1,function(x){
            ifelse(x[1] == 1 & x[2] == 1,1,0)
        })
        KDupSig <- xts(as.numeric(KDupSig),order.by=index(prctrend))

        KDdownSig <- apply(merge(KDdownbreak,prctrend),1,function(x){
            ifelse(x[1] == 1 & x[2] == (-1),-1,0)
        })
        KDdownSig <- xts(as.numeric(KDdownSig),order.by=index(prctrend))

        FULLSignal <- KDupSig + KDdownSig
        names(FULLSignal) <- "breakSig"

    ###
    
            }



#        trade <- function(signal,price){
#            ret <- ROC(price,n=1,type="discrete")[-1]
#            names(ret) <- "ret"
#            signal <- na.omit(lag(signal,1))
#            tradeRet <- ret * signal
#            names(tradeRet) <- "tradeRet"
#            Return <- na.omit(merge(ret,tradeRet))
#            return(Return)
#        }

        close <- data$Close
        KDtrade <- tradeBysignal(FULLSignal,close)
        names(KDtrade) <- c("Ret","KDtradeRet")
        CumulativeRate <- KDtrade
        
        
        return(CumulativeRate)
    }

#data <- testing_data(period="2014-01-01/2015-04-30")
#b <- stock.CumulativeRate(data,strategy_type=4)
