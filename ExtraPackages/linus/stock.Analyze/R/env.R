#' A analysis.Target Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

env <- function(name,value,mode="init_list") {

    #test
    FLAG_add <- TRUE
    name.name <- "name"
    name.value <- "value"
    name.group <- "group"
    env.file_name <- "env_file"
    env.file_value <- ".env.Configure"
    extension <- ".csv"
    conf_name <- m_paste(c("/home/linus/ProjectStock/all_stocks/",env.file_value,extension),op="")

    
    env.get.confname <- function(x=conf_name){
        return(x)
    }
    
    env.reset <- function(v){
#         if( file.exists(conf_name) ) { #if conf exist,delete first,than create
#             unlink(conf_name)
#             }else{
#             file.create(conf_name)
#             }
#         config <- array("@",dim=c(20,2))
#         temp_1 <- as.vector(t(config[,1]))
#         temp_1[1] <- v[1]
#         config[,1] <- temp_1
#         temp_2 <- as.vector(t(config[,2]))
#         temp_2[1] <- v[2]
#         config[,2] <- temp_2
            result <- data.frame(a=c(v[1]),b=c(v[2]))
            names(result) <- c(name.name ,name.value)
            write.csv(result,file=conf_name)
            
        return(result)
    }

    env.read <- function(x){
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        r.name <- as.vector(t(config[,1]))
        r.value <- as.vector(t(config[,2]))
        for( y1 in 1:length(r.name)) {
            if( x == r.name[y1] ) { 
            result <- r.value[y1]
            return(result)
            }
        }
    }
    
    env.modify <- function(){
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        filter <- as.vector(t(config[,1]))
        for( y1 in 1:length(filter)) {
            if( name == filter[y1] ) { 
            temp <- as.vector(t(config[,2]))
            temp[y1] <- value
            config[,2] <- temp
            write.csv(config,file=conf_name)
            FLAG_add <- FALSE
                }
            }
        if( FLAG_add ) { # add record
            temp <- data.frame(a=c(name),b=c(value))
            names(config) <- c(name.name ,name.value)
            names(temp) <- c(name.name ,name.value)
            config <- rbind(config,temp)
            write.csv(config,file=conf_name)
#             for( y1 in 1:length(filter)) {
#                 if( (filter[y1] == "@" || is.null(filter[y1]) || is.na(filter[y1])) && FLAG_add) { 
#                     print(y1)
#                     temp_1 <- as.vector(t(config[,1]))
#                     temp_1[y1] <- name
#                     config[,1] <- temp_1
#                     temp_2 <- as.vector(t(config[,2]))
#                     temp_2[y1] <- value
#                     config[,2] <- temp_2
#                     write.csv(config,file=conf_name)
#                     FLAG_add <- FALSE
#                 }
#             }
         }
        return(config)
    }
    
    env.del <- function(config) {
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        filter <- as.vector(t(config[,1]))
        for( y1 in 1:length(filter)) {
            if( name == filter[y1]  ) { 
#            temp <- as.vector(t(config[,2]))
#            temp[y1] <- value
            config[y1,] <- NA
            config <- na.omit(config)
            write.csv(config,file=conf_name)
#             FLAG_add <- FALSE
                }
            }
    }

    
###AIN SECTION
# 
    conf_name <- env.get.confname()
    #check if config exist at first processing
    if(! file.exists(conf_name) ) { 
        file.create(conf_name) 
#     tryit <- try(read.csv(conf_name,header=TRUE))
#     if(inherits(tryit, "try-error")){
#         #write.csv("@",file=conf_name)
        config <- env.reset(c(env.file_name,env.file_value))
#         write.csv(config,file=conf_name)
        result <- config  
    }
    
    if(mode == "init_reset") {
        config <- env.reset(c(env.file_name,env.file_value)) #add first record
#         names(config) <- c("name","value")
#         write.csv(config,file=conf_name)
        result <- config
    }else if(mode == "init_list") {
         #do nothing
    }else if(mode == "w") {
        config <- env.modify()
    }else if(mode == "r") {
        return(env.read(name))
        break
     }else if(mode == "d") {
        config <- env.del(config)
    }

        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)]
        rownames(config) <- NULL
        names(config) <- c("name","value")
        result <- config
        return(result)
}

#for testing
# a <- "TESTING"
# 
# stop()
# env(name="raw.data.listname",mode="r")
# 
# name="ORDER"
# value="FRUIT"
# mode="w"
# v <- c(env.file_name,env.file_value)
# v[2]
# env()
# (env(name="list",value="NO",mode="w"))
# (env(name="list",value="YES",mode="w"))
# (env(name="ORDER",value="FRUIT",mode="w"))
# (env(name="ORDER",value="BANANA",mode="w"))
# 
# (env(name="ORDER",value="BANANA",mode="d"))
# (env(name="list",mode="d"))
# 
# env(mode="init_reset")
# head(env())
# head(env(mode="init_list"))
# head(env(name="list",mode="r"))
# head(env(name="env_file",mode="r"))
# head(env(name="washed.listname.Result",mode="r"))
# head(env(mode="init_list"))

# head(env(name="prefix.raw.data.name",mode="d"))
# head(env(mode="init_list"))
# 
# head(env(mode="init_reset"))



