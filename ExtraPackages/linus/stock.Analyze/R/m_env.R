#' A m_env Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_env <- function(name=NULL,value=NULL,mode="init_list",dataset="env") {

    #dataset: 
    #1. = default,means handle environment variables
    #2. dataset could be defined to another custom dataset name        
    #         
    FLAG_add <- TRUE
    name.name <- "name"
    name.value <- "value"
#     name.group <- "group"
#     env.file_name <- "env_file"
#     env.file_value <- ".env.Configure"
    # dataset relatived
#     conf_name <- m_paste(c("/home/linus/ProjectStock/all_stocks/",env.file_value,extension),op="")


    # function <<
   
    env.reset <- function(v,name){
            unlink(name)
            file.create(name)
            
            result <- data.frame(a=c(v[1]),b=c(v[2]))
            names(result) <- c(name.name ,name.value)
            write.csv(result,file=name)
            
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

    env.dataset.path.exist <- function(name.env.file_path){
            #zero <- env(name=name.env.file_path,mode="r")
#             print(c("4 [",zero," ]",name.env.file_path))
            if (! file.exists(name.env.file_path) ) { # dataset existed
                    #setup new dataset
#                     set.name <- paste(dataset,".file.path",sep="")
                    env.reset(c(env.file_name,env.file_value),name=name.env.file_path) 
#                     env(name=set.name ,value=get.sysinfo() ,mode="w")
#                     name.env.file_path <- env(name=paste(dataset,".file.path",sep=""),mode="r")
#                     conf_name <- m_paste(c(name.env.file_path,env.file_value,extension),op="")
                    
                }
                
            result <- name.env.file_path
            return(result)
        }
        
#     env.check.confnam <- function(conf.path){
# 
# #         print(c("2",dataset,conf.path ))
#         #conf.path <- env(name=paste(dataset,".file.path",sep=""),mode="r")
# #         print(c("2A",dataset, conf.path ))
#         conf_name <- env.dataset.path.exist(conf.path)
# #         print(c("3",conf_name))
#         result <- conf_name
#         return(result)
#     }

#     env.dataset.switch <- function(dataset){
#         conf_name <- env.dataset.path.exist(name.env.file_path)
#         config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
#         return(x)
#     }
# main function
#     dataset <- "env"
    env.file_name <- paste(dataset ,"_file" ,sep="")
    env.file_value <- m_paste(c(".",dataset,".Configure"),op="")
    extension <- ".csv"
    default.env.file_path <- m_paste(c(get.sysinfo(),env.file_value,extension),op="")

    conf_name <- env.dataset.path.exist(default.env.file_path)
    
    if(mode == "init_reset") {
        config <- env.reset(c(env.file_name,env.file_value),name=conf_name ) #add first record
#         names(config) <- c("name","value")
#         write.csv(config,file=conf_name)
        result <- config
    }else if(mode == "init_list") {
         #do nothing
    }else if(mode == "dataset_list") {
        result  <- dataset
        return(result)
        break
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
# # env(name="raw.data.listname",mode="r")
# # 
# # name="ORDER"
# # value="FRUIT"
# # mode="w"
# # v <- c(env.file_name,env.file_value)
# rm(list=ls())
# # v[2]
# m_env()
# m_env(mode="dataset_list")
# 
# m_env(dataset="env")
# m_env(mode="dataset_list")
# m_env(dataset="new")
# m_env(mode="dataset_list",dataset="new")
# 
# m_env(mode="init_reset")
# m_env(mode="init_reset",dataset="new")
# 
# (m_env(name="list",value="NO",mode="w"))
# (m_env(name="list",value="YES",mode="w"))
# (m_env(name="ORDER",value="FRUIT",mode="w"))
# (m_env(name="ORDER",value="BANANA",mode="w"))
# 
# (m_env(name="ORDER",value="BANANA",mode="d"))
# (m_env(name="list",mode="d"))
# 
# (m_env(name="list",value="NO",mode="w",dataset="new"))
# (m_env(name="list",value="YES",mode="w",dataset="new"))
# (m_env(name="ORDER",value="FRUIT",mode="w",dataset="new"))
# (m_env(name="ORDER",value="BANANA",mode="w",dataset="new"))
# 
# (m_env(name="ORDER",value="BANANA",mode="d",dataset="new"))
# (m_env(name="list",mode="d",dataset="new"))
# 
# 
# 
# head(m_env(name="prefix.raw.data.name",mode="d"))
# head(m_env(mode="init_list"))
# 
# head(m_env(mode="init_reset"))



