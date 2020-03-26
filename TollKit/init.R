
LIBRS <- c('quantmod','stringr','fTrading','xts','TTR','roxygen2')
sapply(LIBRS,library,character.only=TRUE)
#local devel fubction
setwd("/home/linus/ProjectStock/ExtraPackages/linus/stock.Analyze")
roxygenize()
library("stock.Analyze")

#reset configure file
head(env(mode="init_reset"))
head(env(mode="list"))

#init basic options
prefix.raw.data.name <- "RAW."
env(name="prefix.raw.data.name",value=prefix.raw.data.name ,mode="w")

raw.data.Listname <- ".raw.data.Listname.csv"
env(name="raw.data.Listname",value=raw.data.Listname ,mode="w")

all.code.List <- "all_codes.csv"
env(name="all.code.List",value=all.code.List ,mode="w")

prefix.stock.extension <- ".TW"
env(name="prefix.stock.extension",value=prefix.stock.extension,mode="w")

prefix.Washed <- "Washed."
env(name="prefix.Washed",value=prefix.Washed,mode="w")

prefix.Washed.AllListname <- "Washed.Listname."
env(name="prefix.Washed.AllListname",value=prefix.Washed.AllListname,mode="w")

prefix.Filter <- "Filter."
env(name="prefix.Filter",value=prefix.Filter,mode="w")

prefix.Rlist <- "REPORT."
env(name="prefix.Rlist",value=prefix.Rlist,mode="w")

prefix.Finished <- "Finished DataWashing."
env(name="prefix.Finished",value=prefix.Finished,mode="w")

prefix.MsgFor.Delete <- "DELETEED."
env(name="prefix.MsgFor.Delete",value=prefix.MsgFor.Delete,mode="w")

mark.Deleted <- "----DELETE----"
env(name="mark.Deleted",value=mark.Deleted,mode="w")

mark.Splite <- ","
env(name="mark.Splite",value=mark.Splite,mode="w")

prefix.washed.list <- "WashList."
env(name="prefix.washed.list",value=prefix.washed.list,mode="w")

stock.selected.Files_Extension <- ".csv"
env(name="stock.selected.Files_Extension",value=stock.selected.Files_Extension,mode="w")

crawl.check <- ".crawl.check.csv"
env(name="crawl.check", value=crawl.check, mode="w")


