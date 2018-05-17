library(installr)
# Author: Craig Wang
# Last Modified: 17th May 2018

# Download CRAN Log -------------------------------------------------------

setwd("P:/Projects/shiny")
start <- as.Date('2012-10-01')
today <- as.Date(format(Sys.time(), "%Y-%m-%d"))

all_days <- seq(start, today, by = 'day')

# only download the files you don't have:
missing_days <- setdiff(as.character(all_days), tools::file_path_sans_ext(dir("CRANlogs"), TRUE))
missing_days <- head(missing_days, length(missing_days)-1)

year <- as.POSIXlt(missing_days)$year + 1900
urls <- paste0('http://cran-logs.rstudio.com/', year, '/', missing_days, '.csv.gz')

dir.create("CRANlogs")

for (i in 1:length(missing_days)) {
  print(paste0(i, "/", length(missing_days)))
  download.file(urls[i], paste0('CRANlogs/', missing_days[i], '.csv.gz'))
}

# Extract and combine and save --------------------------------------------

# loading<-42 # break logs into 42 parts, each has 36 files
# load.times <- list(c(1:90),c(91:160),c(164:195),c(196:216))
library(data.table)
library(broom)
library(countrycode)
# process string

# largest existing file index
max.file.index <- max(as.numeric(str_extract(list.files(), "[[:digit:]]+")),na.rm = TRUE)
# total number of missing files
missing.total <- length(missing_days)
# process downloaded data in separate runs
n.runs <- round(missing.total/20)
# new file indices
cleaned_file_ID <- seq(max.file.index+1, max.file.index+n.runs)
each_contain <- missing.total/n.runs

# TODO: divide missing_days into n.runs vectors

idx.split <- split(1:missing.total,rep(1:n.runs,c(rep(20,n.runs-1),missing.total-20*(n.runs-1))))

for (i in 1:length(cleaned_file_ID)){
  file_list <- paste0("CRANlogs/",missing_days[idx.split[[i]]],".csv.gz")
  logs <- list()
  for (file in file_list) {
    print(paste("Reading", file, "..."))
    logs[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE, comment.char = "", as.is=TRUE)
  }
  
  dat <- rbindlist(logs)
  
  # add some keys and define variable types
  
  dat[, date:=as.Date(date)]
  dat[, package:=factor(package)]
  dat[, country:=factor(country)]
  dat[, week:=strftime(as.POSIXlt(date),format="%Y-%W")]
  setkey(dat, package, week, country)
  
  dat <- dat[!dat$package=="NA",]
  dat <- as.data.frame(dat)
  dat <- dat[,c(1,2,7,9,11)]
  
  # Aggregate ----------------------------------------------------------------
  
  dat$country.name <- countrycode(dat$country,"iso2c","country.name")
  dat<-as.data.table(dat)
  dat<-dat[, times := length(time), by = list(package, country.name,week)]
  dat<-unique(dat,by = c("country.name", "package","week"))
  dat<-dat[,c("date","package","country","week","country.name","times"),with=FALSE]
  # sum(dat$times) # should same as observations before
  # Final combination -------------------------------------------------------
  
  save(dat, file=paste0("CRANlog",cleaned_file_ID[i],".RData"))
  gc()
}
rm(list = ls())
# new largest existing file index
max.file.index2 <- max(as.numeric(str_extract(list.files(), "[[:digit:]]+")),na.rm = TRUE)

for (i in 1:max.file.index2){
load(paste0("CRANlog",i,".RData"))
  assign(paste0("dat",i),dat)
}
  

dat<-do.call("rbind", mget(paste0("dat",1:max.file.index2)))
dat<-dat[,c("package","country","week","country.name","times"),with=FALSE]

colnames(dat)[5]<-"time"
dat[, times := sum(time), by = list(package,country.name,week)]
dat<-dat[,c("package","country","week","country.name","times"),with=FALSE]
setkey(dat, package, week, country.name)
dat<-unique(dat,by = c("package","country.name","week"))

dat.p <- subset(dat,package=="gapfill")
aggregate(times~week, dat.p,sum)
sum(dat.p$times)

save(dat, file="CRANlog_cleaned.RData")
rm(list = ls())
load("CRANlog_cleaned.RData")

dat[,month:=format(as.Date(paste(week, 1, sep="-"), "%Y-%U-%u"),"%Y-%m")]
colnames(dat)[5]<-"time"
setkey(dat, package, month, country.name)
dat[, times := sum(time), by = list(package,country.name,month)]
dat<-unique(dat,by = c("package","country.name","month"))
dat<-dat[,c("package","country","month","country.name","times"),with=FALSE]
dat<-dat[complete.cases(dat),]
dat$month<-as.Date(paste(dat$month, 1, sep="-"), "%Y-%m-%d")

save(dat, file="CRANlog_cleaned_month.RData")
