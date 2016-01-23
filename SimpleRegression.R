library(gdata)
library(lmtest)
library(sandwich)
library(DataCombine)

rm(list=ls())
filepath <- "~/Dropbox/IAQF 2016/2016/Data/Rdata/"
load(paste0(filepath,"OIL.Rdata"))
load(paste0(filepath,"INDEX.Rdata"))
load(paste0(filepath,"sp500.Rdata"))
AddReturn<-function(DataFrame)
{
  len_ = length(DataFrame$Date)
  DataFrame$LOG_RTN_1D<-numeric(len_)*NA
  DataFrame$LOG_RTN_1W<-numeric(len_)*NA
  DataFrame$LOG_RTN_1M<-numeric(len_)*NA
  DataFrame$LOG_RTN_1D[2:len_]<-diff(log(DataFrame$PX_LAST))
  DataFrame$LOG_RTN_1W[6:len_]<-diff(log(DataFrame$PX_LAST),5)
  DataFrame$LOG_RTN_1M[23:len_]<-diff(log(DataFrame$PX_LAST),22)
  DataFrame<-slide(DataFrame,Var="LOG_RTN_1D",NewVar="LOG_RTN_1D_LAG",slideBy=-1,reminder = FALSE)
  DataFrame<-slide(DataFrame,Var="LOG_RTN_1W",NewVar="LOG_RTN_1W_LAG",slideBy=-5,reminder = FALSE)
  DataFrame<-slide(DataFrame,Var="LOG_RTN_1M",NewVar="LOG_RTN_1M_LAG",slideBy=-22,reminder = FALSE)
  DataFrame<-slide(DataFrame,Var="LOG_RTN_1D_LAG",NewVar="LOG_RTN_1D_LAG_SQ",slideBy=-1,reminder = FALSE)
  DataFrame<-slide(DataFrame,Var="LOG_RTN_1W_LAG",NewVar="LOG_RTN_1W_LAG_SQ",slideBy=-5,reminder = FALSE)
  DataFrame<-slide(DataFrame,Var="LOG_RTN_1M_LAG",NewVar="LOG_RTN_1M_LAG_SQ",slideBy=-22,reminder = FALSE)
  return (DataFrame)
}

simple_regression<-function(sdate,tdate,inducol,oilcol,spcol){
  # sdate,tdate is the start/end date of regression, format is like "2015-1-1"
  # inducol is the column you want to use to represent return, including frequency of day,week,and month 
  # which is represented by LOG_RTN_1D,LOG_RTN_1W,LOG_RTN_1M
  # oilcol and spcol is independent variable as log return, which is similar to inducol, containing different frequency, 
  # but we introduced lag term, can be represented as LOG_RTN_1D_LAG(one term lag),LOG_RTN_1D_LAQ_SQ(two tern lag)
  
  if (file.exists(paste0("~/Dropbox/IAQF/code/csvfile/",sdate," to ",tdate,'.csv')))
      file.remove(paste0("~/Dropbox/IAQF/code/csvfile/",sdate," to ",tdate,'.csv'))
  
  sink(paste0("~/Dropbox/IAQF/code/csvfile/",sdate," to ",tdate,'.csv'),append=TRUE)
  cat("Industry",c(paste("Oil", oilcol, sep="_"),paste("SP500",spcol,sep="_")),'\n',sep=",")
  sdate<-as.Date(sdate)
  tdate<-as.Date(tdate)
  for(i in 1:24){
    
    
    
    industry<-industry.list[[i]]
    industry<-AddReturn(industry)
    
    subindustry<-industry[industry$Date> sdate & industry$Date < tdate & industry$Date %in% oil$Date,]
    suboil <- oil[oil$Date%in%subindustry$Date,]
    subsp500 <- sp500[sp500$Date%in%subindustry$Date,]
    
    regdata<- data.frame(subindustry[inducol],suboil[oilcol],subsp500[spcol])
    colnames(regdata) <-c( paste(industry.namelist[i], inducol, sep="_"), paste("Oil", oilcol, sep="_"),paste("SP500",spcol,sep="_"))
    
    ls = lm(regdata)
    summ <- summary(ls)
    cat(industry.namelist[i],unname(summ$coefficients[-1,3]),'\n',sep=",")
    
  }
  sink()
  unlink(paste0("~/Dropbox/IAQF/code/csvfile/",sdate," to ",tdate,'.csv'))
}


sp500<-AddReturn(sp500)
oil<-oil.list[[1]]
oil<-AddReturn(oil)

sp500<-sp500[sp500$Date%in%oil$Date,]
oil<-oil[oil$Date%in%sp500$Date,]

sdate<-"2009-1-1"
tdate<-"2014-1-1"
inducol <- "LOG_RTN_1D"
oilcol <- c("LOG_RTN_1D","LOG_RTN_1D_LAG")
spcol <-c("LOG_RTN_1D","LOG_RTN_1D_LAG")


simple_regression(sdate,tdate,inducol,oilcol,spcol)
