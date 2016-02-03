library(gdata)
library(vars)

rm(list=ls())

level2numeric<-function(level_){return(as.numeric(levels(level_))[level_])}

read.oil <-function(sheetnum=1,filename ="~/Dropbox/IAQF 2016/2016/Data/OIL.xlsx")
{
oil <- read.xls (filename, sheet = sheetnum, header = TRUE)
oil <- oil[-1,]
colnames(oil) <- c("Date", "PX_LAST","PX_VOLUME","CHG_PCT_1D","CHG_PCT_5D")
oil$Date <- as.Date(as.character(oil$Date))
oil$PX_LAST<- level2numeric(oil$PX_LAST)
oil$PX_VOLUME<-level2numeric(oil$PX_VOLUME)
oil$CHG_PCT_1D <- level2numeric(oil$CHG_PCT_1D)
oil$CHG_PCT_5D <- level2numeric(oil$CHG_PCT_5D)
return (oil)
}

read.industry<-function(sheetnum =2, filename = "~/Dropbox/IAQF 2016/2016/Data/INDEX.xlsx")
{
  industry <- read.xls (filename, sheet = sheetnum, header = TRUE)
  industry <- industry[-1,]
  colnames(industry) <- c("Date", "PX_LAST","PX_VOLUME","CHG_PCT_1D","CHG_PCT_5D","TURNOVER")
  industry$Date <- as.Date(as.character(industry$Date))
  industry$PX_LAST<- level2numeric(industry$PX_LAST)
  industry$PX_VOLUME<-level2numeric(industry$PX_VOLUME)
  industry$CHG_PCT_1D <- level2numeric(industry$CHG_PCT_1D)
  industry$CHG_PCT_5D <- level2numeric(industry$CHG_PCT_5D)
  industry$TURNOVER <- level2numeric(industry$TURNOVER)
  return (industry)
}

read.swf <- function(country,path="~/Dropbox/IAQF 2016/2016/Data/SWFdata/")
{
  swf <- read.csv(paste0(path,country,".csv"))
  colnames(swf) <- c("Date","PX_LAST")
  swf$Date <- as.Date(as.character(swf$Date),format="%m/%d/%y")
  return (swf)
}

read.swf2 <- function(sheetnum, filename="~/Dropbox/IAQF 2016/2016/Data/SWF.xlsx")
{
  swf <- read.xls (filename, sheet = sheetnum, header = TRUE)
  colnames(swf) <- c("Date","PX_LAST")
  swf <- swf[-1,]
  swf$Date <- as.Date(as.character(swf$Date))
  swf$PX_LAST<- level2numeric(swf$PX_LAST)
  return (swf)
}


oil.namelist <-c("CL","CO","NG","XB","HO","QS")
oil.list<-list()

for (i in 1:6){
  oil.list[[i]]<-read.oil(i)
}

save(oil.namelist,oil.list, file="~/Dropbox/IAQF 2016/2016/Data/Rdata/OIL.Rdata")


industry.namelist<-c("Banks","Retailing","Automobiles & Components","Media","Insurance","Transportation",
                 "Energy","Real Estate","Software & Services","Technology Hardware & Equipment",
                 "Pharm Biotech & Life Sciences","Diversified Financials","Capital Goods","Utilities",
                 "Food & Staples Retailing","Health Care Equipment & Services","Consumer Durables & Apparel",
                 "Consumer Services","Household & Personal Products","Commercial Professional Services",
                 "Food Beverage & Tobacco","Telecommunication Services","Materials",
                 "Semiconductors & Semiconductor Equipment")
industry.list<-list()

for (i in 1:24)
{
  industry.list[[i]] <- read.industry(i+1)
}

save(industry.namelist,industry.list,file="~/Dropbox/IAQF 2016/2016/Data/Rdata/INDEX.Rdata")

swf.namelist <-c("mexico","algeria","russia","brazil","saudi","canada")
swf.list<-list()
for (i in 1:6)
{
  swf.list[[i]]<-read.swf(swf.namelist[i])
}


save(swf.namelist,swf.list,file="~/Dropbox/IAQF 2016/2016/Data/Rdata/SWF.Rdata")

swf2.namelist <-c("saudi","china","norway")
swf2.list<-list()
for (i in 1:3)
{
  swf2.list[[i]]<-read.swf2(i*2)
}


save(swf2.namelist,swf2.list,file="~/Dropbox/IAQF 2016/2016/Data/Rdata/SWF2.Rdata")






