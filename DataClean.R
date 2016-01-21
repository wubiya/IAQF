library(gdata)
library(vars)

rm(list=ls())

level2numeric<-function(level_){as.numeric(levels(level_))[level_]}

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


# 
# 
# y <- industry$Chg[is.element(industry$Date, oil$Date)]
# x <- oil$Chg[is.element(oil$Date, industry$Date)]
# 
# y <- cumsum(y)
# x <- cumsum(x)
# 
# x <- x[seq(1,length(x),22)]
# y <- y[seq(1,length(y),22)]
# 
# x<- diff(x)
# y<- diff(y)
# 
# regdata<- data.frame(y[-1],x[-length(x)])
# colnames(regdata) <- c("InduChg","OilChg")
# 
# ls<-lm(regdata)
# print("OLS regression result")
# print(summary(ls))
# 
# ls<-VAR(regdata)
# print("VAR regression")
# print(summary(ls))
# 
# regdata<- data.frame(y[-1],x[-length(x)],y[-length(y)])
# colnames(regdata) <- c("InduChg","OilChg","InduCng_Pre")
# 
# ls<-lm(regdata)
# print("add the previous month as another factor")
# print(summary(ls))










