library(gdata)
rm(list=ls())

filename <- "/Users/wubiya/Dropbox/IAQF 2016/2016/Data/BB_Oil_Price_Historical.xlsx"
oil <- read.xls (filename, sheet = 1, header = TRUE)
oil <- oil[-1,]
colnames(oil) <- c("Date", "Price")
oil$Date <- as.Date(as.character(oil$Date))
oil$Price <- as.numeric(levels(oil$Price))[oil$Price]
oil$Chg <- numeric(length(oil$Date))
oil$Chg[-1] <- diff(oil$Price)/oil$Price[-length(oil$Price)]

filename <- "/Users/wubiya/Dropbox/IAQF 2016/2016/Data/BB_Industry_Level_Aggregates.xlsx"
industry <- read.xls (filename, sheet = 7, header = TRUE)
industry <- industry[-1,]
colnames(industry) <- c("Date", "Price")
industry$Date <- as.Date(as.character(industry$Date))
industry$Price <- as.numeric(levels(industry$Price))[industry$Price]
industry$Chg <- numeric(length(industry$Date))
industry$Chg[-1] <- diff(industry$Price)/industry$Price[-length(industry$Price)]

y <- industry$Chg[is.element(industry$Date, oil$Date)]
x <- oil$Chg[is.element(oil$Date, industry$Date)]

regdata<- data.frame(y[-1],x[-length(x)])
colnames(regdata) <- c("InduPrice","OilPrice")
ls<-lm(regdata[5500:6000,])
summary(ls)



