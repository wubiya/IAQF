library(gdata)
rm(list=ls())

filename <- "/Users/wubiya/Dropbox/IAQF 2016/2016/Data/BB_Oil_Price_Historical.xlsx"
oil <- read.xls (filename, sheet = 1, header = TRUE)
oil <- oil[-1,]
colnames(oil) <- c("Date", "Price")
oil$Date <- as.Date(as.character(oil$Date))
oil$Price <- as.numeric(levels(oil$Price))[oil$Price]
