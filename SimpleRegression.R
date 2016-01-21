

y <- industry$Chg[is.element(industry$Date, oil$Date)]
x <- oil$Chg[is.element(oil$Date, industry$Date)]

y <- cumsum(y)
x <- cumsum(x)

x <- x[seq(1,length(x),22)]
y <- y[seq(1,length(y),22)]

x<- diff(x)
y<- diff(y)

regdata<- data.frame(y[-1],x[-length(x)])
colnames(regdata) <- c("InduChg","OilChg")

ls<-lm(regdata)
print("OLS regression result")
print(summary(ls))

ls<-VAR(regdata)
print("VAR regression")
print(summary(ls))

regdata<- data.frame(y[-1],x[-length(x)],y[-length(y)])
colnames(regdata) <- c("InduChg","OilChg","InduCng_Pre")

ls<-lm(regdata)
print("add the previous month as another factor")
print(summary(ls))


