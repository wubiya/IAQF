library(gdata)
library(lmtest)
library(sandwich)
rm(list=ls())

load("OIL.RData")
load("INDEX.RData")

industry.namelist<-c("Banks","Retailing","Automobiles&Components","Media","Insurance","Transportation",
                     "Energy","RealEstate","Software&Services","TechnologyHardware&Equipment",
                     "PharmBiotech&LifeSciences","DiversifiedFinancials","CapitalGoods","Utilities",
                     "Food&StaplesRetailing","HealthCareEquipment&Services","ConsumerDurables&Apparel",
                     "ConsumerServices","Household&PersonalProducts","CommercialProfessionalServices",
                     "FoodBeverage&Tobacco","TelecommunicationServices","Materials",
                     "Semiconductors&SemiconductorEquipment")

simple_regression<-function(oilnum=1,startpoint="1983-01-01",endpoint="1990-08-01",filename="1.csv"){
  for(i in 1:24){
    industry<-industry.list[[i]]
    industry$LOG_RTN_5D<-numeric(length(industry$Date))
    industry$LOG_RTN_5D[6:length(industry$Date)]<-diff(log(industry$PX_LAST),5)
    
    oil<-oil.list[[oilnum]]
    oil$LOG_RTN_5D<-numeric(length(oil$Date))
    oil$LOG_RTN_5D[6:length(oil$Date)]<-diff(log(oil$PX_LAST),5)

    x <- oil$LOG_RTN_5D[is.element(oil$Date, industry$Date)&oil$Date>=startpoint&oil$Date<endpoint]
    y <- industry$LOG_RTN_5D[is.element(industry$Date, oil$Date)&industry$Date>=startpoint&industry$Date<endpoint]
    if(length(x)>0){
      x<-x[seq(1,length(x),5)]
      y<-y[seq(1,length(y),5)]
      regdata<- data.frame(y[3:length(y)],x[3:length(x)],x[2:(length(x)-1)],x[1:(length(x)-2)])
      colnames(regdata) <- c("industry(t)","oil(t)","oil(t-1)","oil(t-2)")
      ls = lm(regdata)
      summ <- summary(ls)
      summ$coefficients <- unclass(coeftest(ls, vcov. = NeweyWest))
      data<-summ$coefficients
      colnames(data)<-c("Estimate","StdError","t-value","Pr(>|t|)")
      write.fwf(data,filename,append=TRUE,rownames=TRUE,colnames=TRUE,rowCol=industry.namelist[i])
    }
  }
}

#start point-1990/07 (before Gulf War)
simple_regression(1,"1983-01-01","1990-08-01","1.csv")
#1990/08-1997/06 (before Asian financial crisis)
simple_regression(1,"1990-08-01","1997-07-01","2.csv")
#1997/07-2001/08 (before 911 atack)
simple_regression(1,"1997-07-01","2001-09-11","3.csv")
#2001/09-2008/08 (before financial crisis)
simple_regression(1,"2001-09-11","2008-09-01","4.csv")
#2008/09-2014/07 (before oil prices decline)
simple_regression(1,"2008-09-01","2014-08-01","5.csv")
#2014/08-now (after oil prices decline)
simple_regression(1,"2014-08-01","2016-01-21","6.csv")