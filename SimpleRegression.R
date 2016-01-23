library(gdata)
library(lmtest)
library(sandwich)
rm(list=ls())

load("OIL.Rdata")
load("oil_spot.Rdata")
load("INDEX.Rdata")
load("sp500.Rdata")

industry.namelist<-c("Banks","Retailing","Automobiles&Components","Media","Insurance","Transportation","Energy","RealEstate","Software&Services","TechnologyHardware&Equipment","PharmBiotech&LifeSciences","DiversifiedFinancials","CapitalGoods","Utilities","Food&StaplesRetailing","HealthCareEquipment&Services","ConsumerDurables&Apparel","ConsumerServices","Household&PersonalProducts","CommercialProfessionalServices","FoodBeverage&Tobacco","TelecommunicationServices","Materials","Semiconductors&SemiconductorEquipment")

sp500$LOG_RTN_5D<-numeric(length(sp500$Date))
sp500$LOG_RTN_5D[6:length(sp500$Date)]<-diff(log(sp500$PX_LAST),5)

simple_regression<-function(oilnum=1,startpoint="1983-01-01",endpoint="1990-08-01"){
  for(i in 1:24){
    industry<-industry.list[[i]]
    industry$LOG_RTN_5D<-numeric(length(industry$Date))
    industry$LOG_RTN_5D[6:length(industry$Date)]<-diff(log(industry$PX_LAST),5)
    
    oil<-oil.list[[oilnum]]
    oil$LOG_RTN_5D<-numeric(length(oil$Date))
    oil$LOG_RTN_5D[6:length(oil$Date)]<-diff(log(oil$PX_LAST),5)

    #oil<-oil_spot.list[[oilnum]]
    #oil$LOG_RTN_5D<-numeric(length(oil$Date))
    #oil$LOG_RTN_5D[6:length(oil$Date)]<-diff(log(oil$PX_LAST),5)
    
    x <- oil$LOG_RTN_5D[is.element(oil$Date,industry$Date)&is.element(oil$Date,sp500$Date)&oil$Date>=startpoint&oil$Date<endpoint]
    y <- industry$LOG_RTN_5D[is.element(industry$Date,oil$Date)&is.element(industry$Date,sp500$Date)&industry$Date>=startpoint&industry$Date<endpoint]
    z<-sp500$LOG_RTN_5D[is.element(sp500$Date,oil$Date)&is.element(sp500$Date,industry$Date)&sp500$Date>=startpoint&sp500$Date<endpoint]
    
    if(length(x)>0){
      #x<-x[seq(1,length(x),5)]
      #y<-y[seq(1,length(y),5)]
      regdata<- data.frame(y[3:length(y)],z[3:length(z)],x[3:length(x)],x[2:(length(x)-1)],x[1:(length(x)-2)])
      colnames(regdata) <- c("industry(t)","sp500(t)","oil(t)","oil(t-1)","oil(t-2)")
      #regdata<- data.frame(y[4:length(y)],z[4:length(z)],x[4:length(x)-1],x[4:length(x)-2],x[4:length(x)-3])
      #colnames(regdata) <- c("industry(t)","sp500(t-1)","oil(t-1)","oil(t-2)","oil(t-3)")
      ls = lm(regdata)
      summ <- summary(ls)
      summ$coefficients <- unclass(coeftest(ls, vcov. = NeweyWest))
      data<-summ$coefficients
      colnames(data)<-c("Estimate","StdError","t-value","Pr(>|t|)")
      filename<-paste(startpoint,' to ',endpoint,'.csv',sep='')
      write.fwf(data,filename,append=TRUE,rownames=TRUE,colnames=TRUE,rowCol=industry.namelist[i],sep=',')
    }
  }
}
simple_regression(1,"1983-01-01","2006-12-31")
simple_regression(1,"2009-02-01","2014-07-31")
simple_regression(1,"2014-08-01","2016-01-21")