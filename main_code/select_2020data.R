select_data<-function(school_holiday){
  SH<-read.csv(school_holiday, stringsAsFactors = FALSE)
  SH<-subset(SH,SH$Year>=2019)
  SH$Date<-as.Date(SH$Date)+365
  SH<-SH[,c(1:3,13)]
  new<-SH
  new$Date<-as.Date(new$Date)+365
  SH<-do.call(rbind,list(SH,new))
  colnames(SH)<-c("CountryCode","Date","CountryName","Holiday")
  return(SH)
}

