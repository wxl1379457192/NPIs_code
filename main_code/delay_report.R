Reported_delay <-function(COVID_D1,Policy_P1) 
{
  ############# read epidemic and policy data ################
  D1 <- read.csv(COVID_D1, stringsAsFactors = FALSE)
  P1 <- read.csv(Policy_P1, stringsAsFactors = FALSE)
  #D1<-read.csv(url(COVID_D1))
  Con_list <-intersect(D1$iso_code, P1$CountryCode) 
  D1[is.na(D1)]<-0
  D1$date <- as.Date(D1$date)
  dd<-split(D1,D1$iso_code)
  DD_list<-do.call(rbind,lapply(Con_list,function(x){
    v<-dd[[x]]
    v$New_cases_delay<-0
    for(i in 1:nrow(v)){
      if(v$new_cases[i]>=0){
        mean<-rnorm(1,10.92,0.94)
        sd<-rnorm(1,5.41,0.27)
        x1 <- round(rnbinom(v$new_cases[i],mu=mean,size=sd))
        t<-as.data.frame(table(x1))
        if(nrow(t>0)){
          for (n in 1:nrow(t)){
            date <- v$date[i]-as.numeric(paste(t$x1))[n]
            if (nrow(subset(v,date==date))>0){
              v$New_cases_delay[v$date==date]<-
                v$New_cases_delay[v$date==date]+t$Freq[n]}
          }
        }
      }else{
        mean<-rnorm(1,10.92,0.94)
        sd<-rnorm(1,5.41,0.27)
        x1 <- round(rnbinom(-v$new_cases[i],mu=mean,size=sd))
        t<-as.data.frame(table(x1))
        if(nrow(t>0)){
          for (n in 1:nrow(t)){
            date <- v$date[i]-as.numeric(paste(t$x1))[n]
            if (nrow(subset(v,date==date))>0){
              v$New_cases_delay[v$date==date]<-
                v$New_cases_delay[v$date==date]-t$Freq[n]}
          }
        }
      }
    }
    print(paste(v$iso_code[1],"has been finished"))
    return(v)
  }))
  return(DD_list)
}