merge_dataset <-
  function(COVID_D1,
           Policy_P1,
           POPDATA,
           IND_DATA,
           ENV_DATA) 
    {

    source('smooth_Gaussian.R')
    source('plot_code/waveplot.R')
    source('select_2020data.R')
    source('delay_report.R')
    ############# read epidemic and policy data ################
    ##########ilness-onset delay######
    D1 <- Reported_delay(COVID_D1,Policy_P1)
    #write.csv( D1 ,file="dataset/COVID_delay_0722.csv",row.names = F)
    P1 <- read.csv(Policy_P1, stringsAsFactors = FALSE)
    #D1<-read.csv(url(COVID_D1))
    #P1<-read.csv(url(Policy_P1))
    #D1<-read.csv("Dataset/COVID_data_delay0622.csv",stringsAsFactors = FALSE)
    ############# only consider nation-level data ################
    P1$RegionName <- as.character(P1$RegionName)
    P1 <- subset(P1, P1$RegionName == "")
    Country_D1 <- unique(D1$location)
    Country_P1 <- unique(P1$CountryName)
    print(
      paste0(
        "Epidemic dataset has ",
        length(Country_D1),
        " countries and policy dataset has ",
        length(Country_P1),
        " countries"
      )
    )
    
    ############# restructure epidemic data into [iso_code  continent  location  date  new_cases  new_deaths] ################
    D1 <- D1[, c(1, 2, 3, 4, 6, 9, 36,41,61)]
    D1$date <- as.Date(D1$date)
    colnames(D1) <-
      c("CountryCode",
        "Continent",
        "CountryName",
        "Date",
        "New_cases_report",
        "New_deaths",
        "Vaccinations",
        "Vaccinations_preHur",
        "New_cases"
      )
    D1[is.na(D1)]<-0
    ########vaccian data#######
    V<-D1[,c(1,2,3,4,7,8)]
    ############## waves ############
    D1<-D1[,-c(7,8)]
    wave_data<-read.csv("dataset/WaveTime2_0624.csv",stringsAsFactors = FALSE)
    wave_data$Start<-as.Date(wave_data$Start)-7
    wave_data$End<-as.Date(wave_data$End)
    D1_list<-lapply(split(D1,D1$CountryName),function(v){
      v<-v[order(as.Date(v$Date)),]
      v$New_cases_cum <- cumsum(v$New_cases_report)
      wavetime<-wave_data[which(wave_data$Country==v$CountryName[1]),]
      v$wave<-0
      v$New_cases_smoothed <- 0
      if(nrow(wavetime)>0){
        temp <- smooth_Gaussian(v$New_cases)
        temp[which(temp<0)] <- 0
        v$New_cases_smoothed[1:(length(v$New_cases_smoothed)-6)] <- rollmean(temp,7)
        for(i in 1:nrow(wavetime)){
          if(i==1){ start<-1
          }else{start<-which(v$Date==wavetime$Start[i])}
          if(i==nrow(wavetime)){end<-which(v$Date==max(v$Date))
          }else{end<-which(v$Date==wavetime$End[i])}
          if (end-start>1){v$wave[start:end]<-i
          }else{print(paste0(v$CountryName[1],"did not have COVID"))}
        }
        wave1<-subset(v,v$wave==1)
        if(max(wave1$New_cases_smoothed)>300){
          if(which(wave1$New_cases_smoothed>20)[1]>7){wave1<-wave1[(c(which(wave1$New_cases_smoothed>20)[1]-7):nrow(wave1)),]
          }else{wave1<-wave1[c(1:nrow(wave1)),]}
        }else {if(which(wave1$New_cases_smoothed>10)[1]>7){wave1<-wave1[(c(which(wave1$New_cases_smoothed>10)[1]-7):nrow(wave1)),]
          }else{wave1<-wave1[c(1:nrow(wave1)),]}
        }
        v<-do.call(rbind,list(wave1,subset(v,v$wave>1)))
          ################
        #return(v)
        }})
    D1<-do.call(rbind,D1_list)
    starttime<-lapply(split(D1,D1$CountryName),FUN=function(v){
      d<-v[which(v$wave==1)[1],c("Date","CountryName")]
      d$Start<-as.Date(d$Date)+7
      return(d)})
    ss<-do.call(rbind,starttime)
    write.csv(ss,file = "dataset/wave1_start.csv",row.names = F)
    #########plot wave######################################
    waveplot(D1,wave_data)
    ############################################
    colnames(D1)<-c("CountryCode", "Continent", "CountryName",
                    "Date", "New_cases_report", "New_deaths",
                    "New_cases","New_cases_cum","wave","New_cases_smoothed")
    ##############################
    ############# restructure policy data into [CountryName  CountryCode  Date  9 + 3 index policies] ################
    P1 <- P1[, c(1, 2, 6, 7, 9, 11, 13, 15, 17, 19, 21, 33)]
    P1$Date <- as.character(P1$Date)
    Date <-
      paste0(substring(P1$Date, 1, 4),
             "-",
             substring(P1$Date, 5, 6),
             "-",
             substring(P1$Date, 7, 8))
    P1$Date <- as.Date(Date)
    
    ############# read state variables ################
    POP <- read.csv(POPDATA, stringsAsFactors = FALSE)
    #POP<-subset(POP,POP$T_pop_2020>=1000000)
    Index <- read.csv(IND_DATA, stringsAsFactors = FALSE)
    Index <- Index[,-c(2, 3, 4)]
    colnames(Index) <-
      c("CountryCode", "HealthIndex", "PopDensity", "Aging")
    Hum <- read.csv(ENV_DATA, stringsAsFactors = FALSE)
    #Hum <- read.csv(ENV_DATA, stringsAsFactors = FALSE)
    Hum$Day<- as.Date(Hum$Day)
    colnames(Hum) <- c("CountryCode", "Date", "Humidity", "AirTemp")

    Con_list <-intersect(D1$CountryCode, P1$CountryCode) # the countries exist in both epidemic dataset and policy dataset
    print(paste0("There are ", length(Con_list), " countries in both datasets"))
    
    SH<-select_data(school_holiday)
    M1 <- list()
    
    for (i in 1:length(Con_list)) {
      d <-
        subset(D1, D1$CountryCode == Con_list[i])[,-c(1, 3)] # epidemic data for country: Con_list(i)
      p <-
        subset(P1, P1$CountryCode == Con_list[i]) # policy data for country: Con_list(i)
      ind <-
        subset(Index, Index$CountryCode == Con_list[i]) # popdensity health aging data for country: Con_list(i)
      h <-
        subset(Hum, Hum$CountryCode == Con_list[i])[,-1] # environmental data for country: Con_list(i)
      sh <-
        subset(SH, SH$CountryCode == Con_list[i])[,c(-1,-3)] # school holiday data for country: Con_list(i)
      v<-subset(V,V$CountryCode == Con_list[i])[,c(-1,-2,-3)]
      len <- c(nrow(d), nrow(p), nrow(ind), nrow(h),nrow(sh),nrow(v)) # how many samples for the above variables
      
      if (min(len) > 0) {
        d$POP <- POP$T_pop_2020[which(POP$iso3c == Con_list[i])] # add population as a new variable in epidemic dataset
        m <- merge(d, p, by = "Date") # combine epidemic and policy data by date
        m <- merge(m, ind, by = "CountryCode") # further incorporate popdensity health aging data
        m <- merge(m, h, by = "Date") #  further incorporate environmental data vy date
        m <- merge(m, sh, by="Date")
        m <- merge(m, v, by="Date")
        m$C1_School.closing<-ifelse(m$C1_School.closing==0,ifelse(m$Holiday>0,1,0),m$C1_School.closing)
        #m<-m[,-1]
        M1[[i]] <- m

      }
      else{
        print(paste0("Country ", Con_list[i], " have some data missing"))
      }
    }
    Mergedata <- do.call(rbind, M1)
    return(Mergedata)
  }