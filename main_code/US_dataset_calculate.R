library("stats")
library("base")
library("zoo")

waveplot2<-function(D1,wave_data){
  D1$Date<-as.Date(D1$Date)
  theme_set(theme_minimal(base_size=10, base_family="Times New Roman"))
  waveplot_list<-lapply(split(D1,D1$StatesID),function(v){
    C <- v[, c("Date", "New_cases")]
    wavetime<-wave_data
    if(nrow(wavetime)>0){
      plot <- ggplot(data = C, aes(x = Date, y = New_cases)) +
        geom_rect(aes(xmin=wavetime$Start[1], xmax=wavetime$End[1], ymin=-Inf, ymax=Inf),fill=alpha('#ece3da',0.2),color=NA)+
        geom_rect(aes(xmin=wavetime$Start[2], xmax=wavetime$End[2], ymin=-Inf, ymax=Inf),fill=alpha('#dedede',0.1),color=NA)+
        geom_rect(aes(xmin=wavetime$Start[3], xmax=max(v$Date), ymin=-Inf, ymax=Inf),fill=alpha('#eeded2',0.1),color=NA)+
        xlim(min(D1$Date),max(D1$Date))+ geom_line(size=0.4) +
        scale_colour_manual(values=c("#2f5483","#cd6e00"))+ labs(x=NULL,y=NULL,title = v$CountryName[1])+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5,vjust=0,size=10,family = "Times New Roman"),
              plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm"),
              axis.text = element_text(size=7,vjust=0,hjust=0.7,family = "Times New Roman"),
              panel.background = element_rect(fill="white"),axis.line=element_line(color="black"))
      ggsave(sprintf("US_V2/picture/wavetime/%s-Gaussian.jpg", v$StatesID[1]), plot, width = 8, height =6)
      }
    else{print(paste0(v$StatesID[1],"did not have COVID"))}
    return(plot)
  })
}

waveplot<-function(D1,wave_data){
  theme_set(theme_minimal(base_size=10, base_family="Times New Roman"))
  waveplot_list<-lapply(split(D1,D1$StatesID),function(v){
    C <- v[, c("Date", "New_cases", "New_cases_smoothed")]
    longmean <- melt(as.data.frame(C), id = "Date")
    wavetime<-wave_data[which(wave_data$StatesID==v$StatesID[1]),]
    if(nrow(wavetime)>0){
      if(nrow(wavetime)==1){
        plot <- ggplot(data = longmean, aes(x = Date, y = value, color = variable)) +
          geom_rect(aes(xmin=wavetime$Start[1], xmax=max(v$Date), ymin=-Inf, ymax=Inf),fill=alpha('#ece3da',0.2),color=NA)+
          geom_line(size=0.4) +xlim(min(D1$Date),max(D1$Date))+
          scale_colour_manual(values=c("#2f5483","#cd6e00"))+ labs(x=NULL,y=NULL,title = v$CountryName[1])+
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5,vjust=0,size=10,family = "C",face="bold"),
                plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm"),
                axis.text = element_text(size=7,vjust=0,hjust=0.7,family = "C",face="bold"),
                panel.background = element_rect(fill="white"),axis.line=element_line(color="black"))}
      if(nrow(wavetime)==2){plot <- ggplot(data = longmean, aes(x = Date, y = value, color = variable)) +
        geom_rect(aes(xmin=wavetime$Start[1], xmax=wavetime$End[1], ymin=-Inf, ymax=Inf),fill=alpha('#ece3da',0.2),color=NA)+
        geom_rect(aes(xmin=wavetime$Start[2], xmax=max(v$Date), ymin=-Inf, ymax=Inf),fill=alpha('#dedede',0.1),color=NA)+
        geom_line(size=0.4) +xlim(min(D1$Date),max(D1$Date))+
        scale_colour_manual(values=c("#2f5483","#cd6e00"))+ labs(x=NULL,y=NULL,title = v$CountryName[1])+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5,vjust=0,size=10,family = "Times New Roman"),
              plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm"),
              axis.text = element_text(size=7,vjust=0,hjust=0.7,family = "Times New Roman"),
              panel.background = element_rect(fill="white"),axis.line=element_line(color="black"))}
      else{plot <- ggplot(data = longmean, aes(x = Date, y = value, color = variable)) +
        geom_rect(aes(xmin=wavetime$Start[1], xmax=wavetime$End[1], ymin=-Inf, ymax=Inf),fill=alpha('#ece3da',0.2),color=NA)+
        geom_rect(aes(xmin=wavetime$Start[2], xmax=wavetime$End[2], ymin=-Inf, ymax=Inf),fill=alpha('#dedede',0.1),color=NA)+
        geom_rect(aes(xmin=wavetime$Start[3], xmax=max(v$Date), ymin=-Inf, ymax=Inf),fill=alpha('#eeded2',0.1),color=NA)+
        geom_line(size=0.4) +xlim(min(D1$Date),max(D1$Date))+
        scale_colour_manual(values=c("#2f5483","#cd6e00"))+ labs(x=NULL,y=NULL,title = v$CountryName[1])+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5,vjust=0,size=10,family = "Times New Roman"),
              plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm"),
              axis.text = element_text(size=7,vjust=0,hjust=0.7,family = "Times New Roman"),
              panel.background = element_rect(fill="white"),axis.line=element_line(color="black"))}
      #ggsave(sprintf("picture/wavetime/%s-Gaussian.pdf", v$CountryName[1]), plot, width = 6, height = 5, units = "cm",device = cairo_pdf)
      ggsave(sprintf("US_V2/picture/wavetime/%s-Gaussian.jpg", v$StatesID[1]), plot, width = 8, height =6)
    }
    else{print(paste0(v$StatesID[1],"did not have COVID"))}
    return(plot)
  })
}

Reported_delay <-function(COVID_D1,Policy_P1) 
{
  ############# read epidemic and policy data ################
  D1 <- read.csv(COVID_D1, stringsAsFactors = FALSE)
  P1 <- read.csv(Policy_P1, stringsAsFactors = FALSE)
  for(i in 1:nrow(P1)){
    P1$RegionCode[i]<-substr(P1$RegionCode[i],4,5)
  }
  D1$submission_date<-as.Date(D1$submission_date)
  Con_list <-intersect(D1$state, P1$RegionCode) 
  D1[is.na(D1)]<-0
  D1$submission_date <- as.Date(D1$submission_date)
  dd<-split(D1,D1$state)
  DD_list<-do.call(rbind,lapply(Con_list,function(x){
    v<-dd[[x]]
    v$New_cases_delay<-0
    for(i in 1:nrow(v)){
      if(v$new_case[i]>=0){
        mean<-rnorm(1,10.92,0.94)
        sd<-rnorm(1,5.41,0.27)
        x1 <- round(rnbinom(v$new_case[i],mu=mean,size=sd))
        t<-as.data.frame(table(x1))
        if(nrow(t)>0){
          for (n in 1:nrow(t)){
            date <- v$submission_date[i]-as.numeric(paste(t$x1))[n]
            if (nrow(subset(v,submission_date==date))>0){
              v$New_cases_delay[v$submission_date==date]<-
                v$New_cases_delay[v$submission_date==date]+t$Freq[n]}
          }
        }
      }else{
        mean<-rnorm(1,10.92,0.94)
        sd<-rnorm(1,5.41,0.27)
        x1 <- round(rnbinom(-v$new_case[i],mu=mean,size=sd))
        t<-as.data.frame(table(x1))
        if(nrow(t)>0){
          for (n in 1:nrow(t)){
            date <- v$date[i]-as.numeric(paste(t$x1))[n]            
            if (nrow(subset(v,submission_date==date))>0){
              v$New_cases_delay[v$submission_date==date]<-
                v$New_cases_delay[v$submission_date==date]-t$Freq[n]}
          }
        }
      }
    }
    print(paste(x,"has been finished"))
    return(v)
  }))
  return(DD_list)
}
smooth_Gaussian <-function(cases){
  
  
  r <- 7
  
  cases_smoothed <- cases
  
  GaussTemp <- c(0.0004,0.0022,0.0088,0.0270,0.0648,0.1210,0.1761,0.1995,0.1761,0.1210,0.0648,0.0270,0.0088,0.0022,0.0004)
  
  for (i in (8:(length(cases)-r))){
    cases_smoothed[i] <- sum(cases[c((i-r):(i+r))]*t(GaussTemp))
  }
  
  return(cases_smoothed)
}

merge_dataset <-
  function(COVID_D1,
           Policy_P1,
           POPDATA,
           IND_DATA,
           ENV_DATA) 
  {
    ############# read epidemic and policy data ################
    ##########ilness-onset delay######
    D1 <- Reported_delay(COVID_D1,Policy_P1)
    P1 <- read.csv(Policy_P1, stringsAsFactors = FALSE)
    for(i in 1:nrow(P1)){
      P1$RegionCode[i]<-substr(P1$RegionCode[i],4,5)
    }
    #write.csv(D1,file = "US_V2/COVID_Infections_dataset.csv",row.names = F)
    #D1<-read.csv("US_V2/COVID_Infections_dataset.csv")
    #D1<-read.csv(url(COVID_D1))
    #P1<-read.csv(url(Policy_P1))
    ############# only consider nation-level data ################
    Country_D1 <- unique(D1$state)
    Country_P1 <- unique(P1$RegionCode)
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
    D1 <- D1[, c(1, 2, 3, 6, 8, 16)]
    colnames(D1) <-
      c("Date",
        "StatesID",
        "Totalcases",
        "New_cases_report",
        "New_deaths",
        "New_cases"
      )
    D1$Date <- as.Date(D1$Date)
    D1[is.na(D1)]<-0
    

    wave_data<-read.csv("US_V2/US_wavetime.csv",stringsAsFactors = FALSE)
    wave_data$Start<-as.Date(wave_data$Start)
    wave_data$End<-as.Date(wave_data$End)
    

    D1_list<-lapply(split(D1,D1$StatesID),function(v){
      v<-v[order(as.Date(v$Date)),]
      wavetime<-wave_data[which(wave_data$StatesID==v$StatesID[1]),]
      v$New_cases_cum <- cumsum(v$New_cases_report)
      v$wave<-0
      v$New_cases_smoothed <- 0
      if(nrow(wavetime)>0){
        temp <- smooth_Gaussian(v$New_cases)
        temp[which(temp<0)] <- 0
        v$New_cases_smoothed[1:(length(v$New_cases_smoothed)-6)] <- rollmean(temp,7)
        for(i in 1:nrow(wavetime)){
          start<-wavetime$Start[i]
          if(i==nrow(wavetime)){end<-max(v$Date)
          }else{end<-wavetime$End[i]}
          if (end-start>1){v$wave[which(v$Date>=start&v$Date<=end)]<-i
          }else{print(paste0(v$CountryName[1],"did not have COVID"))}
        }
        ################
        return(v)
      }})

    D1<-do.call(rbind,D1_list)

    ############################################
    #waveplot(D1,wave_data)
    ############# restructure policy data into [CountryName  CountryCode  Date  9 + 3 index policies] ################
    P1 <- P1[, c(3, 4, 6, 7, 9, 11, 13, 15, 17, 19, 21, 33)]
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
    colnames(Index) <-
      c("States","StatesID", "HealthIndex", "PopDensity", "Aging")
    Hum <- read.csv(ENV_DATA, stringsAsFactors = FALSE)
    #Hum <- read.csv(ENV_DATA, stringsAsFactors = FALSE)
    Hum$Date<- as.Date(Hum$Date)
    colnames(Hum) <- c( "Date", "AirTemp", "States", "Humidity")
    
    Con_list <-intersect(D1$StatesID, P1$RegionCode) # the countries exist in both epidemic dataset and policy dataset
    print(paste0("There are ", length(Con_list), " countries in both datasets"))
    
    M1 <- list()
    
    for (i in 1:length(Con_list)) {
      d <-
        subset(D1, D1$StatesID == Con_list[i])# epidemic data for country: Con_list(i)
      p <-
        subset(P1, P1$RegionCode == Con_list[i])[,-2] # policy data for country: Con_list(i)
      ind <-
        subset(Index, Index$StatesID == Con_list[i]) # popdensity health aging data for country: Con_list(i)
      h <-
        subset(Hum, Hum$States == ind$States)[,-3] # environmental data for country: Con_list(i)
      
      len <- c(nrow(d), nrow(p), nrow(ind), nrow(h)) # how many samples for the above variables
      
      if (min(len) > 0) {
        d$POP <- POP$POP[which(POP$States == ind$States)] # add population as a new variable in epidemic dataset
        m <- merge(d, p, by = "Date") # combine epidemic and policy data by date
        m <- merge(m, ind[,-1], by = "StatesID") # further incorporate popdensity health aging data
        m <- merge(m, h, by = "Date") #  further incorporate environmental data vy date
        #m<-m[,-1]
        M1[[i]] <- m
        
      }else{
        print(paste0("State ", Con_list[i], " have some data missing"))
      }
    }
    Mergedata <- do.call(rbind, M1)
    return(Mergedata)
  }

gather_dataset <- function (Dataset,N=3) {
  ######N锛歯umber of mean Y0######
  CB <- list()
  #WaveNum<-list()
  #source('FunCut.R')
  residual.effect <- 0
  CC<-lapply(split(Dataset,Dataset$StatesID),FUN=function(d){
    g <- list()
    for(w in 1:max(d$wave)){
      W1<-subset(d,d$wave==w)
      m <- list()
      if(nrow(W1)>6){
        for (k in 1:round(nrow(W1) / 7)) {
          l <- 7 * (k - 1) + 1
          r <- dplyr::slice(W1, breaks = l:(l + 6))
          df <- data.frame(Start = character(1), Wave=character(1), Week = character(1), State = character(1),StateID = character(1),
                           School.closing = numeric(1), Workplace.closing = numeric(1),
                           Cancel.Public.events = numeric(1),Restrictions.on.gatherings = numeric(1),
                           Close.Public.transport = numeric(1), Stay.at.home.requirements = numeric(1),
                           Restrictions.on.internalmovement = numeric(1), International.travel.controls = numeric(1), Facial.Coverings = numeric(1),
                           Pop = numeric(1), HealthIndex = numeric(1), PopDensity = numeric(1), Aging = numeric(1),
                           Humdity = numeric(1), AirTemp = numeric(1), New_cases = numeric(1), New_deaths = numeric(1),
                           New_cases_smoothed = numeric(1),New_cases_cum = numeric(1))
          df$Start <- r$Date[1]
          df$Week <- k
          df$Wave <- r$wave[1]
          df$State <- r$RegionName[1]
          df$StateID <- r$StatesID[1]
          df$School.closing <-
            ifelse(sum(r$C1_School.closing)==0,residual.effect,mean(r$C1_School.closing)/3)
          df$Workplace.closing <-
            ifelse(sum(r$C2_Workplace.closing)==0,residual.effect,mean(r$C2_Workplace.closing)/3)
          df$Cancel.Public.events <-
            ifelse(sum(r$C3_Cancel.public.events)==0,residual.effect,mean(r$C3_Cancel.public.events)/2)
          df$Restrictions.on.gatherings <-
            ifelse(sum(r$C4_Restrictions.on.gatherings)==0,residual.effect,mean(r$C4_Restrictions.on.gatherings)/4)
          df$Close.Public.transport <-
            ifelse(sum(r$C5_Close.public.transport)==0,residual.effect,mean(r$C5_Close.public.transport)/2)
          df$Stay.at.home.requirements <-
            ifelse(sum(r$C6_Stay.at.home.requirements)==0, residual.effect,mean(r$C6_Stay.at.home.requirements)/3)
          df$Restrictions.on.internalmovement <-
            ifelse(sum(r$C7_Restrictions.on.internal.movement)==0,residual.effect,mean(r$C7_Restrictions.on.internal.movement)/2)
          df$International.travel.controls <-
            ifelse(sum(r$C8_International.travel.controls)==0,residual.effect,mean(r$C8_International.travel.controls)/4)
          df$Facial.Coverings <-
            ifelse(sum(r$H6_Facial.Coverings)==0,residual.effect,mean(r$H6_Facial.Coverings)/4)
          df$Restrict.Movement<-mean((df$Stay.at.home.requirements+df$Restrictions.on.internalmovement)/2)
          df$Restrict.Gathering<-mean((df$Restrictions.on.gatherings+df$Cancel.Public.events)/2)
          df$Pop <- r$POP[1]
          df$HealthIndex <- r$HealthIndex[1]
          df$PopDensity <- r$PopDensity[1]
          df$Aging <- r$Aging[1]
          df$Humdity <- mean(r$Humidity)
          df$AirTemp <- mean(r$AirTemp)
          df$New_cases <- sum(r$New_cases)
          df$New_deaths <- sum(r$New_deaths)
          df$New_cases_smoothed <- sum(r$New_cases_smoothed)
          df$New_cases_cum<- max(r$New_cases_cum)
          m[[k]] <- df
        }
      }
      g[[w]]<-do.call(rbind, m)
    }
    gather_data <-do.call(rbind, g)
    for (l in 1:nrow(gather_data)){
      if (length(gather_data$New_cases[l-1])>0){
        gather_data$Natural.immunities[l]<-gather_data$New_cases_cum[l-1]/(gather_data$Pop[1]*1000000)
      }else{gather_data$Natural.immunities[l]<-0
      }}
    gather_data$Y1 <- 0
    gather_data<-do.call(rbind,lapply(split(gather_data,gather_data$Wave),
                                      FUN=function(v){
                                        v<-v[c(which(v$New_cases_smoothed>0)[1]:nrow(v)),]
                                        for(j in 1:nrow(v)){v$Y1[j]<- ifelse(is.na(v$New_cases_smoothed[j - 1][1]), 0,
                                                                             (v$New_cases_smoothed[j] / v$New_cases_smoothed[j - 1]))}
                                        v$Y1[which(is.infinite(v$Y1))] <- 0
                                        v$Y1[which(is.nan(v$Y1))] <- 0
                                        v$Y1[which(v$Y1<0)] <- 0
                                        #v<-v[which(v$Y1<50),]
                                        Y0_forW<-sort(v$Y1[which(v$Y1<5&v$Y1>1)],decreasing=T)[1:N]
                                        Y0<-sum(Y0_forW[which(is.na(Y0_forW)==F)])/length(which(is.na(Y0_forW)==F))
                                        v$Y0 <- Y0
                                        v$Y2 <- v$Y1/v$Y0
                                        v<-v[-1,]
                                        v$Week<-v$Week-1
                                        return(v)}))
    print(paste0(unique(d$StatesID)," has been processed"))
    return(gather_data)
  })
  CBD <- do.call(rbind, CC)
  return(CBD)
}