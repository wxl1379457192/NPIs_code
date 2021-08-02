gather_dataset <- function (Dataset, strata,N) {
  ######N：number of mean Y0######
  CB <- list()
  #WaveNum<-list()
  #source('FunCut.R')
  residual.effect <- 0
  for (i in 1:nrow(strata)) {
    d <- subset(Dataset, Dataset$CountryName == strata$CountryName[i])
    g <- list()
    for(w in 1:max(d$wave)){
      W1<-subset(d,d$wave==w)
      m <- list()
      if(nrow(W1)>6){
        for (k in 1:round(nrow(W1) / 7)) {
          l <- 7 * (k - 1) + 1
          r <- dplyr::slice(W1, breaks = l:(l + 6))
          df <- data.frame(Start = character(1),Wave=character(1), Week = character(1), Country = character(1),
                           Continent = character(1), CountryCode = character(1), Group = character(1),
                           School.closing = numeric(1), Workplace.closing = numeric(1), Cancel.Public.events = numeric(1),
                           Restrictions.on.gatherings = numeric(1), Close.Public.transport = numeric(1), Stay.at.home.requirements = numeric(1),
                           Restrictions.on.internalmovement = numeric(1), International.travel.controls = numeric(1), Facial.Coverings = numeric(1),
                           Pop = numeric(1), HealthIndex = numeric(1), PopDensity = numeric(1), Aging = numeric(1),
                           Humdity = numeric(1), AirTemp = numeric(1), New_cases = numeric(1), New_deaths = numeric(1),
                           New_cases_smoothed = numeric(1),New_cases_cum = numeric(1),Vacc=numeric(1))
          df$Start <- r$Date[1]
          df$Week <- k
          df$Wave <- r$wave[1]
          df$Country <- r$CountryName[1]
          df$Continent <- r$Continent[1]
          df$CountryCode <- r$CountryCode[1]
          df$Group <- strata$Group[i]
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
          df$Vacc<-max(r$Vaccinations_preHur)
          #df$NewCase_perHun<-df$New_cases/df$Pop*100
          m[[k]] <- df
        }
      }
      g[[w]]<-do.call(rbind, m)
    }
    gather_data <-do.call(rbind, g)
    for (l in 1:nrow(gather_data)){
      if (length(gather_data$New_cases[l-1])>0){
        gather_data$Natural.immunities[l]<-gather_data$New_cases_cum[l-1]/gather_data$Pop[1]
      }else{gather_data$Natural.immunities[l]<-0
    }}
    print(paste0(strata$CountryName[i], " wave number is ", max(gather_data$Wave)))
    gather_data$Y1 <- 0
    gather_data<-do.call(rbind,lapply(split(gather_data,gather_data$Wave),
                                      FUN=function(v){
                                        v<-v[c(which(v$New_cases_smoothed>0)[1]:nrow(v)),]
                                        for(j in 1:nrow(v)){v$Y1[j]<- ifelse(is.na(v$New_cases_smoothed[j - 1][1]), 0,
                                                                             (v$New_cases_smoothed[j] / v$New_cases_smoothed[j - 1]))}
                                        v$Y1[which(is.infinite(v$Y1))] <- 0
                                        v$Y1[which(is.nan(v$Y1))] <- 0
                                        v$Y1[which(v$Y1<0)] <- 0
                                        Y0_forW<-sort(v$Y1[which(v$Y1<10&v$Y1>1)],decreasing=T)[1:N]
                                        Y0<-sum(Y0_forW[which(is.na(Y0_forW)==F)])/length(which(is.na(Y0_forW)==F))
                                        v$Y0 <- Y0
                                        v$Y2 <- v$Y1/v$Y0
                                        v<-v[-1,]
                                        v$Week<-v$Week-1
                                      return(v)}))
    Y0_all<-mean(unique(gather_data$Y0))
    gather_data$Y0_all <- Y0_all
    CB[[i]] <- gather_data
  }
  CBD <- do.call(rbind, CB)
  return(CBD)
}