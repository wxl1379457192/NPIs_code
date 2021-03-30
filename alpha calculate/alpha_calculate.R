alpha_calculate<- function (gatherdata,m,output,Y0NAME, totalweeks=max(gatherdata$Week)) {
  if (totalweeks > max(gatherdata$Week)){
    print(paste0("Too much week, there are only ",max(gatherdata$Week)," weeks"))
    totalweeks <- max(gatherdata$Week)}
  #options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
  set.seed(1234)
  policy_variable <- c("School.closing", "Workplace.closing", "Close.Public.transport", "International.travel.controls",
                       "Facial.Coverings","Restrict.Movement","Restrict.Gathering")
  state_variable <- c("HealthIndex", "PopDensity", "Aging", "Humdity", "AirTemp")
  variables <- c(policy_variable,state_variable)
  fit_list<-list()
  for(n in 1:max(gatherdata$Week)){
    v<-subset(gatherdata,gatherdata$Week==n)
    if (nrow(v)>=3){
      interventions<-v[,variables]
      interventions$Aging<-as.numeric(sub("%","",interventions$Aging))
      interventions$Humdity<-(interventions$Humdity-min(interventions$Humdity))/(max(interventions$Humdity)-min(interventions$Humdity))
      interventions$AirTemp<-(interventions$AirTemp-min(interventions$AirTemp))/(max(interventions$AirTemp)-min(interventions$AirTemp))
      interventions$Aging<-(interventions$Aging-min(interventions$Aging))/(max(interventions$Aging)-min(interventions$Aging))
      interventions$HealthIndex<-(interventions$HealthIndex-min(interventions$HealthIndex))/(max(interventions$HealthIndex)-min(interventions$HealthIndex))
      interventions$PopDensity<-(interventions$PopDensity-min(interventions$PopDensity))/(max(interventions$PopDensity)-min(interventions$PopDensity))
      Y<-as.vector(v$Y1)
      Y0<-as.vector(v[,Y0NAME])
      X1<-as.matrix(interventions[,policy_variable])
      X2<-as.matrix(interventions[,state_variable])
      fit<-rstan::sampling(object = m,data = list(m=length(Y),k1=ncol(X1),k2=ncol(X2),X1=X1,X2=X2,Y=Y,Y0=Y0),
                           iter=2000,warmup=200,chains=5,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 12),seed = 1111)
      fit_list[[n]]<-fit
    }
  }
  if(length(which(sapply(fit_list,is.null)))!=0){
    fit_list<-fit_list[-which(sapply(fit_list,is.null))]
  }
  saveRDS(fit_list, file = paste0(output,"/G",unique(gatherdata$Group),"-W",unique(gatherdata$Wave),"weeklist.rds"))
  #############
  rm(week_list)
}