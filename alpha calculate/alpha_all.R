alpha_all<-function(gatherdata,m,outputpath,Y0NAME){
  gatherdata <- gatherdata[complete.cases(gatherdata),]
  #options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
  set.seed(1234)
  policy_variable <- c("School.closing", "Workplace.closing", "Close.Public.transport", "International.travel.controls",
                       "Facial.Coverings","Restrict.Movement","Restrict.Gathering")
  state_variable <- c("HealthIndex", "PopDensity", "Aging", "Humdity", "AirTemp")
  variables <- c(policy_variable,state_variable)
  interventions<-gatherdata[,variables]
  interventions$Aging<-as.numeric(sub("%","",interventions$Aging))
  interventions$Humdity<-(interventions$Humdity-min(interventions$Humdity))/(max(interventions$Humdity)-min(interventions$Humdity))
  interventions$AirTemp<-(interventions$AirTemp-min(interventions$AirTemp))/(max(interventions$AirTemp)-min(interventions$AirTemp))
  interventions$Aging<-interventions$Aging/max(interventions$Aging)
  interventions$HealthIndex<-interventions$HealthIndex/max(interventions$HealthIndex)
  interventions$PopDensity<-interventions$PopDensity/max(interventions$PopDensity)
  Y<-as.vector(gatherdata$Y1)
  Y0<-as.vector(gatherdata[,Y0NAME])
  X1<-as.matrix(interventions[,policy_variable])
  X2<-as.matrix(interventions[,state_variable])
  dataset<- list(m=length(Y),k1=ncol(X1),k2=ncol(X2),X1=X1,X2=X2,Y=Y,Y0=Y0)
  fit<-rstan::sampling(object = m,data=dataset,iter=2000,warmup=200,
                       chains=5,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 12))
  saveRDS(fit, file = outputpath)
  #tp<-traceplot(fit,pars="alpha")
  return(fit)
}


