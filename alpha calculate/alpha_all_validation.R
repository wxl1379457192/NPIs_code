alpha_all_validation<-function(gatherdata,m,outputpath,validationdata,Y0NAME){
  gatherdata <- gatherdata[complete.cases(gatherdata),]
  #options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
  set.seed(1234)
  policy_variable <- c("School.closing", "Workplace.closing", "Close.Public.transport", "International.travel.controls",
                       "Facial.Coverings","Restrict.Movement","Restrict.Gathering")
  state_variable <- c("HealthIndex", "PopDensity", "Aging", "Humdity", "AirTemp")
  variables <- c(policy_variable,state_variable)
  interventions<-gatherdata[,variables]
  interventions$Humdity<-(interventions$Humdity-min(interventions$Humdity))/(max(interventions$Humdity)-min(interventions$Humdity))
  interventions$AirTemp<-(interventions$AirTemp-min(interventions$AirTemp))/(max(interventions$AirTemp)-min(interventions$AirTemp))
  interventions$Aging<-interventions$Aging/max(interventions$Aging)
  interventions$HealthIndex<-interventions$HealthIndex/max(interventions$HealthIndex)
  interventions$PopDensity<-interventions$PopDensity/max(interventions$PopDensity)
  Y<-as.vector(gatherdata$Y1)
  Y0<-as.vector(gatherdata[,Y0NAME])
  X1<-as.matrix(interventions[,policy_variable])
  X2<-as.matrix(interventions[,state_variable])
  X1_new<-as.matrix(validationdata[,policy_variable])
  X2_new<-as.matrix(validationdata[,state_variable])
  Y0_new<-as.vector(validationdata[,Y0NAME])
  Y1_new<-as.vector(validationdata$Y1)
  dataset<- list(m=length(Y),n=length(Y0_new),k1=ncol(X1),k2=ncol(X2),X1=X1,X2=X2,Y=Y,Y0=Y0,Y0_new=Y0_new,X1_new=X1_new,X2_new=X2_new,Y1_new=Y1_new)
  fit<-rstan::sampling(object = m,data=dataset,iter=2000,warmup=200,
                       chains=5,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 12))
  saveRDS(fit, file = outputpath)
  #tp<-traceplot(fit,pars="alpha")
  return(fit)
}

