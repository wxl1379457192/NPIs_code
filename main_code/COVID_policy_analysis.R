###############
library("curl")
library("magrittr")
library("geodetector")
library("rstan")
library("tidyr")
library("lubridate")
library("stringr")
library("dplyr")
library("EnvStats")
library("optparse")
library("sqldf")
library("bayesplot")
library("reshape2")
library("zoo")
library("patchwork")
library("scales")
library("grid")
library("ggsci")
library("ggplot2")
library("gridExtra")
library("cowplot")
library("ggrepel")
library("rstanarm")
library("ggridges")
library("loo")
library("ggcor")
setwd("")#set up your setwd here
Sys.setlocale("LC_TIME","English")
#COVID_D1<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"#epidemical data source
#Policy_P1<-"https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"#policy database
COVID_D1<-"dataset/owid-covid-data.csv"
Policy_P1<-"dataset/OxCGRT_latest.csv"
POPDATA<-'dataset/pop_data_age_by_country.csv'
IND_DATA<-'dataset/index.csv'
ENV_DATA<-'dataset/20200120-20210325humidity&airtem_abb.csv'
school_holiday<-'dataset/day_public_and_school_holidays_2010_2019.csv'

######### merge all the datasets ##############
source('merge_dataset.R')
Dataset<-merge_dataset(COVID_D1,Policy_P1,POPDATA,IND_DATA,ENV_DATA)
###############################################
########## process data (waves & policy) #############
Dataset[is.na(Dataset)]<-0
DatasetN<-do.call(rbind,lapply(split(Dataset,Dataset$CountryName),
                 FUN=function(x){if(max(x$Vaccinations)>0){v<-x[(1:(which(x$Vaccinations>0)[1]-1)),]}else{v<-x}
                 return(v)}))
DatasetV<-do.call(rbind,lapply(split(Dataset,Dataset$CountryName),
                 FUN=function(x){
                    if(max(x$Vaccinations)>0){v<-x[(which(x$Vaccinations>0)[1]:nrow(x)),]}else{v<-NULL}
                   return(v)}))
#write.csv(DatasetN[,-c(ncol(DatasetN)-1,ncol(DatasetN))],file = "dataset/Dataset_0325NPIs.csv",row.names = F)
#write.csv(DatasetV,file = "dataset/Dataset_0325Vaccination.csv",row.names = F)
###########################################
######### regional strata #####################
source('strata_data.R')     ##################
strata<-strata_data(Dataset) #################
#write.csv(strata,file = "dataset/strata.csv",row.names = F)
##############################################
##########gather the dataset##############################
source('gather_dataset_week.R')
gather<-gather_dataset(DatasetN,strata,3)
#######################################################
##########with all waves and groups######
source('alpha calculate/alpha_calculate.R')
source('alpha calculate/alpha_all.R')
#gather<-read.csv("dataset/Gather_dataset_week_lastest0325_max3.csv",stringsAsFactors = FALSE)
gather<- gather[which(gather$Y1!=0),]
m<-stan_model('modelstan/model_Y0.stan')
#m<-stan_model('modelstan/model_Y0_10%_50%.stan')#sensitivity analysis of alpha
#m<-stan_model('modelstan/model_Y0_30%_50%.stan')#sensitivity analysis of alpha
#m<-stan_model('modelstan/model_Y0_20%_40%.stan')#sensitivity analysis of beta
#m<-stan_model('modelstan/model_Y0_20%_60%.stan')#sensitivity analysis of beta
#########all waves and groups##########
output<-paste0("result2/delay10_8/all.rds")
all2<-alpha_all(gather,m,output,"Y0_all")
#########wave result############
waveresult<-function(i,gather,m,out){
  gatherdata<-subset(gather,gather$Wave==i)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  output<-paste0(out,"/Wave",i,".rds")
  all2<-alpha_all(gatherdata,m,output,"Y0")
}
for(i in 1:3){
  waveresult(i,gather,m,"#####set your output path here##########")
}
############group result############
groupresult<-function(i,gather,m,out){
  gatherdata<-subset(gather,gather$Group==i)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  output<-paste0(out,"/G",i,".rds")
  all2<-alpha_all(gatherdata,m,output,"Y0_all")
}
lapply(seq(1,4),FUN = function(i){groupresult(i,gather,m,"#####set your output path here#####")})
######wave result within each group ##########
ALLresult<-function(i,j,gather,m,out){
  gatherdata<-subset(gather,gather$Group==i)
  gatherdata<-subset(gatherdata,gatherdata$Wave==j)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  output<-paste0(out,"/G",i,"-W",j,"all.rds")
  all2<-alpha_all(gatherdata,m,output,"Y0")
}
lapply(seq(1,4),FUN = function(i){lapply(c(1,3),FUN = function(j){
  ALLresult(i,j,gather,m,"#####set your output path here#####")})})
#########time series results within different waves and groups######
timeseriesFun<-function(gather,i,j,output){
  gatherdata<-subset(gather,gather$Group==j)
  gatherdata<-subset(gatherdata,gatherdata$Wave==i)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  alpha_calculate(gatherdata,m,output,"Y0")
}
lapply(seq(1,4),FUN = function(i){lapply(seq(1,3),FUN = function(j){timeseriesFun(gather,i,j,output) })})
###########plot figs#############
source('plot_code/picture_plot.R')
weekfitlist<-list.files("result2/weekly",pattern="*.rds$")
allfitlist<-list.files("result2/all",pattern="*.rds$")
policy_variable <- c("School closures", "Workplace closures", "Public transport closures","International travel restrictions",
                     "Facial coverings", "Movement restrictions","Gathering restrictions")
state_variable <- c("HealthIndex", "PopDensity", "Aging", "Humdity", "AirTemp")
wavefitlist<-list.files("result2/wave",pattern="*.rds$")[1:3]
fig1(wavefitlist,policy_variable,gather,inputpath="result2/wave/",outputpath="result2/result/")
fig2(gather,policy_variable, outputpath="result2/", inputpath="result2/all/")
wavedata<-figuredata_cal(policy_variable,"result2/weekly")
fig3(wavedata,"result2")
modeltranplot("result2/all/","result2/wave/")
SIfig(weekfitlist,gather,policy_variable)
###################### NPIs correlation analysis##########
Corfig<-COR(gather)
#################plot policy intensity##########
Countrylist<-c("Russia","United States","Japan","South Africa")
fig<-policyfig(Countrylist,gather)
##############Cross Validation################
source('alpha calculate/alpha_all_validation.R')
Counlist<-unique(gather$Country)
m<-stan_model('modelstan/model_Y0val.stan')
standardfun1<-function(v,valdata,testdata){(valdata[,v]-min(testdata[,v]))/(max(testdata[,v])-min(testdata[,v]))}
standardfun2<-function(v,valdata,testdata){valdata[,v]/max(testdata[,v])}
validationsample<-lapply(seq(1:60),FUN=function(V){sample(1:length(Counlist),40,replace = F)})
for (i in 1:50){
  state_variable <- c("HealthIndex", "PopDensity", "Aging", "Humdity", "AirTemp")
  ss<-validationsample[[i]]
  output<-paste0("K_Cross/test",i,".rds")
  spl<-split(gather,gather$Country)
  gatherdata<-do.call(rbind,spl[-ss])
  validationdata<-do.call(rbind,spl[ss])
  write.csv(gatherdata,file=paste0("K_Cross/testdata",i,".csv"),row.names = F)
  write.csv(validationdata,file=paste0("K_Cross/validationdata",i,".csv"),row.names = F)
  validationdata$Aging<-as.numeric(sub("%","",validationdata$Aging))
  gatherdata$Aging<-as.numeric(sub("%","",gatherdata$Aging))
  for(n in state_variable[1:3]){validationdata[,n]<-standardfun2(n,validationdata,gatherdata) }
  for(n in state_variable[4:5]){validationdata[,n]<-standardfun1(n,validationdata,gatherdata) }
  all2<-alpha_all_validation(gatherdata,m,output,validationdata,"Y0_all")
}
