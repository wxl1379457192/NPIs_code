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
library("ggalt")
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
COVID_D1<-"dataset/owid-covid-data-lastest.csv"
Policy_P1<-"dataset/OxCGRT_latest_202106.csv"
POPDATA<-'dataset/pop_data_age_by_country.csv'
IND_DATA<-'dataset/index.csv'
ENV_DATA<-'dataset/20200120-20210625humidity&airtem_abb.csv'
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
source('plot_code/picture_plot.R')
DailyCases(Dataset)
write.csv(DatasetN[,-c(ncol(DatasetN)-1,ncol(DatasetN))],file = "dataset/Dataset_0722NPIs.csv",row.names = F)
###########################################
######### regional strata #####################
source('strata_data.R')     ##################
strata<-strata_data(DatasetN) #################
write.csv(strata,file = "dataset/strata.csv",row.names = F)
##############################################
##########gather the dataset##############################
source('gather_dataset_week.R')
gather<-gather_dataset(DatasetN,strata,3)
gather<- gather[which(gather$Y1!=0),]
gather<- gather[complete.cases(gather),]
write.csv(gather,file = "dataset/Gather_dataset_week_lastest_max3.csv",row.names = F)
#######################################################
##########with all waves and groups######
source('alpha calculate/alpha_all.R')
m<-stan_model('modelstan/model_Y0.stan')
#m<-stan_model('modelstan/model_Y0_10%_50%.stan')#sensitivity analysis of alpha
#m<-stan_model('modelstan/model_Y0_30%_50%.stan')#sensitivity analysis of alpha
#m<-stan_model('modelstan/model_Y0_20%_40%.stan')#sensitivity analysis of beta
#m<-stan_model('modelstan/model_Y0_20%_60%.stan')#sensitivity analysis of beta
#########all waves and groups##########
output<-paste0("result/all.rds")
all2<-alpha_all(gather,m,output,"Y1","Y0")

#########wave result############
waveresult<-function(i,gather,m,out){
  gatherdata<-subset(gather,gather$Wave==i)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  output<-paste0(out,"/Wave",i,".rds")
  all2<-alpha_all(gatherdata,m,output,"Y1","Y0")
}
for(i in 1:3){
  waveresult(i,gather,m,"result/wave")
}
############group result############
groupresult<-function(i,gather,m,out){
  gatherdata<-subset(gather,gather$Group==i)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  output<-paste0(out,"/G",i,".rds")
  all2<-alpha_all(gatherdata,m,output,"Y1","Y0")
}
lapply(seq(1,4),FUN = function(i){groupresult(i,gather,m,"result/group")})
######wave result within each group ##########
ALLresult<-function(i,j,gather,m,out){
  gatherdata<-subset(gather,gather$Group==i)
  gatherdata<-subset(gatherdata,gatherdata$Wave==j)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  output<-paste0(out,"/G",i,"-W",j,"all.rds")
  all2<-alpha_all(gatherdata,m,output,"Y1","Y0")
}
lapply(seq(1,4),FUN = function(i){lapply(seq(1,3),FUN = function(j){
  ALLresult(i,j,gather,m,"result/all")})})

###########plot figs#############


allfitlist<-list.files("result/all",pattern="*.rds$")
policy_variable <- c("School closures", "Workplace closures", "Public transport closures","International travel restrictions",
                     "Facial coverings", "Movement restrictions","Gathering restrictions")
state_variable <- c("HealthIndex", "PopDensity", "Aging", "Humdity", "AirTemp")
wavefitlist<-list.files("result/waveresult",pattern="*.rds$")
fig2(gather,policy_variable,wavefitlist,inputpath="result/wave/")
fig3(gather,policy_variable,)

###################### NPIs correlation analysis##########
Corfig<-COR(gather)
#################plot policy intensity##########
Countrylist<-c("Russia","United States","Japan","South Africa")
fig<-policyfig(Countrylist,gather)
