setwd("E:/code_lastest")
source('US_dataset_calculate.R')

COVID_D1<-"dataset/US_dataset/US_covid-data.csv"
Policy_P1<-"dataset/US_dataset/US_OxCGRT_latest_202106.csv"
POPDATA<-'dataset/US_dataset/pop_data_by_states.csv'
IND_DATA<-'dataset/US_dataset/US_index.csv'
ENV_DATA<-'dataset/US_dataset/US_20200120-20210513humidity&airtem.csv'
Dataset<-merge_dataset(COVID_D1,Policy_P1, POPDATA,IND_DATA,ENV_DATA)
Dataset[is.na(Dataset)]<-0
write.csv(Dataset,file = "dataset/US_dataset/US_Dataset_newStatewave_0718.csv",row.names = F)

gather<-gather_dataset(Dataset,3)
gather<- gather[which(gather$Y1!=0),]
gather <- gather[complete.cases(gather),]
starta<-read.csv('dataset/US_dataset/US_starta.csv',stringsAsFactors = F)
gather<-do.call(rbind,lapply(split(gather,gather$StateID),FUN=function(v){
  G<-subset(starta,starta$state==unique(v$StateID))
  if(nrow(G>0)){
    v$Group<-G$Group
  }
  return(v)
}))
write.csv(gather,file = "dataset/US_dataset/US_Gather_dataset_week_max3.csv",row.names = F)

library("curl")
library("magrittr")
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
library("ggplot2")
library("cowplot")
library("ggrepel")
library("rstanarm")
source('alpha calculate/alpha_all_US.R')
#m1<-stan_model('modelstan/model_Y0.stan')
#m1<-stan_model('modelstan/model_Y0_US_STATE_withoutENV.stan')
m1<-stan_model('modelstan/model_Y0_US_STATE.stan')
#m1<-stan_model('modelstan/model_Y0_US.stan')
Getdata<-function(gather,outputpath){
  policy_variable <- c("School closures",
                       "Workplace closures",
                       "Public transport closures",
                       "Stay at home requirements",
                       "Internalmovement restrictions",
                       "Facial coverings", 
                       "Gathering restrictions")
  wavelist<-lapply(split(gather,gather$Wave),FUN = function(g){
    output<-paste0(outputpath,"/Wave",unique(g$Wave),".rds")
    all2<-alpha_all_US(g,m1,output,"Y1","Y0")
    out<-rstan::extract(all2)
    alpha<-as.matrix(out$alpha)
    pp<-g[,c("School.closing", "Workplace.closing", 
             "Close.Public.transport","Stay.at.home.requirements",
             "Restrictions.on.internalmovement",
             "Facial.Coverings","Restrict.Gathering")]
    X<-apply(pp,2,FUN=function(x){mean(x)})
    #  mean(x[which(x>0)])})
    alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
    alpha<-do.call(cbind,alphalist)
    allNPI<-apply(alpha,1,sum)
    list<-list(alpha,allNPI)
    alpha<-do.call(cbind,list)
    alpha<-(1-exp(-alpha))*100
    colnames(alpha)<-c(policy_variable,"All NPIs")
    data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
    #write.csv(data,paste0(outputpath,"/W",unique(g$Wave),".csv"),row.names = F)
    data$wave<-unique(g$Wave)
    return(data)
  })
  wave<-do.call(rbind,wavelist)
  all2<-alpha_all_US(gather,m1,paste0(outputpath,"/all.rds"),"Y1","Y0")
  out<-rstan::extract(all2)
  alpha<-as.matrix(out$alpha)
  pp<-g[,c("School.closing", "Workplace.closing", 
           "Close.Public.transport","Stay.at.home.requirements",
           "Restrictions.on.internalmovement",
           "Facial.Coverings","Restrict.Gathering")]
  X<-apply(pp,2,FUN=function(x){mean(x)})
  #mean(x[which(x>0)])})
  alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
  alpha<-do.call(cbind,alphalist)
  allNPI<-apply(alpha,1,sum)
  list<-list(alpha,allNPI)
  alpha<-do.call(cbind,list)
  alpha<-(1-exp(-alpha))*100
  colnames(alpha)<-c(policy_variable,"All NPIs")
  data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
  data$wave<-"all"
  data<-do.call(rbind,list(wave,data))
  write.csv(data,paste0(outputpath,"/all.csv"),row.names = F)
}
############plot cor#
gather<-g
outputpath<-"US_result"

if (dir.exists(outputpath)==F){dir.create(outputpath)}else{print("This path has been exists")}
output<-paste0(outputpath,"/all.rds")
all2<-alpha_all_US_withimm(gather,m1,output,"Y1","Y0")
waveresult<-function(i,gather,m,out){
  gatherdata<-subset(gather,gather$Wave==i)
  gatherdata <- gatherdata[which(gatherdata$Y0!=0),]
  output<-paste0(out,"/Wave",i,".rds")
  all2<-alpha_all_US_withimm(gatherdata,m,output,"Y1","Y0")
}
for(i in 1:3){
  waveresult(i,gather,m1,outputpath)
}
Getdata(gather,outputpath)
##########Group wave result###############
m1<-stan_model('modelstan/model_Y0_US_STATE_group.stan')
policy_variable <- c("School closures","Workplace closures",
                     "Public transport closures","Stay at home requirements",
                     "Internalmovement restrictions","Facial coverings", 
                     "Gathering restrictions")

outputpath<-"US_result/group"
if (dir.exists(outputpath)==F){dir.create(outputpath)}else{print("This path has been exists")}
groupwavelist<-lapply(split(gather,gather$Group),FUN = function(g){
  wavelist<-lapply(split(g,g$Wave),FUN = function(g1){
    output<-paste0(outputpath,"/G",unique(g1$Group),"_W",unique(g1$Wave),".rds")
    all2<-alpha_all_US_withimm(g1,m1,output,"Y1","Y0")
    #all2<-readRDS(output)
    out<-rstan::extract(all2)
    alpha<-as.matrix(out$alpha)
    X<-colMeans(g1[,c("School.closing", "Workplace.closing", 
                      "Close.Public.transport","Stay.at.home.requirements",
                      "Restrictions.on.internalmovement",
                      "Facial.Coverings","Restrict.Gathering")])
    alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
    alpha<-do.call(cbind,alphalist)
    allNPI<-apply(alpha,1,sum)
    list<-list(alpha,allNPI)
    alpha<-do.call(cbind,list)
    alpha<-(1-exp(-alpha))*100
    colnames(alpha)<-c(policy_variable,"All NPIs")
    data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
    write.csv(data,paste0(outputpath,"/G",unique(g1$Group),"_W",unique(g1$Wave),".csv"),row.names = F)
    data$Group<-unique(g1$Group)
    data$wave<-unique(g1$Wave)
    return(data)})
  return(do.call(rbind,wavelist))
})
group<-do.call(rbind,groupwavelist)
write.csv(group,paste0(outputpath,"/groupWaveresult.csv"),row.names = F)
###########Group result################
m1<-stan_model('modelstan/model_Y0_US_STATE_group.stan')
policy_variable <- c("School closures","Workplace closures",
                     "Public transport closures","Stay at home requirements",
                     "Internalmovement restrictions","Facial coverings", 
                     "Gathering restrictions")
outputpath<-"US_result/group"
if (dir.exists(outputpath)==F){dir.create(outputpath)}else{print("This path has been exists")}
grouplist<-lapply(split(gather,gather$Group),FUN = function(g1){
  output<-paste0(outputpath,"/G",unique(g1$Group),"_all.rds")
  all2<-alpha_all_US_withimm(g1,m1,output,"Y1","Y0")
  #all2<-readRDS(output)
  out<-rstan::extract(all2)
  alpha<-as.matrix(out$alpha)
  X<-colMeans(g1[,c("School.closing", "Workplace.closing", 
                    "Close.Public.transport","Stay.at.home.requirements",
                    "Restrictions.on.internalmovement",
                    "Facial.Coverings","Restrict.Gathering")])
  alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
  alpha<-do.call(cbind,alphalist)
  allNPI<-apply(alpha,1,sum)
  list<-list(alpha,allNPI)
  alpha<-do.call(cbind,list)
  alpha<-(1-exp(-alpha))*100
  colnames(alpha)<-c(policy_variable,"All NPIs")
  data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
  data$Group<-unique(g1$Group)
  return(data)
})
group<-do.call(rbind,grouplist)
write.csv(group,paste0(outputpath,"/Groupall.csv"),row.names = F)


data<-read.csv("US_result/groupWaveresult.csv",stringsAsFactors = FALSE)
data1<-subset(data,data$wave!="all")
data$wave<-as.factor(data$wave)
data$Group<-as.factor(data$Group)
fig1<-ggplot(data)+
  geom_linerange(mapping = aes_(xmin =~ll, xmax=~hh, y=~parameter,color= ~Group),show.legend = F,alpha=0.4,
                 size = 1, position=position_dodge(width = 0.7))+
  geom_hline(show.legend = T,aes(yintercept = -1, color=Group),size = 1,alpha=0.4)+
  geom_linerange(mapping = aes_(xmin =~l, xmax=~h, y=~parameter,color=~Group),show.legend = F,
                 size =1.8,position=position_dodge(width = 0.7))+
  geom_point(mapping = aes_(x = ~m, y = ~parameter, color = ~Group),show.legend = T,alpha=0.7,
             size =2.5,position= position_dodge(width = 0.7))+
  labs(x = paste0("\u0394","\u03C9","t","(%)"),y = NULL,title =NULL)+theme_bw()+
  scale_x_continuous(expand=c(0,0),limits=range(c(0:100)))+facet_wrap(vars(wave))+
  scale_color_manual(name=NULL,values =c("#4798b3","#8d5b51","#697ea8","#b6a681"))+
  theme(legend.position = "top",
        legend.title=element_blank(),
        legend.key.height=unit(0.2,'cm'),
        legend.key.width=unit(1.5,'cm'),
        legend.text = element_text(size = 9,family="Times New Roman"),
        legend.background = element_rect(fill="transparent",color=NA),
        legend.key=element_rect(fill=NA),
        plot.title = element_text(color="black",hjust = 0,vjust=0, size=9,family="Times New Roman"),
        axis.ticks.y= element_line(color="black"),
        axis.text.y = element_text(color="black",vjust=0.5,hjust=1, size=9,family="Times New Roman"),
        panel.grid=element_blank(),
        plot.margin=unit(c(0.01,0.2,0.01,0.2),"cm"),
        panel.background=element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(size=0.05),
        axis.title = element_text(color="black", size=9,family="Times New Roman"),
        axis.text.x = element_text(color="black", size=9,family="Times New Roman"))
ggsave(fig1, filename = "E:/code_lastest/picture/uspig_V1.pdf",width=240,heigh=120,unit="mm",
       bg = "transparent",device = cairo_pdf)



NPImean<-do.call(rbind,lapply(split(gather,gather$Group),FUN = function(g){
  meanlist<-lapply(split(g,g$Wave),FUN = function(g1){
    X<-as.data.frame(t(colMeans(g1[,c("School.closing", "Workplace.closing", 
                      "Close.Public.transport","Stay.at.home.requirements",
                      "Restrictions.on.internalmovement",
                      "Facial.Coverings","Restrict.Gathering")])))
    colnames(X)<-policy_variable
    X$Group<-unique(g1$Group)
    X$wave<-unique(g1$Wave)
    return(X)})
  return(do.call(rbind,meanlist))
}))
M1<-do.call(rbind,lapply(split(g,g$Wave),FUN = function(g1){
    X<-as.data.frame(t(colMeans(g1[,c("School.closing", "Workplace.closing", 
                                      "Close.Public.transport","Stay.at.home.requirements",
                                      "Restrictions.on.internalmovement",
                                      "Facial.Coverings","Restrict.Gathering")])))
    colnames(X)<-policy_variable
    X$wave<-unique(g1$Wave)
    return(X)}))
M1$Group<-"all"

mean<-do.call(rbind,list(NPImean,M1))
mean<-melt(mean,c("Group","wave"))

colnames(mean)<-c("Group","wave","parameter","NPI")
d2<-subset(data,data$parameter!="All NPIs")
data2<-merge(mean, d2, by = c("Group","wave","parameter"))
usp<-ggplot(data2)+geom_point(aes(x=NPI,y=m,shape=Group,color=parameter))+facet_wrap(vars(wave))+
  labs(x = paste0("NPIs strength"),y = paste0("\u0394","\u03C9","t","(%)"),title =NULL)+theme_bw()+
  scale_x_continuous(expand=c(0,0.05),breaks=seq(0,1,0.2))+facet_wrap(vars(wave))+
  scale_color_manual(name=NULL,values =c("#5f3200","#f68037","#fcde36",
                                         "#5e86c1","#64eaf2","#659702","#c5d6fe"))+
  scale_shape_manual(values =c(1,15,4,6))+
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size = 9,family="Times New Roman"),
        legend.background = element_rect(fill="transparent",color=NA),
        legend.key=element_rect(fill=NA),
        plot.title = element_text(color="black",hjust = 0,vjust=0, size=9,family="Times New Roman"),
        axis.ticks.y= element_line(color="black"),
        axis.text.y = element_text(color="black",vjust=0.5,hjust=1, size=9,family="Times New Roman"),
        panel.grid=element_blank(),
        plot.margin=unit(c(0.01,0.2,0.01,0.2),"cm"),
        panel.background=element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(size=0.05),
        axis.title = element_text(color="black", size=9,family="Times New Roman"),
        axis.text.x = element_text(color="black", size=9,family="Times New Roman"))
ggsave(usp, filename = "picture/uspig.pdf",width=240,heigh=75,unit="mm",
       bg = "transparent",device = cairo_pdf)