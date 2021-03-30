source('gather_dataset_week.R')
strata<-read.csv(file = "dataset/strata.csv",stringsAsFactors = FALSE)
Y0cal<- function(strata) {
  Dataset<-read.csv("dataset/Dataset_0325NPIs.csv",stringsAsFactors = FALSE)
  Dataset[is.na(Dataset)]<-0
  gather<-gather_dataset(Dataset,strata,3)
  Y0list<-lapply(split(gather,gather$Country),
                 FUN=function (v){do.call(rbind,lapply(split(v,v$Wave),FUN=function(k){k[1,c("Country","Wave","Y0")]}))})
  return(do.call(rbind,Y0list))
  }
Y0<-Y0cal(strata)
write.csv(Y0,file = "dataset/Y0_N.csv",row.names = F)
######### merge all the datasets ##############
Dataset<-read.csv("dataset/Dataset_0325Vaccination.csv",stringsAsFactors = FALSE)##########合并后的初始数据集
strata<-read.csv("dataset/strata.csv",stringsAsFactors = FALSE)########国家分区
Y0<-read.csv("dataset/Y0_N.csv",stringsAsFactors = FALSE)##########每个国家的Y0(初始新增比例)数据
colnames(Y0)<-c("Country","Wave","Y0")
#############时间序列+注射人数占总人口比例#############
standardFun<-function(D){
  (D-min(D))/(max(D)-min(D))
}
gather_dataset1 <-function(Dataset,strata){
  Dataset<-do.call(rbind,lapply(split(Dataset,Dataset$CountryName),FUN=function(v){
     #if(length(which(v$New_cases_smoothed<0.5*max(v$New_cases_smoothed)))<nrow(v)*0.5){x<-NULL
    if(max(v$New_cases_smoothed)<100){x<-NULL
     }else{x<-v}
  return(x)}))
  Coun<-unique(Dataset$CountryName)
  g <- list()
  for (w in 1:length(Coun)) {
    d <-subset(Dataset,CountryName==Coun[w])
    s<-subset(strata,CountryName==Coun[w])
    y0<-subset(Y0,Country==Coun[w])
    if (min(nrow(y0),nrow(s))>0){
      d$Y1 <- 0
      d[is.na(d)]<-0
      d$Group<-s$Group
      d<-do.call(rbind,lapply(split(d,d$wave),
                FUN=function(v){
                  v$Y0<-y0$Y0[nrow(y0)]
                  return(v)
                }))
      d$Restrict.Movement<-0
      d$Restrict.Gathering<-0
      for (j in 2:nrow(d)) {
        if(d$Vaccinations[j]==0){d$Vaccinations[j]<-d$Vaccinations[j-1]}
        d$Y1[j] <- ifelse(is.na(d$New_cases_smoothed[j - 1][1]), 0,
                                    (d$New_cases_smoothed[j] / d$New_cases_smoothed[j - 1])) }
      d<-d[-1,]
      d$Y1[which(is.infinite(d$Y1))] <- 0
      d$Y1[which(is.nan(d$Y1))] <- 0
      d$Aging<-as.numeric(sub("%","",d$Aging))
      d$Day<-seq(1:nrow(d))
      if(nrow(d)>12){g[[w]]<-d}
      }
  }
  CBD <- do.call(rbind, g)
  for(c in c(10:21)){CBD[,c]<-CBD[,c]/max(CBD[,c]) }
  for(c in c(22,23)){ CBD[,c]<-standardFun(CBD[,c])}
  colnames(CBD)<-c("Date", "CountryCode","Continent","New_cases","New_deaths",
                   "Wave","New_cases_smoothed","Pop","Country","School.closing",
                   "Workplace.closing","Cancel.Public.events","Restrictions.on.gatherings",
                   "Close.Public.transport","Stay.at.home.requirements",
                   "Restrictions.on.internalmovement","International.travel.controls",
                   "Facial.Coverings","HealthIndex","PopDensity","Aging",
                   "Humdity","AirTemp", "Holiday","Vaccination","Vaccination_perHur",
                   "Y1","Group","Y0","Restrict.Movement","Restrict.Gathering","Day")
  CBD$Restrict.Movement<-(CBD$Stay.at.home.requirements+CBD$Restrictions.on.internalmovement)/2
  CBD$Restrict.Gathering<-(CBD$Restrictions.on.gatherings+CBD$Cancel.Public.events)/2
  CBD$VaccinationPre<- CBD$Vaccination/CBD$Pop
  return(CBD)
}
VaccDiverge<-function(gather1,deletetime){
  Count<-unique(gather1$Country)
  Vacc<-gather1[,c("Date","Country","Vaccination","Vaccination_perHur","VaccinationPre")]
  gather2<-gather1[,-c(25,26,33)]
  gather2$Date<-as.Date(gather2$Date)
  Vacc$Date<-as.Date(Vacc$Date)+deletetime
  g<-list()
  for(c in 1:length(Count)){
    g1<-subset(gather2,gather2$Country==Count[c])
    v<-subset(Vacc,Vacc$Country==Count[c])[,-2]
    g[[c]]<-merge(g1,v,by="Date")}
  return(do.call(rbind,g))
}#########考虑抗体生成的延迟作用
alphacal_NPI<-function(data,m,outputpath,policy_variable){
  rstan_options(auto_write = TRUE)
  state_variable <- c("HealthIndex", "PopDensity", "Aging", "Humdity", "AirTemp")
  variables <- c(policy_variable,state_variable)
  interventions<-data[,variables]
  Y<-as.vector(data$Y1)
  Y0<-as.vector(data$Y0)
  X1<-as.matrix(interventions[,policy_variable])
  X2<-as.matrix(interventions[,state_variable])
  dataset<- list(m=length(Y),k1=ncol(X1),k2=ncol(X2),X1=X1,X2=X2,Y=Y,Y0=Y0)
  fit<-rstan::sampling(object = m,data=dataset,iter=1000,warmup=100,
                       chains=5,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 12),seed = 1111)
  saveRDS(fit, file = outputpath)
  return(fit)
}####政策效果分析
plotfun<-function(fitpath,data,outputname,NPI){
  figpath<-paste0("Vaccianc/",outputname,".jpg")
  fit<-readRDS(fitpath)
  X<-colMeans(data[,c("School.closing", "Workplace.closing","Close.Public.transport",
                      "International.travel.controls","Facial.Coverings","Restrict.Movement","Restrict.Gathering")])
  if(length(NPI)>7){
    CCN<-unique(data$Country)
    maxD<-lapply(seq(1:length(CCN)),FUN = function(v){
      V1<-subset(data,data$Country==CCN[v])
      max(V1$VaccinationPre)})
    V<-colMeans(do.call(rbind,maxD))
    X<-rbind(as.data.frame(X),V)
    X<-as.numeric(X$X)
  }
  out<-rstan::extract(fit)
  alpha <- as.matrix(out$alpha)
  alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){ alpha[,k]*X[k]})
  alpha<-do.call(cbind,alphalist)
  alpha<-(1-exp(-alpha))*100
  Name<-c("School closures", "Workplace closures", "Public transport closures",
                     "International travel restrictions", "Facial coverings",
                     "Movement restrictions","Gathering restrictions","Vaccination")
  colnames(alpha)<-Name
  data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
  write.csv(data, file = paste0("Vaccianc/",outputname,".csv"),row.names = F)
  data$parameter<-factor(Name,levels = Name ,ordered = T)
  fig<-ggplot()+
    geom_segment(mapping = aes_(x = ~ ll, xend = ~ hh, y = ~ parameter, yend = ~ parameter),
                 data=data, size = 1, alpha=0.7,color="#b7dfe2")+
    geom_segment(mapping = aes_(x = ~ l, xend = ~ h, y = ~ parameter, yend = ~ parameter),
                   data=data,size =1.5,color="#448f94",show.legend = F)+ xlim(-1,50)+
    geom_point(mapping = aes_(x = ~ m, y = ~ parameter),color="#234244",
               data = data, size = 3.5,shape = 20)+
    labs(x = paste0("\u0394","\u03C9","t","(%)"),y = NULL,title = outputname)+theme_bw()+
    theme(plot.title =  element_text(color="black",hjust=0.5,size = 10,family="Times New Roman"),
            axis.text = element_text(color="black",hjust=0.5,size = 10,family="Times New Roman"),
            legend.position = "none",
            panel.grid=element_blank(),
            panel.background=element_rect(fill = "transparent",colour = "black"),
            plot.margin=unit(c(0.05,0.10,0.05,0.05),"cm"))
  ggsave(fig,file=figpath,height=50,width=80,units="mm")
  return(data)
}
plotmaxfun<-function(fitpath,data,outputname,NPI){
  figpath<-paste0("Vaccianc/",outputname,".jpg")
  fit<-readRDS(fitpath)
  X<-colMeans(data[,c("School.closing", "Workplace.closing","Close.Public.transport",
                      "International.travel.controls","Facial.Coverings","Restrict.Movement","Restrict.Gathering")])
  if(length(NPI)>7){
    CCN<-unique(data$Country)
    maxD<-lapply(seq(1:length(CCN)),FUN = function(v){
      V1<-subset(data,data$Country==CCN[v])
      max(V1$VaccinationPre)})
    V<-colMeans(do.call(rbind,maxD))
    X<-rbind(as.data.frame(X),V)
    X<-as.numeric(X$X)
  }
  out<-rstan::extract(fit)
  alpha <- as.matrix(out$alpha)
  #alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){ alpha[,k]*X[k]})
  #alpha<-do.call(cbind,alphalist)
  alpha<-(1-exp(-alpha))*100
  Name<-c("School closures", "Workplace closures", "Public transport closures",
                     "International travel restrictions", "Facial coverings",
                     "Movement restrictions","Gathering restrictions","Vaccination")
  colnames(alpha)<-Name
  data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
  write.csv(data, file = paste0("Vaccianc/",outputname,".csv"),row.names = F)
  data$parameter<-factor(Name,levels = Name ,ordered = T)
  fig<-ggplot()+
    geom_segment(mapping = aes_(x = ~ ll, xend = ~ hh, y = ~ parameter, yend = ~ parameter),
                 data=data, size = 1, alpha=0.7,color="#b7dfe2")+
    geom_segment(mapping = aes_(x = ~ l, xend = ~ h, y = ~ parameter, yend = ~ parameter),
                   data=data,size =1.5,color="#448f94",show.legend = F)+ xlim(-1,50)+
    geom_point(mapping = aes_(x = ~ m, y = ~ parameter),color="#234244",
               data = data, size = 3.5,shape = 20)+
    labs(x = paste0("\u0394","\u03C9","t","(%)"),y = NULL,title = outputname)+theme_bw()+
    theme(plot.title =  element_text(color="black",hjust=0.5,size = 10,family="Times New Roman"),
            axis.text = element_text(color="black",hjust=0.5,size = 10,family="Times New Roman"),
            legend.position = "none",
            panel.grid=element_blank(),
            panel.background=element_rect(fill = "transparent",colour = "black"),
            plot.margin=unit(c(0.05,0.10,0.05,0.05),"cm"))
  ggsave(fig,file=figpath,height=50,width=80,units="mm")
  return(data)
}
##########################
gather1<-gather_dataset1(Dataset, strata)#####以各国疫苗注射起始为第一周对齐
gather1<- gather1[which(gather1$Y1!=0),]
write.csv(gather1,file = "Vaccianc/gatherdata_Vanalysis_Y0N.csv",row.names = F)
###########################
gather1<-read.csv("Vaccianc/gatherdata_Vanalysis_Y0N.csv",stringsAsFactors = FALSE)
#LIST<-lapply(split(S1,S1$Country),FUN=function(v){v[1,c("Country","Group")]})
#C<-do.call(rbind,LIST)
#write.csv(C,file = "Vaccianc/Country_vaccination.csv",row.names = F)
#S2<-VaccDiverge(gather1,28)###第二种情境，抗体生成28天
S1<-VaccDiverge(gather1,12)###第一种情境，抗体生成12天
S1_before<-subset(S1,S1$Day<=27)#########分割时间节点15天
S2_before<-subset(S1,S1$Day<=42)#########分割时间节点30天
S2_after<-subset(S1,S1$Day>42)#########分割时间节点30天
m<-stan_model('modelstan/model_Y0.stan')
NPI_8<-c("School.closing", "Workplace.closing", "Close.Public.transport", "International.travel.controls",
         "Facial.Coverings","Restrict.Movement","Restrict.Gathering","VaccinationPre")
##########分别投入情境1.2数据，做起效后NPI效果分析##########
S1_NPI8<-alphacal_NPI(S1,m,"Vaccianc/test1/S1_NPI8.rds",NPI_8)
##########分别投入情境1.2数据，做起效后累计效应分析##########
S1_NPI8_before<-alphacal_NPI(S1_before,m,"Vaccianc/test1/S1_NPI8_before15.rds",NPI_8)
S2_NPI8_before<-alphacal_NPI(S2_before,m,"Vaccianc/test1/S2_NPI8_before30.rds",NPI_8)
###出图##########
S1_NPI8fig<-plotfun("Vaccianc/test1/S1_NPI8.rds",S1,"S1_NPI8_overall_withmean",NPI_8)
S1_NPI8figmax<-plotmaxfun("Vaccianc/test1/S1_NPI8.rds",S1,"S1_NPI8_overall_withmax",NPI_8)
S1_NPI8_beforefig<-plotfun("Vaccianc/test1/S1_NPI8_before15.rds",S1_before, "S1_NPI8_before_15days_withmean",NPI_8)
S1_NPI8_beforefigmax<-plotmaxfun("Vaccianc/test1/S1_NPI8_before15.rds",S1_before, "S1_NPI8_before_15days_withmax",NPI_8)
S2_NPI8_beforefig<-plotfun("Vaccianc/test1/S2_NPI8_before30.rds",S2_before, "S2_NPI8_before_30days_withmean",NPI_8)
S2_NPI8_beforefigmax<-plotmaxfun("Vaccianc/test1/S2_NPI8_before30.rds",S2_before, "S2_NPI8_before_30days_withmax",NPI_8)


S1_NPI8fig<-plotfun("Vaccianc/S1_NPI8.rds",S1,"S1_NPI8_overall_withmean",NPI_8)
S1_NPI8_beforefig<-plotfun("Vaccianc/S1_NPI8_before15.rds",S1_before, "S1_NPI8_before_15days_withmean",NPI_8)
S2_NPI8_beforefig<-plotfun("Vaccianc/S2_NPI8_before30.rds",S2_before, "S2_NPI8_before_30days_withmean",NPI_8)

figlist<-list(S2_NPI8_beforefig,S1_NPI8_beforefig,S1_NPI8fig)
#S2_NPI7fig<-plotfun("Vaccianc/test4/S2_NPI7.rds","S2_NPI7",NPI_7)
#S2_NPI8fig<-plotfun("Vaccianc/test4/S2_NPI8.rds","S2_NPI8",NPI_8)

#S2_NPI8_beforefig<-plotfun("Vaccianc/test4/S2_NPI8_before.rds","S2_NPI8_before","Vaccianc/test4/S2_NPI8_before.jpg",NPI_8)
#S2_NPI8_afterfig<-plotfun("Vaccianc/test4/S2_NPI8_after.rds","S2_NPI8_after","Vaccianc/test4/S2_NPI8_after.jpg",NPI_8)
for(n in 1:length(figlist)){
  figlist[[n]]$G<-n
  figlist[[n]]$parameter<-factor(figlist[[n]]$parameter,c("School closures", "Workplace closures", "Public transport closures",
                            "International travel restrictions", "Facial coverings",
                            "Movement restrictions","Gathering restrictions","Vaccination"),ordered=T)
}
figname<-c("Within 15 days after protection onset","Within 30 days after protection onset","Until 25 March 2021")
data<-do.call(rbind,figlist)
data$G<-factor(data$G,levels = c(3,2,1) ,ordered = T)
fig1<-ggplot()+
  geom_linerange(mapping = aes_(xmin =~ll, xmax=~hh, y=~parameter,color= ~G),show.legend = F,alpha=0.2,
                 data=data, size = 1, position=position_dodge(width = 0.8))+
  geom_hline(show.legend = T,aes(yintercept = -1, color=G),data=data,alpha=0.2,size = 1)+
  geom_linerange(mapping = aes_(xmin =~l, xmax=~h, y=~parameter,color=~G),show.legend = F,
                 data=data,size =1.8,position=position_dodge(width = 0.8))+
  geom_point(mapping = aes_(x = ~m, y = ~parameter, color = ~G),show.legend = T,alpha=0.5,
             data = data, size = 2.5,position= position_dodge(width = 0.8))+
  labs(x = paste0("\u0394","\u03C9","t","(%)"),y = NULL,title =NULL)+theme_bw()+
  scale_x_continuous(expand=c(0,0),limits=range(-1,75))+
  scale_color_manual(name=NULL,limits=c("1","2","3"),
                     values =c("#5e5f7f","#389f8f","#d0bc75"),
                     labels = c('1'="Within 15 days after protection onset",
                                '2'="Within 30 days after protection onset",
                                '3'="Until 25 March 2021"))+
  theme(legend.position = "top",
        legend.justification = c(0,0),
        legend.text = element_text(size = 10,family="Times New Roman",color="black"),
        plot.title = element_text(color="black",hjust = 0,vjust=0, size = 10,family="Times New Roman"),
        axis.ticks.y= element_line(color="black"),
        axis.text.y = element_text(color="black",vjust=0.5,hjust=1,size = 10,family="Times New Roman"),
        panel.grid=element_blank(),
        panel.background=element_rect(fill = "transparent",colour = NA),
        plot.margin=unit(c(0.01,0.2,0.01,0.01),"cm"),
        axis.line = element_line(color="black",size=0.05),
        axis.title = element_text(color="black",size = 10,family="Times New Roman"),
        axis.text.x = element_text(color="black",size = 10,family="Times New Roman"))
ggsave("picture/figVall-new.pdf",fig1,height=100,width=160,unit="mm",device = cairo_pdf)

#######################
IsData<-subset(S1,S1$Country=="Israel")
IsData_before30<-subset(IsData,IsData$Day<=42)
IsData_before15<-subset(IsData,IsData$Day<=27)
alphacal_NPI(IsData,m,"Vaccianc/Israel_NPI8.rds",NPI_8)

S1_NPI8_before<-alphacal_NPI(IsData_before15,m,"Vaccianc/Israel_NPI8_before15.rds",NPI_8)
S1_NPI8_before2<-alphacal_NPI(IsData_before30,m,"Vaccianc/Israel_NPI8_before30.rds",NPI_8)
plotfun("Vaccianc/Israel_NPI8.rds",IsData,"Israel_NPI8",NPI_8)
plotfun("Vaccianc/Israel_NPI8_before15.rds",IsData_before15, "Israel_NPI8_before15",NPI_8)
plotfun("Vaccianc/Israel_NPI8_before30.rds",IsData_before30,"Israel_NPI8_before30",NPI_8)
plotmaxfun("Vaccianc/Israel_NPI8.rds",IsData,"Israel_NPI8_max",NPI_8)
plotmaxfun("Vaccianc/Israel_NPI8_before15.rds",IsData_before15, "Israel_NPI8_before15_max",NPI_8)
plotmaxfun("Vaccianc/Israel_NPI8_before30.rds",IsData_before30,"Israel_NPI8_before30_max",NPI_8)