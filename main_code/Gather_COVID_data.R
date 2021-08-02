library(curl)
library(ggplot2)
library(magrittr)
library(geodetector)
library(rstan)
library(tidyr)
library(lubridate)
library(stringr)
library(dplyr)
library(EnvStats)
library(optparse)
library(sqldf)
library(gridExtra)
library(bayesplot)
library(ggplot2)
library(ggsci)
library(reshape2)
setwd("E://code_lastest")
#COVID_D1<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"#疫情数据
#Policy_P1<-"https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"#政策数据
COVID_D1<-"I:/COVID19/test/数据合并/owid-covid-data.csv"
Policy_P1<-"I:/COVID19/test/数据合并/政策.csv"
POPDATA<-'I:/COVID19/test/数据合并/pop_data_age_by_country.csv'#人口数据
IND_DATA<-'I:/COVID19/test/数据合并/index.csv'#其他指标因子
ENV_DATA<-'I:/COVID19/test/数据合并/20200120-1117humidity&airtem_abb.csv'#环境指标
merge_dataset<-function(COVID_D1,Policy_P1,POPDATA,IND_DATA,ENV_DATA){
  #D1<-read.csv(url(COVID_D1))#需要翻墙
  #P1<-read.csv(url(Policy_P1))
  D1<-read.csv(COVID_D1,stringsAsFactors = FALSE)
  P1<-read.csv(Policy_P1,stringsAsFactors = FALSE)
  print(paste0("Length of COVID_D1 is ",nrow(D1)," and Length of Policy_P1 is ",nrow(P1)))
  P1$RegionName<-as.character(P1$RegionName)
  P1<-subset(P1,P1$RegionName=="")
  D1<-D1[,c(1,2,3,4,6,9)]
  P1<-P1[,c(1,2,6,7,9,11,13,15,17,19,21,33)]
  D1$date<-as.Date(D1$date)
  colnames(D1)<-c("CountryCode","Continent","CountryName","Date","New_cases","New_deaths")
  D1$Date<-D1$Date-11
  P1$Date<-as.character(P1$Date)
  Date<-paste0(substring(P1$Date,1,4),"-",substring(P1$Date,5,6),"-",substring(P1$Date,7,8))
  P1$Date<-as.Date(Date)
  POP<-read.csv(POPDATA,stringsAsFactors = FALSE)#人口数据
  #POP<-subset(POP,POP$T_pop_2020>=1000000)#筛选人口大于一百万的国家
  Con_list<-POP$iso3c
  Index<-read.csv(IND_DATA,stringsAsFactors = FALSE)#index文件
  Index<-Index[,-c(2,3,4)]
  colnames(Index)<-c("CountryCode","HealthIndex","PopDensity","Aging")
  Hum<-read.csv(ENV_DATA,stringsAsFactors = FALSE)#温湿度数据
  Hum$Day<-as.Date(Hum$Day)
  colnames(Hum)<-c("CountryCode","Date","Humidity","AirTemp")
  #C<-read.csv('I:/COVID19/test/数据合并/分层.csv',stringsAsFactors = FALSE)
  M1<-list()
  for (i in 1:length(Con_list)){
    d<-subset(D1,D1$CountryCode==Con_list[i])[,-c(1,3)]
    p<-subset(P1,P1$CountryCode==Con_list[i])
    ind<-subset(Index,Index$CountryCode==Con_list[i])
    h<-subset(Hum,Hum$CountryCode==Con_list[i])[,-1]
    len<-c(nrow(d),nrow(p),nrow(ind),nrow(h))
    if (min(len)>0){
      d$POP<-POP$T_pop_2020[which(POP$iso3c==Con_list[i])]
      m<-merge(d,p,by="Date")
      m<-merge(m,ind,by="CountryCode")
      m<-merge(m,h,by="Date")
      m$New_cases[which(is.na(m$New_cases))]<-0
      m$New_cases.lo<-lowess(m$New_cases,f=0.1)[[2]]
      C<-m[,c("Date","New_cases","New_cases.lo")]
      longmean<-melt(as.data.frame(C),id="Date")
      plot<-ggplot(data=longmean,aes(x=Date,y=value,color=variable))+geom_line()+xlab("Date")+
        labs(title=m$CountryName[1])+ylab("New_cases")+ theme(legend.key.size=unit(1,"cm"))
      name<-m$CountryName[1]
      ggsave(sprintf("I:/COVID19/test/picture/loess_day0.1/%s-loess.png",name),plot,width=24,height=15,units="cm")
      M1[[i]]<-m
    }
    else{print(paste0("Country ",Con_list[i]," have some data missing"))}
  }
  Mergedata<-do.call(rbind,M1)
  Mergedata$New_deaths[which(is.na(Mergedata$New_deaths))]<-0
  return(Mergedata)
}
strata_data<-function (Dataset){
  countries <- unique(Dataset[,c("Continent","CountryName","POP")])
  total_summary <- lapply(countries$CountryName, function(cntry) {
  indx <- Dataset$CountryName == cntry
  data.frame(CountryName=cntry, total_cases=sum(Dataset$New_cases[indx], na.rm=T), total_death=sum(Dataset$New_deaths[indx], na.rm=T))})
  countries <- merge(countries, do.call(rbind,total_summary))
  countries$rate_cases <- countries$total_cases/countries$POP * 100000
  countries$rate_death <- countries$total_death/countries$POP * 100000
  q_rate_case <- lapply(seq(1000, 3000, by=100), function(v) {
  d <- data.frame(r=countries$rate_cases, strat=countries$rate_cases > v)
  c(v, geodetector::factor_detector("r","strat",d)[[1]][1,1])
  }) %>% do.call(rbind,.)
  q_rate_death <- lapply(seq(20, 100, by=5), function(v) {
  d <- data.frame(r=countries$rate_death, strat=countries$rate_death > v)
  c(v, geodetector::factor_detector("r","strat",d)[[1]][1,1])
  }) %>% do.call(rbind,.)
  q_rate_case<-as.data.frame(q_rate_case)
  q_rate_death<-as.data.frame(q_rate_death)
  q1<-q_rate_case$V1[which(q_rate_case$V2==max(q_rate_case$V2))]
  q2<-q_rate_death$V1[which(q_rate_death$V2==max(q_rate_death$V2))]
  countries$class <- "1"
  countries$class[countries$rate_cases > max(q1) & countries$rate_death > max(q2)] <- "2"
  countries$class[countries$Continent == "Asia"] <- "3"
  countries$class[countries$Continent == "Africa"] <- "4"
  g<-ggplot(countries, aes(rate_cases, rate_death, color=class)) +
    geom_point(aes(shape=Continent)) + xlab("发病率（1/10万）") + ylab("死亡率（1/10万）") +
    geom_hline(yintercept = 40, color="red") + geom_line(data = data.frame(x=c(1800,1800), y=c(0,40)), aes(x,y), color="red")
  ggsave(sprintf("I:/COVID19/test/数据合并/Countries starta.png"),g,width=10,height=8)
  return(countries)
}
gather_dataset<-function (Dataset,strata){
  CB<-list()
  #WaveNum<-list()
  for (i in 1:nrow(strata)){
    d<-subset(Dataset,Dataset$CountryName==strata$CountryName[i])
    index<-which(d$New_cases>0)[1]
    if (is.na(index)!=TRUE){
      d<-d[index:nrow(d),]
      m<-list()
      for (k in 1:round(nrow(d)/7)){
        l <-7*(k-1)+1
        r<-dplyr::slice(d,breaks=l:(l+6))
        df<-data.frame(Start=character(1),Week=character(1),Country=character(1),Continent=character(1),
                       CountryCode=character(1),Class=character(1),School.closing=numeric(1), Workplace.closing=numeric(1),
                       Cancel.Public.events=numeric(1),Restrictions.on.gatherings=numeric(1), Close.Public.transport=numeric(1),
                       Stay.at.home.requirements=numeric(1),Restrictions.on.internalmovement=numeric(1), International.travel.controls=numeric(1),
                       Facial.Coverings=numeric(1),New_cases=numeric(1),New_deaths=numeric(1), New_cases_lo=numeric(1),
                       Pop=numeric(1), HealthIndex=numeric(1),PopDensity=numeric(1),Aging=numeric(1),Humdity=numeric(1),AirTemp=numeric(1))
        df$Start<-r$Date[1]
        df$Week<-k
        df$Country<-r$CountryName[1]
        df$Continent<-r$Continent[1]
        df$CountryCode<-r$CountryCode[1]
        df$Class<-strata$class[i]
        df$New_cases<-sum(r$New_cases)
        df$New_deaths<-sum(r$New_deaths)
        df$New_cases_lo<-sum(r$New_cases.lo)
        df$School.closing<-ifelse(length(which(r$C1_School.closing>0))>3,1,ifelse(length(which(r$C1_School.closing>0))>0,0,-1))
        df$Workplace.closing<-ifelse(length(which(r$C2_Workplace.closing>0))>3,1,ifelse(length(which(r$C2_Workplace.closing>0))>0,0,-1))
        df$Cancel.Public.events<-ifelse(length(which(r$C3_Cancel.public.events>0))>3,1,ifelse(length(which(r$C3_Cancel.public.events>0))>0,0,-1))
        df$Restrictions.on.gatherings<-ifelse(length(which(r$C4_Restrictions.on.gatherings>0))>3,1,ifelse(length(which(r$C4_Restrictions.on.gatherings>0))>0,0,-1))
        df$Close.Public.transport<-ifelse(length(which(r$C5_Close.public.transport>0))>3,1,ifelse(length(which(r$C5_Close.public.transport>0))>0,0,-1))
        df$Stay.at.home.requirements<-ifelse(length(which(r$C6_Stay.at.home.requirements>0))>3,1,ifelse(length(which(r$C6_Stay.at.home.requirements>0))>0,0,-1))
        df$Restrictions.on.internalmovement<-ifelse(length(which(r$C7_Restrictions.on.internal.movement>0))>3,1,ifelse(length(which(r$C7_Restrictions.on.internal.movement>0))>0,0,-1))
        df$International.travel.controls<-ifelse(length(which(r$C8_International.travel.controls>0))>3,1,ifelse(length(which(r$C8_International.travel.controls>0))>0,0,-1))
        df$Facial.Coverings<-ifelse(length(which(r$H6_Facial.Coverings>0))>3,1,ifelse(length(which(r$H6_Facial.Coverings>0))>0,0,-1))
        df$Pop<-r$POP[1]
        df$HealthIndex<-r$HealthIndex[1]
        df$PopDensity<-r$PopDensity[1]
        df$Aging<-r$Aging[1]
        df$Humdity<-mean(r$Humidity)
        df$AirTemp<-mean(r$AirTemp)
        #df$NewCase_perMil<-df$New_cases/df$Pop*1000000
      m[[k]]<-df
    }
      gather_data<-do.call(rbind,m)
      gather_data$New_cases_lo[which(gather_data$New_cases_lo<0)]<-0
      #####疫情分波
      gather_data$q_new_cases<-ifelse(gather_data$New_cases_lo>=(max(gather_data$New_cases_lo)*0.05),"1","0")
      spl<-split(gather_data,gather_data$q_new_cases)
      spl<-spl$`1`[,2]
      k1<-1
      i0<-spl[1]
      gather_data$wave<-0
      if (length(spl)>=3){
        for(n in 2:length(spl)){
          if (spl[n]-spl[n-1]>=4){
            gather_data$wave[i0:spl[n-1]]<-k1
            i0<-spl[n]
            k1<-k1+1
          }
          else{
            if (i0>spl[1]){
              gather_data$wave[(spl[which(spl==i0)-1]+1):i0]<-0
              gather_data$wave[i0:spl[length(spl)]]<-k1
            }
            else{gather_data$wave[i0:(spl[length(spl)])]<-k1}
          }
        }
      }
      else{print(paste0(strata$CountryName[i]," did not have COVID19"))}
      print(paste0(strata$CountryName[i]," wave number is ",max(gather_data$wave)))
      gather_data$Y1<-0
      for (j in 1:nrow(gather_data)){
        gather_data$Y1[j]<-ifelse(is.na(gather_data$New_cases_lo[j-1][1]),0,(gather_data$New_cases_lo[j]/gather_data$New_cases_lo[j-1]))}
      gather_data$Y1[which(is.infinite(gather_data$Y1))]<-0
      gather_data$Y1[which(is.nan(gather_data$Y1))]<-0
      for (g in 1:nrow(gather_data)){
        gather_data$Y[g]<-ifelse(gather_data$Y1[g]==0,0,log(gather_data$Y1[g]/(gather_data$Y1[which(gather_data$Y1!=0)[1]])))
      }
      CB[[i]]<-gather_data
    }
    else{
      print(paste0(" The number of cases in ",Con_list[i]," is too small"))
    }
    #每7日汇总数据
  }
  CBD<-do.call(rbind,CB)
  return(CBD)
}

Dataset<-merge_dataset(COVID_D1,Policy_P1,POPDATA,IND_DATA,ENV_DATA)#合成所需数据集
strata<-strata_data(Dataset)#国家分层
gather<-gather_dataset(Dataset,strata)#按七天合成数据，对疫情分波
write.csv(gather,file = "I:/COVID19/test/数据合并/Gather_dataset.csv",row.names = F)
gather<-read.csv("I:/COVID19/test/数据合并/Gather_dataset.csv",stringsAsFactors = FALSE)
gatherdata<-subset(gather,gather$wave==i)#[i in 1,2]可选取分波
gatherdata<-subset(gather,gather$Class==i)#[i in 1,2,3,4]#可选取所需国家分层
alpha_calculate<-function (gatherdata){
  week_list<-list()
  for (n in 2:max(gatherdata$Week)){
    data<-subset(gatherdata,gatherdata$Week==n)
    interventions<-data[,c(7:15,20:24)]#每个国家实施每种政策的时间
    interventions$Aging<-as.numeric(sub("%","",interventions$Aging))
    interventions$Humdity<-(interventions$Humdity-min(interventions$Humdity))/(max(interventions$Humdity)-min(interventions$Humdity))
    interventions$AirTemp<-(interventions$AirTemp-min(interventions$AirTemp))/(max(interventions$AirTemp)-min(interventions$AirTemp))
    interventions$Aging<-(interventions$Aging-min(interventions$Aging))/(max(interventions$Aging)-min(interventions$Aging))
    interventions$HealthIndex<-(interventions$HealthIndex-min(interventions$HealthIndex))/(max(interventions$HealthIndex)-min(interventions$HealthIndex))
    interventions$PopDensity<-(interventions$PopDensity-min(interventions$PopDensity))/(max(interventions$PopDensity)-min(interventions$PopDensity))
    Y<-as.vector(data$Y1)
    X<-as.matrix(interventions)
    testdata <- list(m=nrow(X),K=ncol(X),X=X,Y=Y)
    fit<-stan(file=paste0("D:/Rproject/model.stan"),data=testdata)
    out = rstan::extract(fit)
    alpha<-out$alpha#每周各政策的参数
    week_list[[n]]<-alpha
  }
  a_list=list()
  d<-data.frame(matrix(0,ncol=max(gatherdata$Week)-1,nrow=4000))
  mean<-data.frame(matrix(0,ncol=15,nrow=max(gatherdata$Week)-1))
  colnames(mean)<-c('week','School.Closing','Workplace.Closing', 'Cancel.Public.Events', 'Restrictions.on.gatherings',
                    'Close.Publictransport','Stay.at.home.requirements','Restrictions.on.internalmovement',
                    'International.travel.controls','Facial.Coverings','HealthIndex','PopDensity','Aging',
                    'Humdity','AirTemp')
  for (i in 1:14){
    for(n in 1:(max(gatherdata$Week)-1)){
      d[,n]<-week_list[[n+1]][,i]
      mean[n,i+1]<-colMeans(week_list[[n+1]])[i]
      mean$week[n]<-n+1
    }
  a_list[[i]]<-d
  }
  policynames<-c('School.Closing','Workplace.Closing', 'Cancel.Public.Events', 'Restrictions.on.gatherings',
                    'Close.Publictransport','Stay.at.home.requirements','Restrictions.on.internalmovement',
                    'International.travel.controls','Facial.Coverings','HealthIndex','PopDensity','Aging',
                    'Humdity','AirTemp')
  for (l in 1:14){
    alpha <- a_list[[l]]
    weekname<-c()
    for(n in 2:length(week_list)){
      weekname[n-1]<-paste0("week",n)}
    colnames(alpha)<-weekname
    alpha <- as.matrix(alpha)
    g=mcmc_intervals(alpha,prob = .8,prob_outer= .95,point_est="mean")+
      ggplot2::labs(title = paste0(policynames[l]), subtitle = "with means and 95% intervals")
    ggsave(sprintf("I:/COVID19/test/picture/all/class2/%s-alpha.png",policynames[l]),g,width=4,height=6)#图片输出路径需要改一下
  }
  mean<-as.data.frame(mean)
  colorlist <- c("#8FBC94","#546687","#4FB0C6","#4F86C6","#C65146","#EC6A5C","#6E7753",
                 "#77AAAD","#e97f02","#f8ca00","#3a5134","#4f953b","#99CCCC","#FFCC99")
  for (b in 1:14){
    meang<-ggplot(data=mean,aes(x=week,y=mean[,b+1]))+geom_smooth(method="loess",alpha=0.1,size=1,span=1,color=colorlist[b])+
      labs(title=paste0("Variation Curve of Mean Alpha of ",policynames[b]))+ylab("Alpha")+scale_x_continuous("week",seq(0,length(week_list)+1,1))+
      theme(legend.key.size=unit(1,"cm"))
    meanl<-ggplot(data=mean,aes(x=week,y=mean[,b+1]))+geom_line(color=colorlist[b])+
      labs(title=paste0("Variation of Mean Alpha",policynames[b]))+ylab("Alpha")+scale_x_continuous("week",seq(0,length(week_list)+1,1))+
      theme(legend.key.size=unit(1,"cm"))
    ggsave(sprintf("I:/COVID19/test/picture/all/class2/%s-mean-alpha.png",policynames[b]),meanl,width=10,height=6)
    ggsave(sprintf("I:/COVID19/test/picture/all/class2/%s-curve-alpha.png",policynames[b]),meang,width=10,height=6)
  }
}


alpha_calculate(gatherdata)


