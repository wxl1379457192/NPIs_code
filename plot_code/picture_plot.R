theme_set(theme_minimal(base_size=10, base_family="Times New Roman"))
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
#######figure 1##########
fig1<-function(fig1data_list,policy_variable,gather,inputpath,outputpath){
  fig1_list<-list()
  num<-c("Overall","Wave 1","Wave 2")
  for(m in 1:3){
    #fitall<-readRDS(paste0("wave12-10result/waveresult/",fig1data_list[m]))
    fitall<-readRDS(paste0(inputpath,fig1data_list[m]))
    out<-rstan::extract(fitall)
    if(m>1){gatherdata<-subset(gather,gather$Wave==m-1)}else{gatherdata<-gather}
    X<-colMeans(gatherdata[,c("School.closing", "Workplace.closing","Close.Public.transport",
                              "International.travel.controls","Facial.Coverings","Restrict.Movement","Restrict.Gathering")])
    alpha<-as.matrix(out$alpha)
    colnames(alpha)<-policy_variable
    alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
    alpha<-do.call(cbind,alphalist)
    alpha<-(1-exp(-alpha))*100
    colnames(alpha)<-policy_variable
    data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
    write.csv(data,paste0(outputpath,num[m],".csv"),row.names = F)
    data$wave<-num[m]
    fig1_list[[m]]<-data}
  data<-do.call(rbind,fig1_list)
  data$wave<-factor(data$wave,levels = c("Wave 2","Wave 1","Overall") ,ordered = T)
  fig1<-ggplot()+
    geom_linerange(mapping = aes_(xmin =~ll, xmax=~hh, y=~parameter,color= ~wave),show.legend = F,alpha=0.4,
                     data=data, size = 1, position=position_dodge(width = 0.7))+
    geom_hline(show.legend = T,aes(yintercept = -1, color=wave),data=data,size = 1,alpha=0.4)+
    geom_linerange(mapping = aes_(xmin =~l, xmax=~h, y=~parameter,color=~wave),show.legend = F,
                     data=data,size =1.8,position=position_dodge(width = 0.7))+
    geom_point(mapping = aes_(x = ~m, y = ~parameter, color = ~wave),show.legend = T,alpha=0.7,
                 data = data, size =2.5,position= position_dodge(width = 0.7))+
    labs(x = paste0("\u0394","\u03C9","t","(%)"),y = NULL,title =NULL)+theme_bw()+
    scale_x_continuous(expand=c(0,0),limits=range(c(-1:50)))+
    scale_color_manual(name=NULL,values =c("#52610d","#854733","#7ab8cc"),limits=num)+
    theme(legend.position = "top",
          legend.title=element_blank(),
          legend.key.height=unit(0.2,'cm'),
          legend.key.width=unit(1.5,'cm'),
          legend.text = element_text(size = 10,family="Times New Roman"),
          legend.background = element_rect(fill="transparent",color=NA),
          legend.key=element_rect(fill=NA),
          plot.title = element_text(color="black",hjust = 0,vjust=0, size=10,family="Times New Roman"),
          axis.ticks.y= element_line(color="black"),
          axis.text.y = element_text(color="black",vjust=0.5,hjust=1, size=10,family="Times New Roman"),
          panel.grid=element_blank(),
          plot.margin=unit(c(0.01,0.2,0.01,0.2),"cm"),
          panel.background=element_rect(fill = "transparent",colour = NA),
          axis.line = element_line(size=0.05),
          axis.title = element_text(color="black", size=10,family="Times New Roman"),
          axis.text.x = element_text(color="black", size=10,family="Times New Roman"))
  ggsave(paste0(outputpath,"fig1.pdf"),fig1,height=90,width=150,unit="mm",device = cairo_pdf)
}
############figure2##############
fig2<-function(gather,policy_variable,
               outputpath="result_0325withoutmu/result/",
               inputpath="result_0325withoutmu/all/")
{
  g1<-list()
  for(i in 1:4){
    data_list<-list()
    for(j in 1:2){
      all<-readRDS(paste0(inputpath,"G",i,"-W",j,"all.rds"))
      out<-rstan::extract(all)
      gatherdata<-subset(gather,gather$Group==i)
      gatherdata<-subset(gatherdata,gatherdata$Wave==j)
      X<-colMeans(gatherdata[,c("School.closing", "Workplace.closing","Close.Public.transport",
                 "International.travel.controls","Facial.Coverings","Restrict.Movement","Restrict.Gathering")])
      alpha<-as.matrix(out$alpha)
      alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
      alpha<-do.call(cbind,alphalist)
      alpha<-(1-exp(-alpha))*100
      colnames(alpha)<-policy_variable
      data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
      write.csv(data,paste0(outputpath,"W",j,"-G",i,".csv"),row.names = F)
      data$wave<-paste0("Wave ",j)
      data_list[[j]]<-data }
    data<-do.call(rbind,data_list)
    data$wave<-factor(data$wave,levels=c("Wave 2","Wave 1"))
    g1[[i]]<-ggplot()+
      geom_linerange(mapping = aes_(xmin =~ll, xmax=~hh, y=~parameter,color= ~wave),show.legend = F,
                     data=data, size = 1,alpha=0.4, position=position_dodge(width = 0.8))+
      geom_hline(show.legend = T,aes(yintercept = -1, color=wave),data=data,alpha=0.4,size = 1)+
      geom_linerange(mapping = aes_(xmin =~l, xmax=~h, y=~parameter,color= ~wave),show.legend = F,
                     data=data, size = 1.8,alpha=1, position=position_dodge(width = 0.8))+
      geom_point(mapping = aes_(x = ~m, y = ~parameter, color = ~wave),
                 data = data, size = 2.5,alpha=0.7,position=position_dodge(width = 0.8))+
      scale_color_manual(name=NULL,values =c("#854733","#7ab8cc"),limits=c("Wave 1","Wave 2"))+
      scale_x_continuous(expand=c(0,0),limits=range(c(-5:75)))+theme_bw()+
      labs( x =paste0("\u0394","\u03C9","t","(%)"),y = NULL,title = paste0("Group ",i))+
      theme(legend.position = "none",
            plot.title = element_text(color="black",hjust = 0,vjust=0,size = 10,family="Times New Roman"),
            axis.title.x= element_text(color="black",size = 10,family="Times New Roman"),
            axis.text.x= element_text(color="black",size = 10,family="Times New Roman"),
            axis.text.y= element_blank(),
            axis.ticks.y= element_blank(),
            plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm"),
            panel.grid=element_blank(),
            panel.background=element_rect(fill = "transparent",colour = NA))}
  g1[[1]]<-g1[[1]]+theme(axis.ticks.y=element_line(color="black"),
                         axis.text.y= element_text(color="black",hjust=1,size = 10,family="Times New Roman"))
  legend<-g1[[1]]+theme(legend.position = "top",
                        legend.title=element_blank(),
                        legend.text = element_text(size = 10,family="Times New Roman"),
                        legend.key.height=unit(0.2,'cm'),
                        legend.key.width=unit(1,'cm'),
                        legend.background = element_rect(fill="transparent"),
                        legend.key=element_rect(fill="transparent"))
  mylegend<-g_legend(legend)
  plot<-grid.arrange(mylegend,arrangeGrob(grobs =g1,ncol =4,widths=c(5,2,2,2)),heights = c(0.5,10))
  ggsave(paste0(outputpath,"fig2.pdf"),plot,height=60,width=160,unit="mm",device = cairo_pdf)
}
#######figure 3########
figuredata_cal<-function(policy_variable,input){
  figuredata<-list()
  wavelist<-list()
  for(j in 1:2){
    for(i in 1:4){
      week_list<-readRDS(paste0(input,"/G",i,"-W",j,"weeklist.rds"))
      gatherdata<-subset(gather,gather$Group==i)
      gatherdata<-subset(gatherdata,gatherdata$Wave==j)
      fd<-as.data.frame(do.call(rbind,lapply(seq(1:length(week_list)),FUN=function(n){
        v<-week_list[[n]]
        gatherdata<-subset(gatherdata,gatherdata$Week==n)
        if(nrow(gatherdata)>3){
          X<-colMeans(gatherdata[,c("School.closing", "Workplace.closing","Close.Public.transport",
                                    "International.travel.controls","Facial.Coverings","Restrict.Movement","Restrict.Gathering")])
          out<-rstan::extract(v)
          weekdata<-as.matrix(out$alpha)
          alphalist<-lapply(seq(1:ncol(weekdata)),FUN = function(k){weekdata[,k]*X[k]})
          weekdata<-do.call(cbind,alphalist)
          apply((1-exp(-weekdata))*100,2,median)}})))
      colnames(fd)<-policy_variable
      fd$group<-paste0("Group ",i)
      fd$week<-seq(1,nrow(fd))
      figuredata[[i]]<-fd
    }
    wavelist[[j]]<-do.call(rbind,figuredata)}
  return(wavelist)
}
fig3<-function(wavedata,output){
  wave<-list()
  for(W in 1:2){
    wave[[W]]<-melt(wavedata[[W]],id=c("week","group"))
    wave[[W]]$wave<-W
  }
  Wdata<-do.call(rbind,wave)
  w<-ggplot(data=Wdata,aes(x=week,y=variable,fill=paste(variable,wave),height=value/70))+
    geom_ridgeline(size=0.3,alpha=0.7,color="grey50")+
    facet_wrap(~group)+ labs(x="Week",y=NULL)+
    scale_x_continuous(expand=c(0,0),limits=range(c(0:(max(Wdata$week)))))+
    scale_y_discrete(expand=c(0.01,0.01))+
    scale_fill_cyclical(values = c("#854733","#7ab8cc"),
                        breaks = c("School closures 1","School closures 2"),
                        labels = c('School closures 1'="Wave 1",'School closures 2'="Wave 2"),
                        name = NULL,guide=guide_legend(nrow = 1))+theme_bw()+
    theme(legend.position = "top",
          legend.text = element_text(size = 10,family="Times New Roman",color="black",),
          legend.key.height=unit(0.2,'cm'), legend.key.width=unit(1,'cm'),
          strip.background = element_rect(color="white",fill="white"),
          strip.text = element_text(color="black",size = 10,family="Times New Roman"),
          plot.title = element_text(color="black",hjust = 0,vjust=0,size = 10,family="Times New Roman"),
          axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size = 10,family="Times New Roman"),
          axis.text= element_text(color="black",hjust = 1,vjust=0,size = 10,family="Times New Roman"),
          plot.margin=unit(c(0.01,1,0.01,1),"cm"),
          panel.grid=element_blank(), legend.background = element_blank(),
          legend.key=element_rect(fill="transparent"),
          panel.background=element_rect(fill = "transparent",colour = "black"))
  ggsave(w,file=paste0(output,"/fig3.pdf"),height=150,width=160,unit="mm",device = cairo_pdf)
}
##################picture in SI###################
#########R-hat and RSS#############
modeltranplot<-function(alloutput,waveoutput){
  rhat<-list()
  R_ESS<-list()
  for(i in 1:length(allfitlist)){
    allfit<-readRDS(paste0(alloutput,allfitlist[i]))
    G<-as.numeric(substr(allfitlist[i],2,2))
    W<-as.numeric(substr(allfitlist[i],5,5))
    out_rhat<-as.data.frame(rhat(allfit))#Rhat
    colnames(out_rhat)<-"x"
    Rneff<-as.data.frame(neff_ratio(allfit))####ESS
    colnames(Rneff)<-"x"
    rhat[[i+4]]<-ggplot(out_rhat,aes(x))+geom_histogram(bins=50,colour="#a23434",fill="#a23434")+
      labs(x=NULL,y=NULL,title=paste0("Group ",G," Wave ",W))+scale_x_continuous(expand=(c(0,0)),limits=c(0.995,1.005))+
      scale_y_continuous(expand=(c(0,0)))+geom_vline(xintercept = 1,linetype ="dotdash",size=1)+
      theme(axis.line.y= element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            axis.text.x = element_blank(),
            plot.title=element_text(size = 10,family="Times New Roman",hjust=0),
            panel.grid=element_blank(),
            axis.line.x = element_line(colour = "black"),
            axis.ticks.x = element_line(colour = "black"),
            panel.background=element_rect(fill = "transparent",colour = NA),
            plot.margin=unit(c(0.1,0.2,0.1,0.2),"cm"),
            axis.text=element_text(size = 10,family="Times New Roman"))
    R_ESS[[i+4]]<-ggplot(Rneff,aes(x))+geom_histogram(bins=50,colour="#a23434",fill="#a23434")+
      labs(x=NULL,y=NULL,title=paste0("Group ",G," Wave ",W))+scale_x_continuous(expand=(c(0,0)),limits=c(0,1.1))+
      scale_y_continuous(expand=(c(0,0)))+geom_vline(xintercept = 1,linetype ="dotdash",size=1)+
      theme(axis.line.y= element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            axis.text.x = element_blank(),
            plot.title=element_text(size = 10,family="Times New Roman",hjust=0),
            panel.grid=element_blank(),
            axis.line.x = element_line(colour = "black"),
            axis.ticks.x = element_line(colour = "black"),
            panel.background=element_rect(fill = "transparent",colour = NA),
            plot.margin=unit(c(0.1,0.2,0.1,0.2),"cm"),
            axis.text=element_text(size = 10,family="Times New Roman"))}
  for(j in 1:length(wavefitlist)){
    wavefit<-readRDS(paste0(waveoutput,wavefitlist[j]))
    name<-strsplit(wavefitlist[j],split=".rds")[[1]]
    out_rhat<-as.data.frame(rhat(wavefit))#Rhat
    colnames(out_rhat)<-"x"
    Rneff<-as.data.frame(neff_ratio(allfit))
    colnames(Rneff)<-"x"
    rhat[[j]]<-ggplot(out_rhat,aes(x))+geom_histogram(bins=50,colour="#a23434",fill="#a23434")+
      labs(x=NULL,y=NULL,title=name)+scale_x_continuous(expand=(c(0,0)),limits=c(0.995,1.005))+
      scale_y_continuous(expand=(c(0,0)))+geom_vline(xintercept = 1,linetype ="dotdash",size=1)+
      theme(axis.line.y= element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            axis.text.x = element_blank(),
            plot.title=element_text(size = 10,family="Times New Roman",hjust=0),
            panel.grid=element_blank(),
            axis.line.x = element_line(colour = "black"),
            axis.ticks.x = element_line(colour = "black"),
            panel.background=element_rect(fill = "transparent",colour = NA),
            plot.margin=unit(c(0.1,0.2,0.1,0.2),"cm"),
            axis.text=element_text(size = 10,family="Times New Roman"))
    R_ESS[[j]]<-ggplot(Rneff,aes(x))+geom_histogram(bins=50,colour="#a23434",fill="#a23434")+
      labs(x=NULL,y=NULL,title=name)+scale_x_continuous(expand=(c(0,0)),limits=c(0,1.1))+
      scale_y_continuous(expand=(c(0,0)))+geom_vline(xintercept = 1,linetype ="dotdash",size=1)+
      theme(axis.line.y= element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            axis.text.x = element_blank(),
            plot.title=element_text(size = 10,family="Times New Roman",hjust=0),
            panel.grid=element_blank(),
            axis.line.x = element_line(colour = "black"),
            axis.ticks.x = element_line(colour = "black"),
            panel.background=element_rect(fill = "transparent",colour = NA),
            plot.margin=unit(c(0.1,0.2,0.1,0.2),"cm"),
            axis.text=element_text(size = 10,family="Times New Roman"))}
  for(i in 1:length(rhat)){
     if(i>=9){
       rhat[[i]]<-rhat[[i]]+theme(axis.text.x =element_text(size = 6.5,family="Times New Roman"))
       R_ESS[[i]]<-R_ESS[[i]]+theme(axis.text.x =element_text(size = 6.5,family="Times New Roman"))}
  }
  rhatplot<-grid.arrange(arrangeGrob(grobs=rhat,nrow =3,heights=c(5,5, 5.5),
                         top=textGrob("Rhat"),gp=gpar(fontsize=10,hjust=0,fontfamily="Times New Roman")))
  ressplot<-grid.arrange(arrangeGrob(grobs=R_ESS,nrow =3,heights=c(5,5, 5.5),
                         top=textGrob("R_ESS"),gp=gpar(fontsize=10,hjust=0,fontfamily="Times New Roman")))
  ggsave("picture/figure5-1.pdf",rhatplot,width=160,height=75,units="mm",device = cairo_pdf)
  ggsave("picture/figure5-2.pdf",ressplot,width=160,height=75,units="mm",device = cairo_pdf)
}
#########week_list result#########
SIfig<-function(weekfitlist,gather,policy_variable,allinput,weekinput){
  fig6plot<-list()
  for(i in 1:length(weekfitlist)){
    weekfit<-readRDS(paste0(weekinput,weekfitlist[i]))
    G<-as.numeric(substr(weekfitlist[i],2,2))
    W<-as.numeric(substr(weekfitlist[i],5,5))
    allfit<-readRDS(paste0(allinput,"G",G,"-W",W,"all.rds"))
    figure_list<-list()
    a_list<-list()
    for (p in 1:length(policy_variable)){
      d<-data.frame(matrix(0,ncol=length(weekfit),nrow=9000))
          for(n in 1:length(weekfit)){
            out_week<-as.matrix(rstan::extract(weekfit[[n]])$alpha)
            d[,n]<-out_week[,p]}
      a_list[[p]]<-d}
    gatherdata<-subset(gather,gather$Wave==W)
    gatherdata<-subset(gatherdata,gatherdata$Group==G)
    plotdata<-gatherdata[,c("Week","Y1")]%>%
      group_by(Week)%>%summarise(number=mean(Y1))
    if(min(gatherdata$Week)==2){plotdata$Week<-plotdata$Week-1}
    policy_name<- c("School.closing", "Workplace.closing", "Close.Public.transport", "International.travel.controls",
                    "Facial.Coverings","Restrict.Movement","Restrict.Gathering")
    for (l in 1:length(policy_variable)){
      policydata<-gatherdata[,c("Week",policy_name[l])]
      colnames(policydata)<-c("Week","Policy")
      policydata<-policydata%>%group_by(Week)%>%
        summarise(strength=mean(Policy))
      if(min(policydata$Week)==2){
        policydata$Week<-policydata$Week-1}
      alpha <- a_list[[l]]
      weekname<-c()
      for(n in 1:length(weekfit)){
        weekname[n]<-n}
      alpha<-1-exp(-alpha)
      alpha <- as.data.frame(t(alpha))
      alpha$week<-weekname
      alpha<-melt(alpha,"week")
      color_scheme_set("red")
      pd<-subset(plotdata,plotdata$Week<=length(weekfit))
      policydata<-subset(policydata,policydata$Week<=length(weekfit))
      figure_list[[l+1]]<-ggplot()+geom_col(data=pd,aes(x=Week,y=rescale(number,c(0,1))),fill="#e4ddde")+
        scale_y_continuous(limits=c(-0.12,1),sec.axis = sec_axis(~rescale(.,c(0,max(pd$number))),name=paste0("\u0394","New cases")))+
        stat_summary(data=alpha,aes(x=week,y=value,group=week),
                     fun.min = function(z) { quantile(z,0.25) },
                     fun.max = function(z) { quantile(z,0.75) },
                     fun = median,size=0.5,color="#A73E3E")+
        geom_tile(data=policydata,aes(x=Week,y=-0.06,fill=strength),height = 0.1)+
        scale_fill_gradientn(colours= c("#f1e5e5","#a46a6c","#503636"), limits=c(0,1))+
        labs( x = "Week",y = paste0("\u0394","\u03C9","t","(%)"),title = paste0(policy_variable[l]))+theme_classic()+
        theme(legend.position = "none",
              plot.title = element_text(color="black",hjust = 0.5, size = 12,family ="T"),
              axis.text = element_blank(),
              plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
              axis.title=element_blank(),
              axis.ticks.y = element_blank(),
              panel.border = element_rect(fill=NA,color=NA))
      outall<-rstan::extract(allfit)
      alpha_all<-1-exp(-as.matrix(outall$alpha))
      colnames(alpha_all)<-policy_variable
      figure_list[[1]]<-mcmc_intervals(alpha_all,prob = .5,prob_outer= .95,point_est="median")+
        ggplot2::labs( x = NULL,y = NULL,title=paste0("Group ",G," Wave ",W))+theme_classic()+
        theme(axis.text.y.left = element_text(color="black",size=12,family ="T"),
              legend.position = "none",
              plot.title = element_text(color="black",hjust = 0, size = 12,family ="T"),
              plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
              axis.text = element_text(color="black",size=12,family ="T"))}
    figure_list[[2]]<-figure_list[[2]]+
      theme(plot.title = element_text(color="black",hjust = 0.5, size = 12,family ="T"),
            axis.ticks.y.left = element_line(color="black"))
    figure_list[[4]]<-figure_list[[4]]+
      theme(axis.text.y.right = element_text(color="black",size=12,family ="T"),
            axis.ticks.y.right= element_line(color="black"),
            axis.title.y.right=element_text(color="black",hjust = 0.5, vjust=0.2,size = 12,family ="T"))
    figure_list[[5]]<-figure_list[[5]]+
      theme(axis.text.y.left = element_text(color="black",size=12,family ="T"),
            axis.text.x = element_text(color="black",size=12,family ="T"),
            axis.ticks.y.left= element_line(color="black"),
            axis.title.y.left =element_text(color="black",hjust = 0.5, vjust=0.2,size = 12,family ="T"),
            axis.title.x =element_text(color="black",hjust = 0.5, vjust=0.2,size = 12,family ="T"))
    figure_list[[6]]<-figure_list[[6]]+
      theme(axis.text.x = element_text(color="black",size=12,family ="T"),
            axis.title.x =element_text(color="black",hjust = 0.5, vjust=0.2,size = 12,family ="T"))
    figure_list[[7]]<-figure_list[[7]]+
      theme(axis.text.x = element_text(color="black",size=12,family ="T"),
            axis.title.x =element_text(color="black",hjust = 0.5, vjust=0.2,size = 12,family ="T"))
    figure_list[[8]]<-figure_list[[8]]+
      theme(axis.text.y.right = element_text(color="black",size=12,family ="T"),
            axis.title.y.right=element_text(color="black",hjust = 0.5, vjust=0.2,size = 12,family ="T"),
            axis.text.x = element_text(color="black",size=12,family ="T"),
            axis.ticks.y.right= element_line(color="black"),
            axis.title.x =element_text(color="black",hjust = 0.5, vjust=0.2,size = 12,family ="T"))
    fig6plot[[i]]<-grid.arrange(arrangeGrob(grobs=figure_list[1:4],nrow =1,widths=c(5,4,4,4.5)),
                                arrangeGrob(grobs=figure_list[5:8],nrow =1,widths=c(5,4,4,4.5)),heights=c(3,3.5))
  }
  fig6leg<-figure_list[[2]]+
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.text = element_text(size=12,family = "T"),
          legend.box = "horizontal",
          legend.key.height=unit(0.3,'cm'),
          legend.key.width=unit(4,'cm'),
          legend.background = element_rect(fill="transparent"),
          legend.key=element_rect(fill="transparent"))+
    guides(color=guide_legend(byrow=TRUE))
  fig6legend<-g_legend(fig6leg)
  ggsave("picture/fig6-1.pdf",grid.arrange(arrangeGrob(grobs = fig6plot[c(1,3,6,9)], ncol = 1),fig6legend,heights=c(10, 0.3)),
       width=320,height=401,units="mm",device = cairo_pdf)
  ggsave("picture/fig6-2.pdf",grid.arrange(arrangeGrob(grobs = fig6plot[c(2,4,7,10)], ncol = 1),fig6legend,heights=c(10, 0.3)),
       width=320,height=401,units="mm",device = cairo_pdf)
  ggsave("picture/fig6-3.svg",grid.arrange(arrangeGrob(grobs = fig6plot[c(5,8)], ncol = 1),fig6legend,heights=c(10, 0.5)),
       width=320,height=201,units="mm",device = cairo_pdf)
}
#######NPIs correlation analysis###########
COR<-function(gather){
  x1<-gather[,c(8:16,18:22)]
  x1$Aging<-as.numeric(gsub("\\%", "",x1$Aging))
  colnames(x1)<- c("School closures", "Workplace closures", "Public transport closures",
                   "Gatherings restrictions", "Close public transport",
                   "Stay at home requirements","Internalmovement restrictions",
                   "International travel control", "Facial coverings",
                   "Health index", "Pop density", "Aging", "Humdity", "Air temp")
  p1<-quickcor(x1, cor.test = TRUE)+geom_star(data = get_data(type = "upper" ,show.diag = FALSE))+
    geom_mark(data = get_data(type = "lower", show.diag = FALSE),size=2,family="T")+
    geom_abline(size=0.5,slope=-1,intercept =15)+
    scale_fill_gradient2(midpoint = 0, low = "#00498d", mid = "white", high = "#74230a",space="Lab")+
    theme(legend.position = "right",
          legend.text = element_text(color="black",size=12,family="T"),
          legend.title = element_text(color="black",size=12,family="T"),
          legend.key.height=unit(10,'mm'),
          legend.key.width=unit(2,'mm'),
          axis.text = element_text(color="black",size=12,family="T"))
  return(p1)
}
###################plot policy intensity###############
policyfig<-function(Countrylist,gather){
  Pfiglist<-list()
  for(n in 1:length(Countrylist)){
  Countrydata<-gather[which(gather$Country==Countrylist[n]),c("Start","School.closing", "Workplace.closing",
                                                              "Close.Public.transport", "International.travel.controls",
                                                              "Facial.Coverings","Restrict.Movement","Restrict.Gathering")]
  colnames(Countrydata)<-c("Start","School closures", "Workplace closures",
              "Public transport closures","International travel restrictions",
              "Facial coverings", "Movement restrictions","Gathering restrictions")
  Countrydata$Start<-substring(Countrydata$Start,6,10)
  DD<-melt(Countrydata,id="Start")
  Pfiglist[[n]]<-ggplot(DD,aes(x=Start,y=variable,fill=value))+geom_tile(color="white")+
    scale_fill_gradient2(midpoint = 0, low = "white",
                         mid = "#f0e5cc", high = "#723021",space="Lab")+
    labs(title=paste0("Group ",n,": ",Countrylist[n]),y=NULL,x=NULL)+
    theme(axis.text.x=element_text(color="black",hjust=1,angle=90,family="Times New Roman",size=10),
          axis.ticks.y= element_line(color="black"),
          plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm"),
          axis.text.y=element_blank(),
          plot.title=element_text(color="black",family="Times New Roman",size=10,hjust=0.5),
          legend.position = "none")}
  for(n in c(1,3)){Pfiglist[[n]]<-Pfiglist[[n]]+
  theme(axis.text.y=element_text(color="black",family="Times New Roman",size=10))}
  legend<-Pfiglist[[1]]+theme(legend.position = "top",
                            legend.title=element_blank(),
                            legend.text = element_text(size=10,family = "Times New Roman"),
                            legend.box = "horizontal",
                            legend.key.height=unit(0.2,'cm'),
                            legend.key.width=unit(2,'cm'),
                            legend.background = element_rect(color="transparent",fill="transparent"),
                            legend.key=element_rect(fill="transparent"))+guides(color=guide_legend(byrow=TRUE))
  leg<-g_legend(legend)
  plot<-grid.arrange(leg,arrangeGrob(grobs=Pfiglist,ncol=2,widths = c(6,4)),heights=c(1,10))
  return(plot)
}







