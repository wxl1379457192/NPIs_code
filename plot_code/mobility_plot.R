mobility_plot<-function(mobility,wave_data){
  M1 <- read.csv(mobility, stringsAsFactors = FALSE)
  M1<-subset(M1,M1$sub_region_1=="")
  M1<-subset(M1,M1$metro_area=="")
  M1<-M1[,c(1,2,9,12,15)]
  colnames(M1) <- c("CountryCode", "CountryName", "Date1", "Parks", "Residential")
  M1$Date1<-as.Date(M1$Date1)
  wave_data$Start<-as.Date(wave_data$Start)
  wave_data$End<-as.Date(wave_data$End)
  Group<-lapply(seq(1:4),function(v) { Con_list<-unique(subset(strata,strata$Group==v)$'CountryName')
    A<-lapply(Con_list,function(i){
      wavetime<-subset(wave_data,wave_data$Country==i)
      M <-subset(M1, M1$CountryName == i)[,-c(1, 2)]
      D <-subset(Dataset, Dataset$CountryName == i)
      len <- c(nrow(M), nrow(D), nrow(wavetime))
          if (min(len) > 0){
            m <- merge(M, D, by = "Date1")[,c(1:3)]
            mp <- melt(m , id = "Date1")
            mp$value<-mp$value/100
               if(nrow(wavetime)>=2){
                 plot<-ggplot(data=mp,aes(x=Date1,y=value,color=variable),size=.5)+
                   geom_rect(aes(xmin=wavetime$Start[1], xmax=wavetime$End[1], ymin=-Inf, ymax=Inf),
                              fill=alpha('#d3d7c2',0.1),color=NA)+
                   geom_rect(aes(xmin=wavetime$Start[2], xmax=wavetime$End[2], ymin=-Inf, ymax=Inf),
                              fill=alpha('#d3d7c2',0.5),color=NA)+
                   geom_hline(aes(yintercept = 0), linetype="dashed",colour="#990000")+ geom_line()+
                   scale_color_manual(values=c("#697723","#5e86c1"))+
                   scale_y_continuous(labels = percent,limits = c(-1,1))+
                   labs(x = NULL,y=NULL,title=i)+theme_bw()+guides(color = guide_legend(nrow = 1))+
                   theme(plot.title =  element_text(hjust=0.5,vjust=0,size=9,family ="C",face="bold"),
                         axis.text.y = element_blank(),
                         legend.position = "none",
                         panel.background=element_rect(fill = "transparent",colour = NA),
                         plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm"),
                         axis.text.x =element_text(size=8,family = "C",face="bold"),
                         panel.grid=element_blank())}else{plot<-NULL}}else{plot<-NULL}
      return(plot)})
    A<-A[-which(sapply(A,is.null))]
    return(A)})
  legend<-Group[[1]][[1]]+theme(
    legend.position = c(0.1,0.9),legend.title=element_blank(),
    legend.text = element_text(size=9,family = "C",face="bold"),
    legend.box = "horizontal",
    legend.background = element_rect(fill="transparent"),
    legend.key.height=unit(2,'mm'),legend.key.width=unit(5,'mm'),
    legend.key=element_rect(fill="transparent"))
  ml<-g_legend(legend)
      for(g in 1:4){
        G<-Group[[g]]
        len<-length(G)
            for(n in 1:ceiling(len/4)){G[[(n-1)*4+1]]<-G[[(n-1)*4+1]]+
              theme(axis.text.y =element_text(size=8,family = "C",face="bold"))}
        Gplot<-grid.arrange(arrangeGrob(grobs=G,ncol=4,widths=c(5.5,5,5,5),
                                        top=textGrob(paste0("Group ",g)),
                                        gp=gpar(fontsize=10,hjust=0,fontfamily="C",fontface="bold")),
                            ml,heights=c(10,0.3))
        ggsave(paste0("picture/Group",g,".tiff"),Gplot,height=40*ceiling(len/4),width=240,unit="mm")}
}
