waveplot<-function(D1,wave_data){
  theme_set(theme_minimal(base_size=10, base_family="Times New Roman"))
  waveplot_list<-lapply(split(D1,D1$CountryName),function(v){
      C <- v[, c("Date", "New_cases", "New_cases_smoothed")]
      longmean <- melt(as.data.frame(C), id = "Date")
      wavetime<-wave_data[which(wave_data$Country==v$CountryName[1]),]
      if(nrow(wavetime)>0){
        if(nrow(wavetime)==1){
          plot <- ggplot(data = longmean, aes(x = Date, y = value, color = variable)) +
            geom_rect(aes(xmin=wavetime$Start[1], xmax=wavetime$End[1], ymin=-Inf, ymax=Inf),fill=alpha('#ece3da',0.2),color=NA)+
            geom_line(size=0.4) +xlim(min(D1$Date),max(D1$Date))+
            scale_colour_manual(values=c("#2f5483","#cd6e00"))+ labs(x=NULL,y=NULL,title = v$CountryName[1])+
            theme(legend.position = "none", plot.title = element_text(hjust = 0.5,vjust=0,size=10,family = "C",face="bold"),
                plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm"),
                axis.text = element_text(size=7,vjust=0,hjust=0.7,family = "C",face="bold"),
                panel.background = element_rect(fill="white"),axis.line=element_line(color="black"))}
        if(nrow(wavetime)==2){plot <- ggplot(data = longmean, aes(x = Date, y = value, color = variable)) +
          geom_rect(aes(xmin=wavetime$Start[1], xmax=wavetime$End[1], ymin=-Inf, ymax=Inf),fill=alpha('#ece3da',0.2),color=NA)+
          geom_rect(aes(xmin=wavetime$Start[2], xmax=wavetime$End[2], ymin=-Inf, ymax=Inf),fill=alpha('#dedede',0.1),color=NA)+
          geom_line(size=0.4) +xlim(min(D1$Date),max(D1$Date))+
          scale_colour_manual(values=c("#2f5483","#cd6e00"))+ labs(x=NULL,y=NULL,title = v$CountryName[1])+
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5,vjust=0,size=10,family = "Times New Roman"),
                plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm"),
                axis.text = element_text(size=7,vjust=0,hjust=0.7,family = "Times New Roman"),
                panel.background = element_rect(fill="white"),axis.line=element_line(color="black"))}
        else{plot <- ggplot(data = longmean, aes(x = Date, y = value, color = variable)) +
          geom_rect(aes(xmin=wavetime$Start[1], xmax=wavetime$End[1], ymin=-Inf, ymax=Inf),fill=alpha('#ece3da',0.2),color=NA)+
          geom_rect(aes(xmin=wavetime$Start[2], xmax=wavetime$End[2], ymin=-Inf, ymax=Inf),fill=alpha('#dedede',0.1),color=NA)+
          geom_rect(aes(xmin=wavetime$Start[3], xmax=wavetime$End[3], ymin=-Inf, ymax=Inf),fill=alpha('#eeded2',0.1),color=NA)+
          geom_line(size=0.4) +xlim(min(D1$Date),max(D1$Date))+
          scale_colour_manual(values=c("#2f5483","#cd6e00"))+ labs(x=NULL,y=NULL,title = v$CountryName[1])+
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5,vjust=0,size=10,family = "Times New Roman"),
                plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm"),
                axis.text = element_text(size=7,vjust=0,hjust=0.7,family = "Times New Roman"),
                panel.background = element_rect(fill="white"),axis.line=element_line(color="black"))}
        #ggsave(sprintf("picture/wavetime/%s-Gaussian.pdf", v$CountryName[1]), plot, width = 6, height = 5, units = "cm",device = cairo_pdf)
        ggsave(sprintf("picture/wavetime/%s-Gaussian.jpg", v$CountryName[1]), plot, width = 8, height =6)
      }
      else{print(paste0(v$CountryName[1],"did not have COVID"))}
      return(plot)
    })
}
