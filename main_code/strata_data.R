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
  countries$Group <- "1"
  countries$Group[countries$rate_cases > max(q1) & countries$rate_death > max(q2)] <- "2"
  countries$Group[countries$Continent == "Asia"] <- "3"
  countries$Group[countries$Continent == "Africa"] <- "4"
  g<-ggplot(countries, aes(rate_cases, rate_death)) +
    geom_point(aes(shape=Continent,color=Group),size=2) + xlab("Infection (per 0.1 million)") +
    scale_color_manual(values=c("#ffdb47","#8da636","#1581c7","#a52a2a"))+ ylab("Fatality (per 0.1 million)") +
    geom_hline(yintercept = 40, color="#b22222") + geom_line(data = data.frame(x=c(1800,1800), y=c(-Inf,40)), aes(x,y), color="#b22222")+
    theme(legend.position = "right",legend.text =element_text(size=10,family="T"),
          
          axis.text = element_text(size=10,family="T"),
          axis.title=element_text(hjust = 0.5, vjust=0.2,size = 12,family ="T",face="bold"),
          panel.border = element_rect(fill=NA,color=NA))
  ggsave("picture/Countries starta.pdf",g,width=140,height=100,units="mm",device = cairo_pdf)
  return(countries)
}