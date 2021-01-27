telco=read_csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

telco%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%                                          
  summarise(Churn_Rate=mean(Churn2),Observations=n())-> Overall_Churn_Rate

telco%>%
  mutate(tenure.decile = cut(tenure, 
                             breaks= quantile(tenure, 
                                              probs= seq(0, 1, by= 0.1)),
                             include.lowest= TRUE, labels= c(1:10)))%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(tenure.decile)%>%
  summarise(min(tenure), max(tenure), Observations=n(),Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100),
  )-> tenure_Tab

telco%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(InternetService)%>%
  summarise(Observations=n(), Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100)
  )->InternetService
#how these factors seem to be impactiing churn

char_tab = function(infile,var){
  infile%>%
    mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
    group_by_at(var)%>%
    summarise(Observations=n(), Churn_Rate=mean(Churn2),
              Index=round(mean(Churn2)/0.2653699 * 100)
    ) -> outfile
  return(outfile)
}

num_tab = function(infile,var){
  library(stringr)
  var.decile=paste0(var,'.decile')
  
  infile%>%
    mutate(var.decile = cut(var, 
                            breaks= quantile(var, 
                                             probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                            include.lowest= TRUE, labels= c(1:10)))%>%
    mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
    group_by_at(var.decile)%>%
    summarise(min(var), max(var), Observations=n(),Churn_Rate=mean(Churn2),
              Index=round(mean(Churn2)/0.2653699 * 100),
    )-> outfile
  return(outfile)
}

char_tab(telco,"InternetService",InternetService_Tab)
Parnter_tab = char_tab(telco,"Partner")
num_tab(telco,'MonthlyCharges')
