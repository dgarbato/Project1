library(shiny)

telco=read_csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

telco <- telco %>% 
            mutate(SeniorCitizen = ifelse(SeniorCitizen == 0, 'No', 'Yes'))

char_tab = function(infile,var){
  infile%>%
    mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
    group_by_at(var)%>%
    summarise(Observations=n(), Churn_Rate=mean(Churn2),
              Index=round(mean(Churn2)/0.2653699 * 100)
    ) -> outfile
  return(outfile)
}

##############create tenure tab#########################################################################
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

##############MonthlyCharges#########################################################################
telco%>%
  mutate(MonthlyCharges.decile = cut(MonthlyCharges, 
                                     breaks= quantile(MonthlyCharges, 
                                                      probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                     include.lowest= TRUE, labels= c(1:10)))%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(MonthlyCharges.decile)%>%
  summarise(min(MonthlyCharges), max(MonthlyCharges), Observations=n(),Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100),
  )-> MonthlyCharges_Tab

##############Total Charges#########################################################################
telco%>%
  mutate(TotalCharges.decile = cut(TotalCharges, 
                                   breaks= quantile(TotalCharges, 
                                                    probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                   include.lowest= TRUE, labels= c(1:10)))%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(TotalCharges.decile)%>%
  summarise(min(TotalCharges), max(TotalCharges), Observations=n(),Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100),
  )-> TotalCharges_Tab