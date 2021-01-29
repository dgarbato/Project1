telco=read_csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

#############calculate overall churn rate and totla observations##################
telco%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%                                          
  summarise(Churn_Rate=mean(Churn2),Observations=n())-> Overall_Churn_Rate

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
###########################Internet Services##########################################################################
telco%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(InternetService)%>%
  summarise(Observations=n(), Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100)
  )->InternetService
#########################Character function and tabs###########################################################################################
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



char_tab(telco,"InternetService",InternetService_Tab)#yes
Parnter_tab = char_tab(telco,"Partner")#yes
#num_tab(telco,'MonthlyCharges')
SeniorCitizen_tab=char_tab(telco,'SeniorCitizen')#yes
gender_tab=char_tab(telco,'gender')#no
Dependents_tab=char_tab(telco,'Dependents')#slightly with those without
PhoneService_tab=char_tab(telco,'PhoneService')#no
MultipleLines_tab=char_tab(telco,'MultipleLines')#no
OnlineSecurity_tab=char_tab(telco,'OnlineSecurity')#yes when it is equal to "No"
OnlineBackup_tab=char_tab(telco,'OnlineBackup')#yes when it is equal to "No"
DeviceProtection_tab=char_tab(telco,'DeviceProtection')#yes when it is equal to "No"
TechSupport_tab=char_tab(telco,'TechSupport')#yes when it is equal to "No"
StreamingTV_tab=char_tab(telco,'StreamingTV')#somewhat
StreamingMovies_tab=char_tab(telco,'StreamingMovies')#somewhat
Contract_tab=char_tab(telco,'Contract')#yes Month to Month
PaperlessBilling_tab=char_tab(telco,'PaperlessBilling')#yes moderately                                               
PaymentMethod_tab=char_tab(telco,'PaymentMethod') #yes electronic check
