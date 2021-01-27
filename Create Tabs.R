telco=read_csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
tenure.decile = cut(tenure, 
                    breaks= quantile(tenure, 
                                     probs= seq(0, 1, by= 0.1)),
                    include.lowest= TRUE, labels= c(1:10))
round(x, digits = 0)

str(telco)
summary(telco) 

######################## bombed ##################################################################
telco%>%
  mutate(tenure.decile = cut(tenure, 
                      breaks= quantile(tenure, 
                      probs= seq(0, 1, by= 0.1)),
                      include.lowest= TRUE, labels= c(1:10)))%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(tenure.decile)%>%
  summarise(min(tenure), max(tenure), Observations=n(), digits=2),Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100),
            )-> Tenure_Tab
################################################################################################

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
  mutate(MonthlyCharges.decile = cut(MonthlyCharges, 
                                     breaks= quantile(MonthlyCharges, 
                                                      probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                     include.lowest= TRUE, labels= c(1:10)))%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(MonthlyCharges.decile)%>%
  summarise(min(MonthlyCharges), max(MonthlyCharges), Observations=n(),Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100),
  )-> MonthlyCharges_Tab

############### Below doesn't bomb anymore ##############################################################
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
####################################################################################
#note: 0.2653699 is the overall churn rate and there are 7043 observations
#would like to include percentage of total obs in each group
#would like to include number of Churns
#function to include for loop that loops through a list of variables


telco%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%                                          
  summarise(Churn_Rate=mean(Churn2),Observations=n())-> Overall_Churn_Rate



+

telco%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(InternetService)%>%
  summarise(Observations=n(), Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100)
            )->InternetService


char_tab = function(infile,var,outfile){
  infile%>%
    mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
    group_by(var)%>%
    summarise(Observations=n(), Churn_Rate=mean(Churn2),
              Index=round(mean(Churn2)/0.2653699 * 100)
    ) -> outfile
              return(outfile)
}

num_tab = function(infile,var,outfile){
  library(stringr)
  var.decile=str_c('var','.decile')

infile%>%
  mutate(var.decile = cut(var, 
                             breaks= quantile(var, 
                                              probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                             include.lowest= TRUE, labels= c(1:10)))%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  group_by(var.decile)%>%
  summarise(min(var), max(var), Observations=n(),Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100),
  )-> outfile
return(outfile)
}

char_tab(telco,InternetService,InternetService_Tab)
num_tab(telco,tenure,InternetService_Tab)