library(tidyverse)
telco2=read_csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

#telco2$senior=ifelse(Telco2$SeniorCitizen==1),'Yes','No')

telco2%>%
  mutate(senior=ifelse(SeniorCitizen == 1,'Yes','No'))->telco


telco %>%
  mutate(tenure.decile = cut(tenure, 
                             breaks= quantile(tenure, 
                                              probs= seq(0, 1, by= 0.1)),
                             include.lowest= TRUE, labels= c(1:10))) %>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
  filter(.,SeniorCitizen == 1) #%>% 
  # # group_by(tenure.decile) %>%
  # summarise(min(tenure), max(tenure), Observations=n(), Churn_Rate=mean(Churn2),
  #           Index=round(mean(Churn2)/0.2653699 * 100)
  # )

unique(telco$SeniorCitizen)
c('All', 'Yes' = 1, 'No' = 0)  

