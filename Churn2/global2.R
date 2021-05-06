library(shiny)
library(RColorBrewer)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(shinydashboard)

telco2=read_csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

telco2%>%
  mutate(senior=ifelse(SeniorCitizen == 1,'Yes','No'))->telco







#test function
# char_tab2=function(var,gender,senior){
#   
#   if (gender == 'All' && senior == 'All'){
#     telco %>%
#       mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
#       group_by_at(var)%>%
#       summarise(Observations=n(), Churn_Rate=mean(Churn2),
#                 Index=round(mean(Churn2)/0.2653699 * 100)
#       ) } else {NA}
# }

#function used
char_tab3=function(var,sex,senior){
  
  if (sex == 'All' && senior == 'All'){
    telco %>%
      mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
      group_by_at(var)%>%
      summarise(Observations=n(), Churn_Rate=mean(Churn2),
                Index=round(mean(Churn2)/0.2653699 * 100)
      ) }#close if
  
  else if (sex  == 'All' && senior != 'All'){
    telco %>%
      mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
      filter(SeniorCitizen == senior) %>% 
      group_by_at(var) %>%
      summarise(Observations=n(), Churn_Rate=mean(Churn2),Index=round(mean(Churn2)/0.2653699 * 100))
  }#close else if
  else if (sex != 'All' && senior == 'All'){
    telco %>%
      mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
      filter(gender == sex) %>% 
      group_by_at(var) %>%
      summarise(Observations=n(), Churn_Rate=mean(Churn2),Index=round(mean(Churn2)/0.2653699 * 100))
  }#close else if
  else{
    telco %>%
      mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
      filter(gender == sex, SeniorCitizen == senior) %>% 
      group_by_at(var) %>%
      summarise(Observations=n(), Churn_Rate=mean(Churn2),Index=round(mean(Churn2)/0.2653699 * 100))
  }
}
