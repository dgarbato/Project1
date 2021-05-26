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



char_tab = function(infile,var){
  infile%>%
    mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
    group_by_at(var)%>%
    summarise(Observations=n(),Percent_Total=round(n()/nrow(telco)*100,2), Churn_Rate=mean(Churn2),
              Index=round(mean(Churn2)/0.2653699 * 100))->outfile
  
  return(outfile)
}


num_tab=function(var){
  telco%>%
    mutate(.,Decile =  cut(var, 
                                     breaks= quantile(var, 
                                                      probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                     include.lowest= TRUE, labels= c(1:10)))%>%
    mutate(.,Churn2=ifelse(Churn=='Yes',1,0))%>%
    group_by_at(Decile)%>%
    summarise(Minimum=min(var), Maximum=max(var), Observations=n(),Churn_Rate=mean(Churn2),
              Index=round(mean(Churn2)/0.2653699 * 100),
    )-> outfile
  return(outfile)
  
}

num_tab(TotalCharges) ->tot_charges_tabs