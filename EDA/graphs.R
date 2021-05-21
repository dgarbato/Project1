ggplot(data = internet_service_tab,aes(x = InternetService,y=Index))  + geom_col() +geom_text(aes(label=Index),vjust=1.5,colour='white')
#                             

ggplot(data = tenure_Tab,aes(x = factor(tenure.decile),y=Index))  + geom_col()
#                             

ggplot(data = MonthlyCharges_Tab,aes(x = factor(MonthlyCharges.decile),y=Index))  + geom_col()
#                             

ggplot(data = TotalCharges_Tab,aes(x = factor(TotalCharges.decile),y=Index))  + geom_col()

telco_tenure = telco%>%
  mutate(tenure.decile = cut(tenure, 
                             breaks= quantile(tenure,  
                                              probs= seq(0, 1, by= 0.1)),
                             include.lowest= TRUE, labels= c(1:10)))%>%
  mutate(Churn2=ifelse(Churn=='Yes',1,0))%>%
  filter(gender == 'Male') %>% 
  # filter based on widget values
  # Some widgets may be:
  # - Gender
  # - Senior Citizen
  # - Partner
  group_by(tenure.decile)%>%
  summarise(min(tenure), max(tenure), Observations=n(), Churn_Rate=mean(Churn2),
            Index=round(mean(Churn2)/0.2653699 * 100)
  )

ggplot(data = telco_tenure, aes(x = factor(tenure.decile),y=Index))  + geom_col()


ggplot(data = internet_service_tab, aes(x = InternetService,y=Index)) + geom_col(fill='lightblue',color='black')  + 
  ggtitle("Internet Service") + xlab("") + ylab("Index to Overall Churn Rate")

g + geom_density(aes(color = cut), binwidth = 0.2)

ggplot(data=telco,aes(x=))