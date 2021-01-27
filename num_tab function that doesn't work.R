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