#    http://shiny.rstudio.com/
function(input, output) {
    
    tenure_tab_reactive <- reactive({
        
        if (input$gender == 'All' && input$senior == 'All'){
            telco %>%
                mutate(tenure.decile = cut(tenure, 
                                           breaks= quantile(tenure, 
                                                            probs= seq(0, 1, by= 0.1)),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                group_by(tenure.decile) %>%
                summarise(min(tenure), max(tenure), Observations=n(), Churn_Rate=mean(Churn2),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close if
        
        else if (input$gender == 'All' && input$senior != 'All'){
            telco %>%
                mutate(tenure.decile = cut(tenure, 
                                           breaks= quantile(tenure, 
                                                            probs= seq(0, 1, by= 0.1)),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                filter(SeniorCitizen == input$senior) %>% 
                group_by(tenure.decile) %>%
                summarise(min(tenure), max(tenure), Observations=n(), Churn_Rate=mean(Churn2),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close else if
        
        else if (input$gender != 'All' && input$senior == 'All'){
            telco %>%
                mutate(tenure.decile = cut(tenure, 
                                           breaks= quantile(tenure, 
                                                            probs= seq(0, 1, by= 0.1)),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                filter(gender == input$gender) %>% 
                group_by(tenure.decile) %>%
                summarise(min(tenure), max(tenure), Observations=n(), Churn_Rate=mean(Churn2),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close else if
        
        else{
            telco %>%
                mutate(tenure.decile = cut(tenure, 
                                           breaks= quantile(tenure, 
                                                            probs= seq(0, 1, by= 0.1)),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                filter(gender == input$gender, SeniorCitizen == input$senior) %>% 
                group_by(tenure.decile) %>%
                summarise(min(tenure), max(tenure), Observations=n(), Churn_Rate=mean(Churn2),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close else
        
    }) # Close reactive
    
    # Next reactive
    TotalCharges_tab_reactive <- reactive({
        
        if (input$gender == 'All' && input$senior == 'All'){
            telco %>%
                mutate(TotalCharges.decile = cut(TotalCharges , 
                                                 breaks= quantile(TotalCharges , 
                                                                  probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                                 include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                group_by(TotalCharges.decile) %>%
                summarise(min(TotalCharges,na.rm=TRUE), max(TotalCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close if
        
        else if (input$gender == 'All' && input$senior != 'All'){
            telco %>%
                mutate(TotalCharges.decile = cut(TotalCharges , 
                                                 breaks= quantile(TotalCharges , 
                                                                  probs= seq(0, 1, by= 0.1,na.rm=TRUE),na.rm=TRUE),
                                                 include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                filter(SeniorCitizen == input$senior) %>% 
                group_by(TotalCharges.decile) %>%
                summarise(min(TotalCharges,na.rm=TRUE), max(TotalCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close else if
        
        else if (input$gender != 'All' && input$senior == 'All'){
            telco %>%
                mutate(TotalCharges.decile = cut(TotalCharges , 
                                                 breaks= quantile(TotalCharges , 
                                                                  probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                                 include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                filter(gender == input$gender) %>% 
                group_by(TotalCharges.decile) %>%
                summarise(min(TotalCharges,na.rm=TRUE ), max(TotalCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close else if
        
        else{
            telco %>%
                mutate(TotalCharges.decile = cut(TotalCharges , 
                                                 breaks= quantile(TotalCharges , 
                                                                  probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                                 include.lowest= TRUE, labels= c(1:10))) %>%
                mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
                filter(gender == input$gender, SeniorCitizen == input$senior) %>% 
                group_by(TotalCharges.decile) %>%
                summarise(min(TotalCharges,na.rm=TRUE), max(TotalCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                          Index=round(mean(Churn2)/0.2653699 * 100)
                )
        } # Close else
        
    }) # Close reactive
    
    #Plots
    ######
    
    output$tenure_tab_plot <- renderPlot({
        
        ggplot(data = tenure_tab_reactive(), aes(x = factor(tenure.decile),y=Index)) + geom_col()
        
    }) # Close renderPlot
    
    # Next plot
    output$TotalCharges_tab_plot <- renderPlot({
        
        ggplot(data = TotalCharges_tab_reactive(), aes(x = factor(TotalCharges.decile),y=Index)) + geom_col()
        
    })# Close renderPlot
    
} # Close function