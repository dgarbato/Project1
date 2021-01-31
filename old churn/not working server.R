library(shiny)
function(input, output) {
  
  
  #############################
  #Numeric Variable Reactives
  #############################
  
  
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
  
  # Next reactive
  MonthlyCharges_tab_reactive <- reactive({
    
    if (input$gender == 'All' && input$senior == 'All'){
      telco %>%
        mutate(MonthlyCharges.decile = cut(MonthlyCharges , 
                                           breaks= quantile(MonthlyCharges , 
                                                            probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
        mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
        group_by(MonthlyCharges.decile) %>%
        summarise(min(MonthlyCharges,na.rm=TRUE), max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                  Index=round(mean(Churn2)/0.2653699 * 100)
        )
    } # Close if
    
    else if (input$gender == 'All' && input$senior != 'All'){
      telco %>%
        mutate(MonthlyCharges.decile = cut(MonthlyCharges , 
                                           breaks= quantile(MonthlyCharges , 
                                                            probs= seq(0, 1, by= 0.1,na.rm=TRUE),na.rm=TRUE),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
        mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
        filter(SeniorCitizen == input$senior) %>% 
        group_by(MonthlyCharges.decile) %>%
        summarise(min(MonthlyCharges,na.rm=TRUE), max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                  Index=round(mean(Churn2)/0.2653699 * 100)
        )
    } # Close else if
    
    else if (input$gender != 'All' && input$senior == 'All'){
      telco %>%
        mutate(MonthlyCharges.decile = cut(MonthlyCharges , 
                                           breaks= quantile(MonthlyCharges , 
                                                            probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
        mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
        filter(gender == input$gender) %>% 
        group_by(MonthlyCharges.decile) %>%
        summarise(min(MonthlyCharges,na.rm=TRUE ), max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                  Index=round(mean(Churn2)/0.2653699 * 100)
        )
    } # Close else if
    
    else{
      telco %>%
        mutate(MonthlyCharges.decile = cut(MonthlyCharges , 
                                           breaks= quantile(MonthlyCharges , 
                                                            probs= seq(0, 1, by= 0.1),na.rm=TRUE),
                                           include.lowest= TRUE, labels= c(1:10))) %>%
        mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
        filter(gender == input$gender, SeniorCitizen == input$senior) %>% 
        group_by(MonthlyCharges.decile) %>%
        summarise(min(MonthlyCharges,na.rm=TRUE), max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=mean(Churn2,na.rm=TRUE),
                  Index=round(mean(Churn2)/0.2653699 * 100)
        )
    } # Close else
    
  }) # Close reactive
  
  
  ##############################################################
  # Character Variable Reactives created with char_tab3 function
  ##############################################################
  
  #next reactive
  Online_TechSupport_tab_reactive <- reactive({char_tab3('TechSupport',input$gender,input$senior)
  }) # Close reactive
  
  #next reactive
  InternetService_tab_reactive <- reactive({char_tab3('InternetService',input$gender,input$senior)
  }) # Close reactive
  
  #next reactive
  OnlineBackup_tab_reactive <- reactive({char_tab3('OnlineBackup',input$gender,input$senior)
  }) # Close reactive    
  
  Online_security_tab_reactive <- reactive({char_tab3('OnlineSecurity',input$gender,input$senior)
  }) # Close reactive
  
  
  
  
  #function test
  #output$test <-renderTable({
  #   char_tab3('OnlineSecurity',input$gender,input$senior)})
  
  
  #############################
  # Numeric Variable Plots
  #############################
  
  output$tenure_tab_plot <- renderPlot({
    
    ggplot(data = tenure_tab_reactive(), aes(x = factor(tenure.decile),y=Index)) + geom_col(fill='lightblue',color='black') + 
      ggtitle("Customer Tenure") +  xlab("Deciles") 
    
    
  }) # Close renderPlot
  
  # Next plot
  output$TotalCharges_tab_plot <- renderPlot({
    
    ggplot(data = TotalCharges_tab_reactive(), aes(x = factor(TotalCharges.decile),y=Index)) + geom_col(fill='lightblue',color='black') + 
      ggtitle("Total Charges") +  xlab("Deciles") 
    
  })# Close renderPlot
  
  # Next plot
  output$MonthlyCharges_tab_plot <- renderPlot({
    
    ggplot(data = MonthlyCharges_tab_reactive(), aes(x = factor(MonthlyCharges.decile),y=Index)) + geom_col(fill='lightblue',color='black') + 
      ggtitle("Monthly Charges") +  xlab("Deciles") 
    
  })# Close renderPlot
  
  #############################
  # Character Variable Plots
  #############################
  
  # Next plot
  # Next plot
  output$TechSupport_security_tab_plot <- renderPlot({
    
    ggplot(data = TechSupport_tab_reactive(), aes(x = TechSupport,y=Index)) + geom_col(fill='lightblue',color='black') +
      ggtitle("TechSupport")
    
  })# Close renderPlot
  
  # Next plot
  output$InternetService_tab_plot <- renderPlot({
    
    ggplot(data = InternetService_tab_reactive(), aes(x = InternetService,y=Index)) + geom_col(fill='lightblue',color='black')  + 
      ggtitle("Internet Service") 
    
    
  })# Close renderPlot
  
  # Next plot
  output$OnlineBackup_tab_plot <- renderPlot({
    
    ggplot(data = OnlineBackup_tab_reactive(), aes(x = OnlineBackup,y=Index)) + geom_col(fill='lightblue',color='black')  + 
      ggtitle("Internet Service") 
    
    
  })# Close renderPlot
  
  
  
  
  
  
  
  
} # Close function