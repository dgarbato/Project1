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
        summarise(Minimum=min(tenure), Maximum=max(tenure), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
                  Index=round(mean(Churn2)/0.2653699 * 100,0)
        )
    } # Close if
    
    else if (input$gender == 'All' && input$senior != 'All'){
      telco %>%
        mutate(tenure.decile = cut(tenure, 
                                   breaks= quantile(tenure, 
                                                    probs= seq(0, 1, by= 0.1)),
                                   include.lowest= TRUE, labels= c(1:10))) %>%
        mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
        filter(seniorcitz == input$senior) %>% 
        group_by(tenure.decile) %>%
        summarise(Minimum=min(tenure), Maximum=max(tenure), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
                  Index=round(mean(Churn2)/0.2653699 * 100,0)
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
        summarise(Minimum=min(tenure), Maximum=max(tenure), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
                  Index=round(mean(Churn2)/0.2653699 * 100,0)
        )
    } # Close else if
    
    else{
      telco %>%
        mutate(tenure.decile = cut(tenure, 
                                   breaks= quantile(tenure, 
                                                    probs= seq(0, 1, by= 0.1)),
                                   include.lowest= TRUE, labels= c(1:10))) %>%
        mutate(Churn2=ifelse(Churn=='Yes',1,0)) %>%
        filter(gender == input$gender, seniorcitz == input$senior) %>% 
        group_by(tenure.decile) %>%
        summarise(Minimum=min(tenure), Maximum=max(tenure), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
                  Index=round(mean(Churn2)/0.2653699 * 100,0)
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
        summarise(Minimum=min(TotalCharges,na.rm=TRUE), Maximum=max(TotalCharges,na.rm=TRUE), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
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
        filter(seniorcitz == input$senior) %>% 
        group_by(TotalCharges.decile) %>%
        summarise(Minimum=min(TotalCharges), Maximum=max(TotalCharges), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
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
        summarise(Minimum=min(TotalCharges,na.rm=TRUE ), Maximum=max(TotalCharges,na.rm=TRUE), Observations=n(),Churn_Rate=round(mean(Churn2)*100,2),
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
        filter(gender == input$gender, seniorcitz == input$senior) %>% 
        group_by(TotalCharges.decile) %>%
        summarise(Minimum=min(TotalCharges,na.rm=TRUE), Maximum=max(TotalCharges,na.rm=TRUE), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
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
        summarise(Minimum=min(MonthlyCharges,na.rm=TRUE), Maximum=max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
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
        filter(seniorcitz == input$senior) %>% 
        group_by(MonthlyCharges.decile) %>%
        summarise(Minimum=min(MonthlyCharges,na.rm=TRUE), Maximum=max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
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
        summarise(Minimum=min(MonthlyCharges,na.rm=TRUE ), Maximum=max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
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
        filter(gender == input$gender, seniorcitz == input$senior) %>% 
        group_by(MonthlyCharges.decile) %>%
        summarise(Minimum=min(MonthlyCharges,na.rm=TRUE), Maximum=max(MonthlyCharges,na.rm=TRUE), Observations=n(), Churn_Rate=round(mean(Churn2)*100,2),
                  Index=round(mean(Churn2)/0.2653699 * 100)
        )
    } # Close else
    
  }) # Close reactive
  
  
  ##############################################################
  # Character Variable Reactives created with char_tab3 function
  ##############################################################
  # 
  # #next reactive
  # Online_security_tab_reactive <- reactive({char_tab3('OnlineSecurity',input$gender,input$senior)
  # }) # Close reactive
  
  #next reactive
  InternetService_tab_reactive <- reactive({char_tab3('InternetService',input$gender,input$senior)
  }) # Close reactive
  
  # #next reactive
  # OnlineBackup_tab_reactive <- reactive({char_tab3('OnlineBackup',input$gender,input$senior)
  # }) # Close reactive    
  # 
  # Online_security_tab_reactive <- reactive({char_tab3('OnlineSecurity',input$gender,input$senior)
  # }) # Close reactive
  
  
  ##### REACTIVES ADDED 5/9/2021
  
  #next reactive
  PaymentMethod_tab_reactive <- reactive({char_tab3('PaymentMethod',input$gender,input$senior)
  }) # Close reactive
  
  #next reactive
  Contract_tab_reactive <- reactive({char_tab3('Contract',input$gender,input$senior)
  }) # Close reactive
  
  
  #next reactive
  MultipleLines_tab_reactive <- reactive({char_tab3('MultipleLines',input$gender,input$senior)
  }) # Close reactive
  
  
  #next reactive
  PhoneService_tab_reactive <- reactive({char_tab3('PhoneService',input$gender,input$senior)
  }) # Close reactive 
  
  
  #next reactive
  StreamingMovies_tab_reactive <- reactive({char_tab3('StreamingMovies',input$gender,input$senior)
  }) # Close reactive 
  
  
  
  #function test
  #output$test <-renderTable({
  #   char_tab3('OnlineSecurity',input$gender,input$senior)})
  
  
  #############################
  # Numeric Variable Plots
  #############################
  
  output$tenure_tab_plot <- renderPlot({
    
    ggplot(data = tenure_tab_reactive(), aes(x = factor(tenure.decile),y=Index)) + geom_col(fill='lightblue',color='black') + 
      ggtitle("Customer Tenure") +  xlab("Deciles") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
    
  }) # Close renderPlot
  
  # Next plot
  output$TotalCharges_tab_plot <- renderPlot({
    
    ggplot(data = TotalCharges_tab_reactive(), aes(x = factor(TotalCharges.decile),y=Index)) + geom_col(fill='lightblue',color='black') + 
      ggtitle("Total Charges") +  xlab("Deciles") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
  })# Close renderPlot
  
  # Next plot
  output$MonthlyCharges_tab_plot <- renderPlot({
    
    ggplot(data = MonthlyCharges_tab_reactive(), aes(x = factor(MonthlyCharges.decile),y=Index)) + geom_col(fill='lightblue',color='black') + 
      ggtitle("Monthly Charges") +  xlab("Deciles") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
  })# Close renderPlot
  
  #############################
  # Character Variable Plots
  #############################
  
  # Next plot
  # output$Online_security_tab_plot <- renderPlot({
  #   
  #   ggplot(data = Online_security_tab_reactive(), aes(x = OnlineSecurity,y=Index)) + geom_col(fill='lightblue',color='black') +
  #     ggtitle("Online security") + xlab("") + ylab("Index to Overall Churn Rate") + coord_flip()
  #   
  # })# Close renderPlot
  # 
  
  # Next plot
  output$InternetService_tab_plot <- renderPlot({
    
    ggplot(data = InternetService_tab_reactive(), aes(x = InternetService,y=Index)) + geom_col(fill='lightblue',color='black')  + 
      ggtitle("Internet Service") + xlab("") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black') 
    
    
    
    
  })# Close renderPlot
  
  # Next plot
  # output$OnlineBackup_tab_plot <- renderPlot({
  #   
  #   ggplot(data = OnlineBackup_tab_reactive(), aes(x = OnlineBackup,y=Index)) + geom_col(fill='lightblue',color='black')  + 
  #     ggtitle("Online Backup") + xlab("") + ylab("Index to Overall Churn Rate")
    
    
  # })# Close renderPlot
  
#Next plot
  output$PaymentMethod_tab_plot <- renderPlot({
    
    ggplot(data = PaymentMethod_tab_reactive(), aes(x = PaymentMethod,y=Index)) + geom_col(fill='lightblue',color='black')  + 
      ggtitle("Payment Method") + xlab("") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
  })# Close renderPlot
  
  
  #Next plot
  output$Contract_tab_plot <- renderPlot({
    
    ggplot(data = Contract_tab_reactive(), aes(x = Contract,y=Index)) + geom_col(fill='lightblue',color='black')  + 
      ggtitle("Contract") + xlab("") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
  })# Close renderPlot
  
  
  #Next plot
  output$MultipleLines_tab_plot <- renderPlot({
    
    ggplot(data = MultipleLines_tab_reactive(), aes(x = MultipleLines,y=Index)) + geom_col(fill='lightblue',color='black')  + 
      ggtitle("Multiple Lines") + xlab("") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
  })# Close renderPlot
  
  
  
  #Next plot
  output$PhoneService_tab_plot <- renderPlot({
    
    ggplot(data = PhoneService_tab_reactive(), aes(x = PhoneService,y=Index)) + geom_col(fill='lightblue',color='black')  + 
      ggtitle("PhoneService") + xlab("") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
  })# Close renderPlot
  

  #Next plot
  # output$StreamingMovies_tab_plot <- renderPlot({
  #   
  #   ggplot(data = StreamingMovies_tab_reactive(), aes(x = StreamingMovies,y=Index)) + geom_col(fill='lightblue',color='black')  + 
  #     ggtitle("StreamingMovies") + xlab("") + ylab("Index to Overall Churn Rate") +geom_text(aes(label=Index),vjust=1.5,colour='black')
    
  # })# Close renderPlot
  
  ############################ TABLES ######################################################################################
  #Numeric variable tables
  
  #Next table
  output$table_tenure <- renderTable(tenure_tab_reactive()  )
  
  #Next table
  output$table_TotalCharges <-renderTable(TotalCharges_tab_reactive())
  
  #Next table
  output$table_MonthlyCharges <-renderTable(MonthlyCharges_tab_reactive())
  
  
  #character variable tables
  
  # # #Next table
  #  output$table_InternetService <-  renderTable(InternetService_tab_reactive())
  # # 
  # #Next table
  # output$table_PaymentMethod <- renderTable(PaymentMethod_tab_reactive())
  # 
  # #Next table
  # output$table_Contract <- renderTable(Contract_tab_reactive())
  # 
  # #Next table
  # output$table_MultipleLines <- renderTable(MultipleLines_tab_reactive())
  # 
  # #Next table
  # output$table_PhoneService <-  renderTable(PhoneService_tab_reactive())


  
} # Close function