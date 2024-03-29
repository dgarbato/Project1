ui <- dashboardPage(
  dashboardHeader(title = "Factors Driving Churn At A Telecommunications Company
"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",tabName="Tab0"),
      menuItem("Numeric Factors",tabName="Tab1"),  
      menuItem("Important Categorical Factors",tabName="Tab2"),
      menuItem("Box Plots Of Monthly Charges",tabName = "Tab2a"),
      menuItem("Unimportant Categorical Factors",tabName="Tab3"),
      menuItem("Conclustion And Future Work",tabName = "Tab4"),
      selectizeInput(inputId = "gender",
                     label = "Filter by Gender:",
                     choices = c("All", unique(telco[, 'gender'])$gender)),
      selectizeInput(inputId = "senior",
                     label = "Filter by Senior Citizen:",
                     choices = c("All", "Yes", "No"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Tab0",
              fluidRow(
                column(width=6,offset=3,
                       
                       h1(strong("Customer Churn Analysis")),
                       
                       h2(strong("Data Facts")),
                       
                       h4("This data is from Kaggle.  It has customer churn information about a fictitious telecommunications company called Telco
                       that provides phone, internet service, streaming movies, streaming TV and online security.  There are 7,043 customers."),
                       
                       h4("A customer who left within the last month is considered to have churned."),
                       
                       h4("There is customer tenure (in months), payment information, information about various services and demographic information in the data."),
                       
                       
                       h2(strong("Objective: To identify factors driving customer churn.")),
                       
                       h3("Why it's important"),
                
                       h4("If customers who are likely to churn are identified, marketers can take action to incentivize them in the form of speicial promotions
                       and improve customer retention."),
                       
                       h4("It is a lot more expensive to acquire new customers than it is to invest in retaining current ones."),
                       
                       h2(strong("Methodology")),
                       
                       h3("Numeric Variables"),
                       
                       h4("The numeric variables were broken out into deciles for analysis."),
              
                       h4("A decile is comprised of ten percentiles. For example, if a studen't test scores are in the 90th percentile, that means that he/she scored in the top 10%.  
                       If would be equivalent to say that his/her score is in the tenth decile as that also means the top ten percent"),
                       
                       h4("As the values of the numeric variable increase the decile values increase."),
                       
                       h4("The churn rate within each decile was calculated and indexed to the overall churn rate of 27% using the
                          following calculations:"   
                          ),
                       h4(strong("Overall Churn Rate = (Number Of Churns)/(Total Number Of Customers)")),
                       
                       h4(strong("Index = (Churn Rate Within Each Decile)/(Overall Churn Rate) * 100")),
                       
                       h3("Character Variables"),
                       
                       h4("For the categorical variables, each category was indexed  to the overall churn rate of 27% using the following
                          calculation:
                          "),
                       
                       h4(strong("Index = (Churn Rate Per Category)/(Overall Churn Rate) * 100")),
                       
                       h4(em("Note: when the index of the Churn Rate Per Category is equivalent to the Overall Churn Rate the index = 100")),
                       
                       h4("Two drop down menus allow users to see if the relationship with churn varies by gender and senior citizen reapectively.")
                       
                       
                       )
                       
                       
                       
                       
                       
                       
                  
                )
              ),
              
      
      tabItem(tabName="Tab1",
              
              fluidRow(box(
                plotOutput("tenure_tab_plot"),width=4,offset=2,),
                box(tableOutput("table_tenure"),width=5,offset=1)
                ),
              
              fluidRow(box(
                plotOutput("MonthlyCharges_tab_plot"),width=4,offset=2),
                box(tableOutput("table_MonthlyCharges"),width=5,offset=1)
                ),
              
              fluidRow(box(plotOutput("TotalCharges_tab_plot"),width=4,offset=2),
                       box(tableOutput("table_TotalCharges"),width=5,offset=1)
                       
                       )
              ),
      tabItem(tabName="Tab2",
              fluidRow(
                
                box(width=4,offset=2,plotOutput("InternetService_tab_plot")),
                  
                box(width=4,offset=2,
                       h4("44% of customers have fiber optic internet service.  These customers need to be incentivized through special 
                          promotions to reduce churn. "))
                
                
                       ),
              fluidRow(
                box(plotOutput("PaymentMethod_tab_plot"),width=4,offset=2),
                box(width=4,offset=2, h4("34% of customers pay by electronic check.  These customers customers need to be encouraged to auto pay
                                   by credit card or bank transfer. In my professional experience analyzing churn data in another industry customers who 
                                            auto pay are much less likely to churn.")),
                
                       
                       ),
              fluidRow(
                box(plotOutput("Contract_tab_plot"),width=4,offset=2),
                box(width=4,offset=2, h4("55% of customers have a month-to-month contract.  These customers need to be offered better deals not only 
                because they comprise such a large percentage of customers but also higher monthly charges are associated with higher churn rates.")),
                
                                   
                
                       
                       )
              ),
      
      tabItem(tabName = "Tab2a",
              fluidRow(box(plotOutput("MonthlyCharges_InternetService_plot"))),
              fluidRow(box(plotOutput("MonthlyCharges_contract_plot"))),
              fluidRow(box(plotOutput("MonthlyCharges_PaymentMethod_plot")))
      ),
      
      
      tabItem(tabName="Tab3",
              fluidRow(box(plotOutput("MultipleLines_tab_plot"),width=4,offset=2)
                       
                       ),
              fluidRow(box(plotOutput("PhoneService_tab_plot"),width=4,offset=2)
                       )
              ),
      tabItem(tabName = "Tab4",
              fluidRow(
                column(width=6,
                       offset=3,
                       h1(strong("Conclusions")),
                       h4(" "),
                       h3(strong("Numeric Factors")),
                       
                      
                       h4("The newest customers are more likely to churn given the strong negative linear relationship between customer tenure
                          and churn. As customer tenure increases, the churn rate goes down "),
                       h4("Monthly has a positive relationship with churn.  Higher monthly charges are associated with higher churn rates "),
                       h4("Churn trends downward as total charges increase."),
                       h4(" "),
                       h3(strong("Categorical Factors" )),
                       h4("Fiber optic internet service, month-to-month contracts and payment by electronic check all have high churn rates."),
                       h4("Boxplots revealed a relationship between monthly charges and each of the above factors"  ),
                       h4("Fiber Optic internet service has much higher monthly charges than the other types of service"),
                       h4("Month-to-month contracts have higher montly charges and less spread than other contract types."),
                       h4(""),
                       
                       h3(strong("Next Steps")),
                       h4("Add drop down menus for all demographic categories."),
                       h4("")
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       )

              )
              
              
        
      )
      
    ))
)