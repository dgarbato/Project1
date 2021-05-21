ui <- dashboardPage(
  dashboardHeader(title = "Factors Driving Churn At A Telecommunications Company
"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",tabName="Tab0"),
      menuItem("Numeric Factors",tabName="Tab1"),  #will add filtered churn rate to box
      menuItem("Important Categorical Factors",tabName="Tab2"),
      menuItem("Unimportant Categorical Factors",tabName="Tab3"),
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
                       
                       h4("This data comes to you from Kaggle.  It has customer churn information about a fictitious telecommunications company
                       that provides phone and internet service."),
                       
                       h4("A customer who left within the last month is considered to have churnned."),
                       
                       h4("There is customer tenure, payment information, information about various services and demographic information in the data."),
                          
                       h4("The objective of this analysis is to identify factors that drive churn."),
                
                       h4("If customers who are likely to churn are identified, marketers can take action to incentivize them in the form of speicial promotions etc.
                          It is a lot more expensive to acquire new customers than it is to invest in retaining current ones.  Offering special deals to at risk 
                          customers can improve customer retention."),
                       
                       h4("It is a lot more expensive to acquire new customers than it is to invest in retaining current ones.  Offering special deals to at risk 
                          customers can improve customer retention."),
                          
                       h4("The numeric variables were broken out into deciles for analysis."),
              
                       h4("A decile is comprised of ten percentiles.  If a studen't test scores are in the 90th percentile, that means that they scored in the top 10%.  If would be equivalent to say that they 
                          scored in the tenth decile.  As the values of a variable increase the decile value increases."),
                       
                       h4("The churn rate within each decile was calculated and indexed to the overall churn rate of 26.53% using the
                          following calculation:"   
                          ),
                       
                       h4("     Index=(churn rate within each decile)/(overall churn rate) * 100"),
                       
                       h4("For the categorical variables, each category was indexed  to the overall churn rate of 26.5% using the following
                          calculation:
                          "),
                       
                       h4("     Index = (Churn Rate Per Category)/(Overall Churn Rate) * 100"),
                       
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
                column(width=4,offset=2,
                       h4("44% of customers have fiber optic internet service.  These customers need to be incentivized through special 
                          promotions to reduce churn ")),
                box(plotOutput("InternetService_tab_plot"),width=4,offset=2)
                
                       ),
              fluidRow(
                column(width=4,offset=2, h4("34% of customers pay by electronic check.  These customers customers need to be encouraged to auto pay
                                   by credit card or bank transfer. Customers who auto pay in other industries are also less likely to churn.")),
                box(plotOutput("PaymentMethod_tab_plot"),width=4,offset=2)
                       
                       ),
              fluidRow(
                column(width=4,offset=2, h4("55% of customers have a month-to-month contract.  These customers need to be offered better deals not only 
                because they comprise such a large percentage of customers but also higher monthly charges are associated with higher churn rates.")),
                
                                   
                box(plotOutput("Contract_tab_plot"),width=4,offset=2)
                       
                       )
              ),
      tabItem(tabName="Tab3",
              fluidRow(box(plotOutput("MultipleLines_tab_plot"),width=6,)
                       
                       ),
              fluidRow(box(plotOutput("PhoneService_tab_plot"),width=6)
                       )
              )
      
    ))
)