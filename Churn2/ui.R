ui <- dashboardPage(
  dashboardHeader(title = "Factors Driving Churn At A Telecommunications Company
"),
  dashboardSidebar(
    sidebarMenu(
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