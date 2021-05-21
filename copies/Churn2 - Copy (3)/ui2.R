ui <- dashboardPage(
  dashboardHeader(title = "Factors Driving Churn At A Telecommunications Company
"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Numeric Factors",tabName="Tab1"),  #will add filtered churn rate to box
      menuItem("Important Categorical Factors",tabName="Tab2"),
      menuItem("Meaningless Categorical Factors",tabName="Tab3"),
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
                plotOutput("tenure_tab_plot"),width=8,height=300)),
              
              fluidRow(box(
                plotOutput("MonthlyCharges_tab_plot"),width=8,height=300)),
              
              fluidRow(box(plotOutput("TotalCharges_tab_plot"),width=8,height=300))
              ),
      tabItem(tabName="Tab2",
              fluidRow(box(plotOutput("InternetService_tab_plot"),width=8,height=300)),
              fluidRow(box(plotOutput("PaymentMethod_tab_plot"),width=8,height=300)),
              fluidRow(box(plotOutput("Contract_tab_plot"),width=8,height=300))
              ),
      tabItem(tabName="Tab3",
              fluidRow(box(plotOutput("MultipleLines_tab_plot"),width=8,height=300)),
              fluidRow(box(plotOutput("PhoneService_tab_plot"),width=8,height=300)),
              fluidRow(box(plotOutput("StreamingMovies_tab_plot"),width=8,height=300))
              )
      
    ))
)