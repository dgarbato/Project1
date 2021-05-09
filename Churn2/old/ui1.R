library(shiny)
fluidPage(
  titlePanel("Factors Driving Churn At A Telecommunications Company"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "gender",
                     label = "Filter by Gender:",
                     choices = c("All", unique(telco[, 'gender'])$gender)),
            selectizeInput(inputId = "senior",
                     label = "Filter by Senior Citizen:",
                     choices = c("All", "Yes", "No"))
      
      
      
      
         ),
    mainPanel(plotOutput("tenure_tab_plot"),
              plotOutput("MonthlyCharges_tab_plot"),
              plotOutput("TotalCharges_tab_plot"),
              plotOutput("InternetService_tab_plot"),
              plotOutput("OnlineBackup_tab_plot"),
              plotOutput("Online_security_tab_plot")
              
              
              
              )
  )
)