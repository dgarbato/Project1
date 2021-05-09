ui <- dashboardPage(
  dashboardHeader(title = "Factors Driving Churn At A Telecommunications Company
"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("menuitem1",tabName="Tab1"),  #will add filtered churn rate to box
      menuItem("menuitem2",tabName="Tab2"),
      menuItem("menuitem3",tabName="Tab3"),
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
             plotOutput("tenure_tab_plot"),
             plotOutput("MonthlyCharges_tab_plot")),
    tabItem(tabName="Tab2",
             plotOutput("TotalCharges_tab_plot"),
             plotOutput("InternetService_tab_plot")),
    tabItem(tabName="Tab3",
             plotOutput("OnlineBackup_tab_plot"),
             plotOutput("Online_security_tab_plot"))
    
  ))
)