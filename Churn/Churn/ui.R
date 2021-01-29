
library(shiny)

# Define UI for application that creates graphs that help the user understand what factors are driving churn for a 
#telecommunications company
shinyUI(
fluidPage(
    titlePanel("Factors Driving Churn At A Telecommunications Company"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "gender",
                           label = "Filter by Gender:",
                           choices = c("All", unique(telco[, 'gender'])$gender)
            ),
            selectizeInput(inputId = "senior",
                           label = "Filter by Senior Citizen:",
                           choices = c("All", "Yes", "No")
            )
        ),
        mainPanel(
            #fluidRow(tableOutput("test")),
            fluidRow(
               column(12, plotOutput("tenure_tab_plot")),
               column(12, plotOutput("TotalCharges_tab_plot")),
               column(12, plotOutput("MonthlyCharges_tab_plot")),
               column(12, plotOutput("Online_security_tab_plot"))
                # column(6, plotOutput("delay"))
            )
        )
    )
)
)
