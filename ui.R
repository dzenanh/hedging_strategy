require(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
require(shinyjs)
library(xts)
library(dygraphs)

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"
ui <- dashboardPage(
  dashboardHeader(title = "OntoREA© Prototype"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Forward Pricing",
      tabName = "forwardpricing",
      icon = icon("balance-scale")
    ),
    menuItem(
      "Option Pricing",
      tabName = "optionpricing",
      badgeColor = "red",
      badgeLabel = "help!",
      icon = icon("road")
    ),
    menuItem("Table Explorer",
             tabName = "tableexplorer",
             icon = icon("gear"))
  )),
  
  dashboardBody(
    #necessary for remote box-collapsing
    useShinyjs(),
    extendShinyjs(text = jscode),
    
    tabItems(
      tabItem(
        tabName = "forwardpricing",
        h2("Forward Pricing"),
        fluidRow(
          box(
            title = "Initial Pricing",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            actionButton("initialReadFile", "Load initial values"),
            dateInput(
              "initialPricingDate",
              "Initial pricing date",
              value = "2020-01-01",
              min = "2020-01-01"
            ),
            dateInput(
              "initialMaturityDate",
              "Maturity date",
              value = "2021-01-01",
              min = "2020-01-01"
            ),
            sliderInput("initialStockPrice", "Initial stock price:", 0, 250, 0),
            sliderInput("initialInterestRate", "Interest rate:", 0, 100, 0),
            actionButton("initialSaveFile", "Save initial values") #to ini.rds (all values) and database (interest rate)
          ),
          box(
            id = "box_Do",
            title = "First Step",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            #  title = "Subsequent Pricings",width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
            textInput(
              "subsequentPricingStock_ID",
              "Stock ID",
              value = 1,
              width = 150
            ),
            textInput(
              "subsequentPricingStock_Derivative",
              "Stock Derivative ID",
              value = 1,
              width = 150
            ),
            dateInput(
              "subsequentPricingDate",
              "Actual pricing date",
              value = "2020-01-01",
              min = "2020-01-01"
            ),
            sliderInput("subsequentStockPrice", "Actual stock price:", 0, 250, 0),
            sliderInput(
              "subsequentPricingVolatility",
              "Actual stock volatility",
              0,
              100,
              0
            ),
            actionButton("addSubsequentPricingObservation", "Add Observation!")
            #actionButton("button_Do", "Do")
          ),
          
          box(
            id = "box_Plan",
            title = "Second Step",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            actionButton("button_Plan", "Plan")
          ),
          
          box(
            id = "box_Check",
            title = "Third Step",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            actionButton("button_Check", "Check")
          ),
          
          box(
            id = "box_Act",
            title = "Fourth Step",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            actionButton("button_Act", "Act")
          ),
          
          box(
            title = "Timeline",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            dygraphOutput("timeline", height = 250)
          )
        )
        
      ),
      
      tabItem(tabName = "optionpricing",
              h2("Option Pricing")),
      
      tabItem(
        tabName = "tableexplorer",
        h2("Table Explorer"),
        fluidRow(
          box(
            title = "Database Control",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("clearDB", "Clear all database tables")
          ),
          
          box(
            title = "Table: Stock_Information",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Stock_Information", "Load from database"),
            dataTableOutput('table_Stock_Information')
          ),
          
          box(
            title = "Table: Interest_Rate",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Interest_Rate", "Load from database"),
            dataTableOutput('table_Interest_Rate')
          )
          ,
          
          box(
            title = "Table: Asset",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Asset", "Load from database"),
            dataTableOutput('table_Asset')
          ),
          
          box(
            title = "Table: Liability",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Liability", "Load from database"),
            dataTableOutput('table_Liability')
          ),
          
          box(
            title = "Table: Off_Balance",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Off_Balance", "Load from database"),
            dataTableOutput('table_Off_Balance')
          )
        )
      )
    )
  )
)
