library(shiny)
library(shinydashboard)
library(shinyjs)
#library(ggplot2)
library(RSQLite)
library(xts)
library(dygraphs)

#necessary for remote box-collapsing
jscode <- " 
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"
db <- dbConnect(SQLite(), "database.sqlite")


ui <- dashboardPage(
  
  dashboardHeader(
    title = "OntoREA© Prototype"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forward Pricing", tabName = "forwardpricing", icon = icon("balance-scale")),
      menuItem("Option Pricing", tabName = "optionpricing", badgeColor = "red", badgeLabel = "help!", icon = icon("road")),
      menuItem("Table Explorer", tabName = "tableexplorer", icon = icon("gear"))
    )
  ),
  
  dashboardBody(
    useShinyjs(), #necessary for remote box-collapsing
    extendShinyjs(text = jscode),#necessary for remote box-collapsing
    
    tabItems(
      # First tab content
      tabItem(tabName = "forwardpricing",
              h2("Forward Pricing"),
              fluidRow(
                box(
                  id = "box_Do", title = "First Step", width = 3, align="center", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  # 
                  #                #   title = "Subsequent Pricings",width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  #                   textInput("subsequentPricingStock_ID", "Stock ID", value = 1, width = 150),
                  #                   textInput("subsequentPricingStock_Derivative", "Stock Derivative ID", value = 1, width = 150),
                  #                   dateInput("subsequentPricingDate", "Actual pricing date", value= "2020-01-01", min = "2020-01-01"),
                  #                   sliderInput("subsequentStockPrice", "Actual stock price:", 0, 250, 0),
                  #                 #  sliderInput("subsequentPricingVolatility", "Actual stock volatility", 0, 100, 0),
                  #                   actionButton("addSubsequentPricingObservation", "Add Observation!"),
                  actionButton("button_Do", "Do")
                ),
                
                box(
                  id = "box_Plan", title = "Second Step", width = 3, align="center", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  actionButton("button_Plan", "Plan")
                ),
                
                box(
                  id = "box_Check", title = "Third Step", width = 3, align="center", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  actionButton("button_Check", "Check")
                ),
                
                box(
                  id = "box_Act", title = "Fourth Step", width = 3, align="center", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  actionButton("button_Act", "Act")
                ),
                box(
                  title = "Clear DB",width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  actionButton("clearStock_InformationDB", "Clear DB")
                  
                ),
                
                box(
                  title = "Initial Pricing",width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  
                  dateInput("initialPricingDate", "Initial pricing date", value= "2020-01-01", min = "2020-01-01"),
                  dateInput("initialMaturityDate", "Maturity date", value= "2021-01-01", min = "2020-01-01"),
                  
                  sliderInput("initialStockPrice", "Initial stock price:", 0, 250, 0),
                  sliderInput("initialInterestRate", "Interest rate:", 0, 100, 0),
                  
                  actionButton("initialReadFile", "Load from ini.rds"),
                  actionButton("initialSaveFile", "Save to ini.rds")
                ),
                
                box(
                  title = "Subsequent Pricings",width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  textInput("subsequentPricingStock_ID", "Stock ID", value = 1, width = 150),
                  textInput("subsequentPricingStock_Derivative", "Stock Derivative ID", value = 1, width = 150),
                  dateInput("subsequentPricingDate", "Actual pricing date", value= "2020-01-01", min = "2020-01-01"),
                  sliderInput("subsequentStockPrice", "Actual stock price:", 0, 250, 0),
                  sliderInput("subsequentPricingVolatility", "Actual stock volatility", 0, 100, 0),
                  actionButton("addSubsequentPricingObservation", "Add Observation!")
                  
                ),
                
                box(
                  title = "Timeline",width = 12, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  dygraphOutput("timeline", height = 250))
              )
              
      ),
      # Second tab content
      tabItem(tabName = "optionpricing",
              h2("Option Pricing")
      ),
      
      # third tab content
      tabItem(tabName = "tableexplorer",
              h2("Table Explorer"),
              
              fluidRow(
                box(
                  title = "Table: Stock_Information", width = 12, align="center", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  actionButton("load_table_Stock_Information", "Load from database"),
                  dataTableOutput('table_Stock_Information')
                ),
                
                box(
                  title = "Table: Liability", width = 3, align="center", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  actionButton("load_table_Liability", "Load from database"),
                  dataTableOutput('table_Liability')
                )
                
              )
      )
    )
  )
)




server <- function(input, output, session) {
  
  observeEvent(input$button_Do, {
    
    js$collapse("box_Do")
    js$collapse("box_Plan")
  })
  
  observeEvent(input$button_Plan, {
    js$collapse("box_Plan")
    js$collapse("box_Check")
  })
  
  observeEvent(input$button_Check, {
    js$collapse("box_Check")
    js$collapse("box_Act")
  })
  
  observeEvent(input$button_Act, {
    js$collapse("box_Act")
    js$collapse("box_Do")
  })
  
  v <- reactiveValues(doCalcAndPlot = FALSE) #recalc and redraw
  
  observeEvent(input$addSubsequentPricingObservation, {
    temp_db_stock_information <- cbind.data.frame(input$subsequentPricingStock_ID,input$subsequentPricingStock_Derivative,input$subsequentStockPrice,input$subsequentPricingVolatility,as.character(input$subsequentPricingDate))
    names(temp_db_stock_information) <- c("Stock_ID", "Stock_Derivative", "Stock_Price", "Stock_Volatility", "Pricing_Date") # set header to df
    dbWriteTable(db, "Stock_Information", temp_db_stock_information, append = TRUE)
    updateDateInput(session, "subsequentPricingDate",value = isolate(input$subsequentPricingDate+1)
    )
    
    v$doCalcAndPlot <- input$addSubsequentPricingObservation
  })
  
  
  observeEvent(input$clearStock_InformationDB, {
    
    dbSendStatement(db, "DELETE from Stock_Information")
    v$doCalcAndPlot <- input$clearStock_InformationDB
    
  })
  
  output$timeline <- renderDygraph({
    if (v$doCalcAndPlot == FALSE) return()
    isolate({
      temp_db_draw <- dbReadTable(db, "Stock_Information")
      temp_db_draw$Pricing_Date <- as.Date(as.POSIXct(temp_db_draw$Pricing_Date))
      
      #CALC
      #Wiki says that the gregorian calender has 365.2425 days... 365.2425/7=52.1775
      #as.numeric(difftime(as.Date("2003-04-05"), as.Date("2001-01-01"), unit="weeks"))/52.1775    
      temp_db_draw$TtM <- as.numeric(difftime(as.Date(isolate(input$initialMaturityDate)), as.Date(temp_db_draw$Pricing_Date), unit="weeks"))/52.1775
      
      temp_db_draw$Interest_Rate <- input$initialInterestRate / 100
      
      temp_db_draw$Interest_Rate_Cont <- log(1+temp_db_draw$Interest_Rate)
      temp_db_draw$F_Price <- input$initialStockPrice*(1+input$initialInterestRate/100)^(as.numeric(difftime(as.Date(input$initialMaturityDate), as.Date(input$initialPricingDate), unit="weeks"))/52.1775)
      temp_db_draw$Liability <- -temp_db_draw$F_Price*exp(-temp_db_draw$Interest_Rate_Cont*temp_db_draw$TtM)
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      temp_db_draw$'Forward Value' <- temp_db_draw$Liability + temp_db_draw$Stock_Price
      #Compose XTS
      
      
      temp_xts_draw <- xts(x = temp_db_draw[,c("Asset", "Liability", "Forward Value")], order.by=temp_db_draw[,6])
      
      #Plot XTS
      dygraph(temp_xts_draw) %>%
        dyRangeSelector()
    })
  })
  
  observeEvent(input$initialReadFile, {
    forwardMasterData <- readRDS("forwardIni.rds")
    updateDateInput(session, "initialPricingDate", value = isolate(forwardMasterData$initialPricingDate))
    updateDateInput(session, "initialMaturityDate", value = isolate(forwardMasterData$initialMaturityDate))
    updateSliderInput(session, "initialStockPrice", value = isolate(forwardMasterData$initialStockPrice))
    updateSliderInput(session, "initialInterestRate", value = isolate(forwardMasterData$initialInterestRate))
  })
  
  observeEvent(input$initialSaveFile, {
    isolate({
      forwardMasterData  <- list (initialPricingDate = input$initialPricingDate,
                                  initialMaturityDate = input$initialMaturityDate,
                                  initialStockPrice = input$initialStockPrice,
                                  initialInterestRate = input$initialInterestRate)
    })
    saveRDS(forwardMasterData, "forwardIni.rds")
  })
  
  
  observeEvent(input$load_table_Stock_Information,
               
               output$table_Stock_Information <- renderDataTable({
                 dbReadTable(db, "Stock_Information")
               }
               )
               
  )
}

shinyApp(ui, server)