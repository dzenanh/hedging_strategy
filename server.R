library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)

#necessary for remote box-collapsing
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

db <- dbConnect(SQLite(), "database.sqlite")
#db <- dbConnect(SQLite(), "database_forward.sqlite")



server <- function(input, output, session) {
  
  
  
  observeEvent(input$button_Do, {
    js$collapse("box_Do")
    js$collapse("box_Plan")
    
    
    
    
    
  })
  
  observeEvent(input$button_Plan, {
    js$collapse("box_Plan")
    js$collapse("box_Check")
  })
  
  #https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
  
  
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
    temp_db_stock_information <-
      cbind.data.frame(
        input$subsequentPricingStock_ID,
        input$subsequentPricingStock_Derivative,
        input$subsequentStockPrice,
        input$subsequentPricingVolatility,
        as.character(input$subsequentPricingDate)
      )
    names(temp_db_stock_information) <-
      c(
        "Stock_ID",
        "Stock_Derivative",
        "Stock_Price",
        "Stock_Volatility",
        "Pricing_Date"
      ) # set header to df
    dbWriteTable(db,
                 "Stock_Information",
                 temp_db_stock_information,
                 append = TRUE)
    updateDateInput(
      session,
      "subsequentPricingDate",
      value = isolate(input$subsequentPricingDate + 1),
      js$collapse("box_Do"),
      js$collapse("box_Plan")
      
    )
    
    v$doCalcAndPlot <- input$addSubsequentPricingObservation
  })
  
  output$timeline <- renderDygraph({
    if (v$doCalcAndPlot == FALSE)
      return()
    isolate({
      temp_db_draw <- dbReadTable(db, "Stock_Information")
      temp_db_draw$Pricing_Date <-
        as.Date(as.POSIXct(temp_db_draw$Pricing_Date))
      
      #CALC
      #Wiki says that the gregorian calender has 365.2425 days... 365.2425/7=52.1775
      #as.numeric(difftime(as.Date("2003-04-05"), as.Date("2001-01-01"), unit="weeks"))/52.1775
      #Composing temp_db_draw for
      temp_db_draw$TtM <-
        as.numeric(difftime(
          as.Date(isolate(input$initialMaturityDate)),
          as.Date(temp_db_draw$Pricing_Date),
          unit = "weeks"
        )) / 52.1775
      temp_db_draw$Interest_Rate <- input$initialInterestRate / 100
      temp_db_draw$Interest_Rate_Cont <-
        log(1 + temp_db_draw$Interest_Rate)
      temp_db_draw$F_Price <-
        input$initialStockPrice * (1 + input$initialInterestRate / 100) ^ (as.numeric(difftime(
          as.Date(input$initialMaturityDate),
          as.Date(input$initialPricingDate),
          unit = "weeks"
        )) / 52.1775)
      temp_db_draw$Liability <-
        -temp_db_draw$F_Price * exp(-temp_db_draw$Interest_Rate_Cont * temp_db_draw$TtM)
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      temp_db_draw$'Forward Value' <-
        round(temp_db_draw$Liability + temp_db_draw$Stock_Price, 1)
      
      #Composing XTS
      temp_xts_draw <-
        xts(x = temp_db_draw[, c("Asset", "Liability", "Forward Value")], order.by =
              temp_db_draw[, 6])
      
      #Deciding whether Asset, Liability of Off Balance
      
      if (tail(temp_db_draw$'Forward Value', 1) > 0) {
        #Asset
        
        temp_db_asset <-
          cbind.data.frame(
            "1",
            as.character(input$subsequentPricingDate),
            tail(temp_db_draw$'Forward Value', 1),
            1
          )
        names(temp_db_asset) <-
          c("Derivative_ID",
            "Pricing_Date",
            "Fair_Value",
            "Mark_to_Model") # set header to df
        dbWriteTable(db, "Asset", temp_db_asset, append = TRUE)
        
      } else if (tail(temp_db_draw$'Forward Value', 1) < 0) {
        #Liability
        
        temp_db_liability <-
          cbind.data.frame(
            "1",
            as.character(input$subsequentPricingDate),
            tail(temp_db_draw$'Forward Value', 1),
            1
          )
        names(temp_db_liability) <-
          c("Derivative_ID",
            "Pricing_Date",
            "Fair_Value",
            "Mark_to_Model") # set header to df
        dbWriteTable(db, "Liability", temp_db_liability, append = TRUE)
      } 
      
      else {
        # Off_Balance
        temp_db_off_balance <-
          cbind.data.frame("1",
                           as.character(input$subsequentPricingDate))
        names(temp_db_off_balance) <-
          c("Derivative_ID",
            "Pricing_Date") # set header to df
        dbWriteTable(db, "Off_Balance", temp_db_off_balance, append = TRUE)
      }
      
      #Plotting XTS
      dygraph(temp_xts_draw) %>%
        dyRangeSelector()
    })
  })
  
  observeEvent(input$initialReadFile, {
    forwardMasterData <- readRDS("forwardIni.rds")
    updateDateInput(session,
                    "initialPricingDate",
                    value = isolate(forwardMasterData$initialPricingDate))
    updateDateInput(
      session,
      "initialMaturityDate",
      value = isolate(forwardMasterData$initialMaturityDate)
    )
    updateSliderInput(session,
                      "initialStockPrice",
                      value = isolate(forwardMasterData$initialStockPrice))
    updateSliderInput(
      session,
      "initialInterestRate",
      value = isolate(forwardMasterData$initialInterestRate)
    )
  })
  
  observeEvent(input$initialSaveFile, {
    isolate({
      forwardMasterData  <-
        list (
          initialPricingDate = input$initialPricingDate,
          initialMaturityDate = input$initialMaturityDate,
          initialStockPrice = input$initialStockPrice,
          initialInterestRate = input$initialInterestRate
        )
    })
    saveRDS(forwardMasterData, "forwardIni.rds")
    
    
    input$initialPricingDate
    input$initialInterestRate
    
    temp_db_interest_rate <-
      cbind.data.frame(as.character(input$initialPricingDate),
                       input$initialInterestRate)
    names(temp_db_interest_rate) <-
      c("Pricing_Date", "Interest_Rate") # set header to df
    dbWriteTable(db, "Interest_Rate", temp_db_interest_rate, append = TRUE)
    
  })
  
  observeEvent(input$clearDB, {
    dbSendStatement(db, "DELETE from Stock_Information")
    dbSendStatement(db, "DELETE from Interest_Rate")
    dbSendStatement(db, "DELETE from Asset")
    dbSendStatement(db, "DELETE from Liability")
    dbSendStatement(db, "DELETE from Off_Balance")
    
    v$doCalcAndPlot <- input$clearStock_InformationDB
    
  })
  
  
  observeEvent(
    input$load_table_Stock_Information,
    output$table_Stock_Information <- renderDataTable({
      dbReadTable(db, "Stock_Information")
    })
  )
  
  observeEvent(
    input$load_table_Interest_Rate,
    output$table_Interest_Rate <- renderDataTable({
      dbReadTable(db, "Interest_Rate")
    })
  )
  
  observeEvent(input$load_table_Asset,
               output$table_Asset <- renderDataTable({
                 dbReadTable(db, "Asset")
               }))
  
  observeEvent(input$load_table_Liability,
               output$table_Liability <- renderDataTable({
                 dbReadTable(db, "Liability")
               }))
  
  observeEvent(input$load_table_Off_Balance,
               output$table_Off_Balance <- renderDataTable({
                 dbReadTable(db, "Off_Balance")
               }))
  
}