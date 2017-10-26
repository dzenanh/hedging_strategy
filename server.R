library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)
library(V8)

#necessary for remote box-collapsing
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

db <- dbConnect(SQLite(), "database.sqlite")
sqlite <- dbConnect(SQLite(), "db.sqlite")
server <- function(input, output, session) {
  
  observeEvent(input$ab_Initial_Pricing, {
    #js$collapse("box_Initial_Pricing")
    js$collapse("box_Do")
    hide(id = "box_Initial_Pricing", anim = FALSE)
    
    temp_db_Stock_Derivative_Static <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative,
        input$ti_Stock_ISIN,
        input$ti_Execution_Or_Forward_Price,
        as.character(input$ti_Contracting_Date),
        as.character(input$ti_Expiration_Date),
        input$ti_Contract_Size,
        input$ti_Number_Of_Contracts,
        input$ti_Stock_Volatility,
        input$ti_Interest_Rate,
        input$ti_Mark_To_Model
      )
    names(temp_db_Stock_Derivative_Static) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Execution_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static,
                 append = TRUE)
  })
  observeEvent(input$button_Do, {
    temp_db_Stock_Pricing_Dynamic <-
      cbind.data.frame(
        input$ti_Stock_ISIN,
        input$ti_Do_Stock_Price,
        as.character(input$ti_Do_timestamp)
      )
    names(temp_db_Stock_Pricing_Dynamic) <-
      c(
        "Stock_ISIN",
        "Stock_Price",
        "timestamp"
      ) 
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic,
                 append = TRUE)
    
    
    #js$collapse("box_Do")
    js$collapse("box_Plan")
  })
  
  observeEvent(input$button_Plan, {
    #js$collapse("box_Plan")
    js$collapse("box_Check")
  })
  
  #https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
  
  
  observeEvent(input$button_Check, {
    #js$collapse("box_Check")
    js$collapse("box_Act")
  })
  
  observeEvent(input$button_Act, {

    v$doCalcAndPlot <- input$button_Act #CalcAndPlot
  })
  
  observeEvent(input$button_Act_Continue, {
    js$collapse("box_Act")
    js$collapse("box_Plan")
    js$collapse("box_Check")
    #js$collapse("box_Do")
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
      
      
      temp_db_draw <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
      temp_db_draw$Pricing_Date <- as.Date(as.POSIXct(temp_db_draw$timestamp))
      
      #get initial pricing
      
      #temp_db_draw <- dbReadTable(db, "Stock_Information")
      #temp_db_draw$Pricing_Date <-
       # as.Date(as.POSIXct(temp_db_draw$Pricing_Date))
      
      temp_db_draw$TtM <-
        as.numeric(difftime(
          as.Date(isolate(input$ti_Expiration_Date)),
          as.Date(temp_db_draw$Pricing_Date),
          unit = "weeks"
        )) / 52.1775
      temp_db_draw$Interest_Rate <- as.numeric(input$ti_Interest_Rate) / 100
      temp_db_draw$Interest_Rate_Cont <-
        log(1 + temp_db_draw$Interest_Rate)
      temp_db_draw$F_Price <-
        temp_db_draw[1,3] * (1 + as.numeric(input$ti_Interest_Rate) / 100) ^ (as.numeric(difftime(
          as.Date(input$ti_Expiration_Date),
          as.Date(input$ti_Contracting_Date),
          unit = "weeks"
        )) / 52.1775)
      temp_db_draw$Liability <-
        -temp_db_draw$F_Price * exp(-temp_db_draw$Interest_Rate_Cont * temp_db_draw$TtM)
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      temp_db_draw$'Forward Value' <-
        round(temp_db_draw$Liability + temp_db_draw$Stock_Price, 1)
      
      write.csv(temp_db_draw, "tempdb.csv")
      
      #Composing XTS
      temp_xts_draw <-
        xts(x = temp_db_draw[, c("Asset", "Liability", "Forward Value", "TtM")], order.by =
              temp_db_draw[, 5])
      
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
    dbSendStatement(sqlite, "DELETE from Stock_Pricing_Dynamic")
    
    #v$doCalcAndPlot <- input$clearStock_InformationDB
    
  })
  
  
  observeEvent(
    input$load_table_Stock_Pricing_Dynamic,
    output$table_Stock_Pricing_Dynamic <- renderDataTable({
      dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Information_Static,
    output$table_Stock_Information_Static <- renderDataTable({
      dbReadTable(sqlite, "Stock_Information_Static")
    })
  )
  
  observeEvent(input$load_table_Stock_Derivative_Static,
               output$table_Stock_Derivative_Static <- renderDataTable({
                 dbReadTable(sqlite, "Stock_Derivative_Static")
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