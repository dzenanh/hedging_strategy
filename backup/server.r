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

#shinyApp(ui, server)