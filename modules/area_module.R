# AREA module functions for
# "iQuaCalc (Lite).R"


areaModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
              
    # fluidRow(
    
      # splitLayout(cellWidths = c('40%', '50%'),
      splitLayout(cellWidths = c('25%', '50%', '25%'),
                  
                  numericInput(ns('area_input'), 'Area',
                               value = 1,
                               min = 0, max = 1000, step = 0.01),
                  
                  selectInput(ns('areaConvertUnits'), 'Area Units', 
                              choices = areaUnitsChoices),
                  
                  tags$h6()
      )
      
    # ) # END fluidRow
  )   # END tagList
}




areaModule <- function(input, output, session, st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       area_units_default = 'm²',
                       area_sl_init = -1,
                       area_default = c(rep(1, 8))
                       )
  

  
  # Observe SELECT_INPUT input, store when changed ----
  observeEvent(input$areaConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('areaConvertUnits')
      
      rv$select_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "areaConvertUnits",
                        label = 'Area Units', choices = areaUnitsChoices,
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("areaConvertUnits"), input$areaConvertUnits)
    
    
    idx <- which(input$areaConvertUnits == areaUnitsChoices)
    
    y <- paste0(ns('sl_'), input$areaConvertUnits)
    
    
    my_area_value <- st()[[y]]
    
    if(length(my_area_value) == 0)
      my_area_value <- rv$area_default[idx]
    
    updateNumericInput(session, 'area_input', areaUnits[idx], 
                       value = my_area_value, 
                       min = areaMin[idx], max = areaMax[idx], step = areaStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$areaConvertUnits), my_area_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed ----
  observeEvent(input$area_input, {
    
    if(rv$area_sl_init < 0) {
      
      rv$area_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$areaConvertUnits == areaUnitsChoices)
    
    y <- paste0(ns('sl_'), input$areaConvertUnits)
    
    my_area_value <- st()[[y]]
    
    
    if(length(my_area_value) == 0)
      my_area_value <- rv$area_default[idx]
    else
      my_area_value <- input$area_input
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$areaConvertUnits), my_area_value)
    
  })

  
  # calc conversion data ----
  df_area <- reactive({
    
    req(
      input$area_input, input$areaConvertUnits,
      cancelOutput = T
    )
    
    icArea <- getInIcUnits(input$area_input, input$areaConvertUnits, area.data)
    
    df <- convertAll(icArea, 'ha', area.data)
    
    
    
    # format decimal values
    df <- round_values(df)
    
    
    idx_area <- which(input$areaConvertUnits == areaUnitsChoices)
    
    hidden.col <- c(rep(0, 8))
    hidden.col[idx_area] <- 1
    
    # df_x <- data.frame(vals = c(rep(3, 9)), units = c(rep('w', 9)), stringsAsFactors = F)
       
    df <- cbind(df, h = hidden.col)
    
    area_list <- list(df = df, 
                      ic = icArea,
                      val = input$area_input,
                      units = input$areaConvertUnits)
    
    area_list
    
  })
  
  
  return(df_area)
}
