# AREA module functions for
# "iQuaCalc (Lite).R"


volumeNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                numericInput(ns('volume_input'), 'Volume',
                             value = 1,
                             min = 0, max = 1000, step = 0.01),
                
                selectInput(ns('volumeConvertUnits'), 'Volume Units', 
                            choices = volumeUnitsChoices),
                
                tags$h6()
                
    )
  )   # END tagList
}



volumeNumericModule <- function(input, output, session, st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       volume_units_default = 'cubic meters (m\U00B3)',
                       volume_sl_init = -1,
                       volume_default = volumeSet
                       )
  

  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$volumeConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('volumeConvertUnits')
      
      rv$select_init <- 1
      
      volume_units_init <- st()[[x]]
      
      if(length(volume_units_init) == 0)
        volume_units_init <- rv$volume_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "volumeConvertUnits",
                        label = 'Volume Units', 
                        choices = volumeUnitsChoices,
                        selected = volume_units_init)
    }
    
    updateStore(session, ns("volumeConvertUnits"), input$volumeConvertUnits)
    
    
    idx <- which(input$volumeConvertUnits == volumeUnitsChoices)
    
    y <- paste0(ns('sl_'), input$volumeConvertUnits)
    
    
    my_volume_value <- st()[[y]]
    
    if(length(my_volume_value) == 0)
      my_volume_value <- rv$volume_default[idx]
    
    updateNumericInput(session, "volume_input", label = volumeUnitsChoices_short[idx],
                       value = my_volume_value,
                       min = volumeMin[idx], max = volumeMax[idx], step = volumeStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$volumeConvertUnits), my_volume_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$volume_input, {
    
    if(rv$volume_sl_init < 0) {
      
      rv$volume_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$volumeConvertUnits == volumeUnitsChoices)
    
    y <- paste0(ns('sl_'), input$volumeConvertUnits)
    
    my_volume_value <- st()[[y]]
    
    
    if(length(my_volume_value) == 0)
      my_volume_value <- rv$volume_default[idx]
    else
      my_volume_value <- input$volume_input
    
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$volumeConvertUnits), my_volume_value)
    
  })

  
  
  df_volume <- reactive({
    
    req(
      input$volume_input, input$volumeConvertUnits,
      cancelOutput = T
    )
    
    validate(
      
      need(
        
        try(
          
          input$volume_input > 0
        ),
        
        'Please enter a volume greater than 0'
      )
    )
    
    icVolume <- getInIcUnits(input$volume_input, input$volumeConvertUnits, volume.data)
    
    df <- convertAll(icVolume, 'liters (L)', volume.data)
    
    
    # format decimal values
    df <- round_values(df)
    
    
    idx_volume <- which(input$volumeConvertUnits == volumeUnitsChoices)
    
    hidden.col <- c(rep(0, 7))
    hidden.col[idx_volume] <- 1
    
    # df_x <- data.frame(vals = c(rep(3, 9)), units = c(rep('w', 9)), stringsAsFactors = F)
       
    df <- cbind(df, h = hidden.col)
    
    volume_list <- list(df = df, 
                        ic = icVolume,
                        val = input$volume_input,
                        units = input$volumeConvertUnits)
    
    volume_list
    
  })
  
  
  return(df_volume)
  
}
