# temperature module functions for
# "iQuaCalc (Lite) salinity module.R"


temperatureNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                numericInput(ns('tempSlider_convert'), 'Temperature', 
                             min = 4, max = 42, value = 28, step = 0.1),
                
                selectInput(ns('tempConvertUnits'), 'Temperature Units', 
                            choices = tempUnitsList),
                
                tags$h6()
                
    )
    
  )
}



temperatureNumericModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       temp_units_default = 'C',
                       temp_sl_init = -1,
                       temp_default = c(30, 82, 301.15))
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$tempConvertUnits, priority = 100, {

    if(rv$select_init < 0)  {

      x <- session$ns('tempConvertUnits')

      rv$select_init <- 1

      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "tempConvertUnits",
                        label = 'Temperature Units', choices = tempUnitsList,
                        selected = st()[[x]])
    }

    updateStore(session, session$ns("tempConvertUnits"), input$tempConvertUnits)


    idx <- which(input$tempConvertUnits == tempUnitsList)

    y <- paste0(session$ns('sl_'), input$tempConvertUnits)
    

    my_temp_value <- st()[[y]]

    if(length(my_temp_value) == 0)
      my_temp_value <- rv$temp_default[idx]

    updateNumericInput(session, "tempSlider_convert", label = tempUnitsList_short[idx],
                      value = my_temp_value,
                      min = tempMin[idx], max = tempMax[idx], step = tempStep[idx])
    
    freezeReactiveValue(input, "tempSlider_convert")

    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$tempConvertUnits), my_temp_value)

  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$tempSlider_convert, {
    
    if(rv$temp_sl_init < 0) {
      
      rv$temp_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$tempConvertUnits == tempUnitsList)
    
    y <- paste0(session$ns('sl_'), input$tempConvertUnits)
    
    my_temp_value <- st()[[y]]
  
    
    if(length(my_temp_value) == 0)
      my_temp_value <- rv$temp_default[idx]
    else
      my_temp_value <- input$tempSlider_convert
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$tempConvertUnits), my_temp_value)
    
  })
  
  
  
  df_temp <- reactive({
    
    idx_t <- which(input$tempConvertUnits == tempUnitsList)
    
    temp.LL <- tempMin[idx_t]
    temp.UU <- tempMax[idx_t]
    temp.units.short <- tempUnitsList_short[idx_t]
    
    str_message <- paste0('Please enter a temperature between ', 
                          temp.LL, ' and ', temp.UU, ' ', temp.units.short)
    
    validate(
      
      need(
        
        try(
        
        input$tempSlider_convert >= temp.LL && 
          input$tempSlider_convert <= temp.UU
        ),
      
        str_message
        )
    )
    
    
    temp <- input$tempSlider_convert
    
    icTemp <- tempToIcUnits(temp, idx_t)
    
    hidden.col <- c(0, 0, 0)
    hidden.col[idx_t] <- 1
    
    df <- tempToAllUnits(icTemp)    
    df <- cbind(df, h = hidden.col)
    
    # as.data.frame(df)
    
    # as in "salinity_module.R," not just "as.data.frame(df)"
    
    current_units_short <- tempUnitsList_short[idx_t]
    
    temp_list <- list(df = df, 
                      ic = icTemp, 
                      val = temp, 
                      units = current_units_short)
    
    temp_list
  })
  
  
  return(df_temp)
}