# barometric pressure module functions


barometricNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                numericInput(ns('barometric_convert'), 'Barometric', 
                             min = 0, max = 45, value = 760, step = 0.01),
                
                selectInput(ns('barometricConvertUnits'), label = paste0('Barometric Pressure Units'), 
                            choices = barometricChoices,
                            selected = 'mm Hg (torr)'),
                
                tags$h6()
    )
    
  )
  
}



barometricNumericModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       barometric_units_default = 'mm Hg (torr)',
                       barometric_or_altitude_default = 'Pressure',
                       barometric_sl_init = -1,
                       barometric_default = barometricSet)
  
  
  # ---- PRESSURE/ALTITUDE ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$barometricConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('barometricConvertUnits')
      
      rv$select_init <- 1
      
      barometric_units_init <- st()[[x]]
      
      if(length(barometric_units_init) == 0) {
        
        barometric_units_init <- rv$barometric_units_default
        
        # pressure_or_altitude <- rv$barometric_or_altitude_default
        
      } 
      # else {
      #   
      #   # SelectInput label: pressure_or_altitude 
      #   if(barometric_units_init %in% barometricChoices[1:4])
      #     pressure_or_altitude <- 'Pressure'
      #   else
      #     pressure_or_altitude <- 'Altitude'
      # }
      # 
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'barometricConvertUnits',
                        label = 'Barometric Pressure Units',  
                        choices = barometricChoices,
                        selected = barometric_units_init)
    }
    
    updateStore(session, session$ns("barometricConvertUnits"), input$barometricConvertUnits)
    
    idx <- which(input$barometricConvertUnits == barometricUnitsList)
    
    # modify selectInput label to reflect 'Pressure' or 'Altitude'
      if(idx %in% c(1:4))
        pressure_or_altitude <- 'Barometric Pressure Units'
      else
        pressure_or_altitude <- 'Altitude Units'
    
    updateSelectInput(session = session,
                      inputId = 'barometricConvertUnits',
                      label   = pressure_or_altitude)
    
    
    y <- paste0(session$ns('sl_'), input$barometricConvertUnits)
    
    my_barometric_value <- st()[[y]]
    
    if(length(my_barometric_value) == 0)
      my_barometric_value <- rv$barometric_default[idx]
    
    updateNumericInput(session, "barometric_convert", label = barometricUnitsList_short[idx],
                      value = my_barometric_value,
                      min = barometricMin[idx], max = barometricMax[idx], step = barometricStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$barometricConvertUnits), my_barometric_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  # observeEvent(c(input$gasSlider_convert, icTemp(), icSal()), {
  observeEvent(input$barometric_convert, {
    
    if(rv$barometric_sl_init < 0) {
      
      rv$barometric_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$barometricConvertUnits == barometricUnitsList)
    
    y <- paste0(session$ns('sl_'), input$barometricConvertUnits)
    
    my_barometric_value <- st()[[y]]
    
    
    if(length(my_barometric_value) == 0)
      my_barometric_value <- rv$barometric_default[idx]
    else
      my_barometric_value <- input$barometric_convert
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$barometricConvertUnits), my_barometric_value)
    
  })
  
  
  df_barometric <- reactive({    
    
    # req(input$barometricConvertUnits,
    #     input$barometric_convert
    # )
    
    idx_b <- which(input$barometricConvertUnits == barometricUnitsList)

    bar.LL <- barometricMin[idx_b]
    bar.UU <- barometricMax[idx_b]
    
    # cat('in barometric_numeric_module.R/df_barometric...\n')
    # cat('input$barometricConvertUnits = ', input$barometricConvertUnits, '\n')
    # cat('                       idx_b = ', idx_b, '\n')
    # cat('                      bar.LL = ', bar.LL, '\n')
    # cat('    input$barometric_convert = ', input$barometric_convert, '\n')
    # cat('                      bar.UU = ', bar.UU, '\n')

    str_message <- paste0('Please enter a value between ',
                          bar.LL, ' and ', bar.UU, ' ', barometricUnitsList[idx_b])

    validate(

      need(

        try(

          input$barometric_convert >= bar.LL &&
          input$barometric_convert <= bar.UU
        ),

        str_message
      )
    )
      
    icBarometric <- calcBarometricToIcUnits(input$barometric_convert, input$barometricConvertUnits)
    
    df <- calcBarometricToAllUnits(input$barometric_convert, input$barometricConvertUnits)
    
    # ** [KLUDGE] ** not use 'h' for formatting,
    #                so cache icSal -- unformatted for use in calcs
    
    hidden.col <- rep(0, 4)
    # hidden.col[idx_b] <- 1
    
    df$h <- hidden.col
    
    barometric_list <- list(df    = df, 
                            ic    = icBarometric,
                            val   = input$barometric_convert,
                            units = input$barometricConvertUnits)
    
    barometric_list
    
  })
  
  
  return(df_barometric)
  
}