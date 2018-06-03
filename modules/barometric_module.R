# barometric pressure module functions for
# "iQuaCalc (Lite) salinity module.R"


barometricModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column( width = 7,
              selectInput(ns('barometricConvertUnits'), 
                          label = paste0('Barometric'), 
                          choices = barometricChoices
              )
      )
    ),
    
    fluidRow(
      
      column( width = 11,
              sliderInput(ns('barometricSlider_convert'), 'Barometric', 
                          min = 0, max = 45, value = 33, step = 0.01, animate = F)
              )
      )
  )
}



barometricModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       barometric_units_default = 'mm Hg (torr)',
                       barometric_sl_init = -1,
                       barometric_default = barometricSet)
  
  # ---- PRESSURE ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$barometricConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('barometricConvertUnits')
      
      rv$select_init <- 1
      
      barometric_units_init <- st()[[x]]
      
      if(length(barometric_units_init) == 0)
        barometric_units_init <- rv$barometric_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'barometricConvertUnits',
                        label = paste0('Barometric'),  
                        choices = barometricChoices,
                        selected = barometric_units_init)
    }
    
    updateStore(session, session$ns("barometricConvertUnits"), input$barometricConvertUnits)
    
    idx <- which(input$barometricConvertUnits == barometricUnitsList)
    
    y <- paste0(session$ns('sl_'), input$barometricConvertUnits)
    
    my_barometric_value <- st()[[y]]
    
    if(length(my_barometric_value) == 0)
      my_barometric_value <- rv$barometric_default[idx]
    
    updateSliderInput(session, "barometricSlider_convert", label = barometricUnitsList[idx],
                      value = my_barometric_value,
                      min = barometricMin[idx], max = barometricMax[idx], step = barometricStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$barometricConvertUnits), my_barometric_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  # observeEvent(c(input$gasSlider_convert, icTemp(), icSal()), {
  observeEvent(input$barometricSlider_convert, {
    
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
      my_barometric_value <- input$barometricSlider_convert
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$barometricConvertUnits), my_barometric_value)
    
  })
  
  
  df_barometric <- reactive({    
    
    idx_b <- which(input$barometricConvertUnits == barometricUnitsList)
    
    
    if((input$barometricSlider_convert <= barometricMax[idx_b] && 
        input$barometricSlider_convert >= barometricMin[idx_b])) {
      
    # validate(
    #   
    #   need(input$gasSlider_convert <= gasMax[idx_g] && input$gasSlider_convert >= gasMin[idx_g], 
    #        'Whoa 1'),
    #   
    #   need(my_icTemp <= tempMax[3] && my_icTemp >= tempMin[3], 
    #        'Whoa 2')
    # )
      
      icBarometric <- calcBarometricToIcUnits(input$barometricSlider_convert, input$barometricConvertUnits)

      df <- calcBarometricToAllUnits(input$barometricSlider_convert, input$barometricConvertUnits)
      
      # ** [KLUDGE] ** not use 'h' for formatting,
      #                so cache icSal -- unformatted for use in calcs
    
      hidden.col <- rep(0, 4)
      # hidden.col[idx_b] <- 1

      df$h <- hidden.col

      barometric_list <- list(df = df, 
                              ic = icBarometric,
                              val = input$barometricSlider_convert,
                              units = input$barometricConvertUnits)

      barometric_list
    }
    
  })
  
  
  return(df_barometric)
  
}