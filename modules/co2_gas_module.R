# ATMOSPHERIC CO2 gas module functions for
# gas_tgp_module.R in
# "iQuaCalc (Lite) salinity module.R"


co2_gasModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                # splitLayout(cellWidths = c('35%', '65%'),
                numericInput(ns('co2_gasSlider_convert'), 'Atmospheric CO2', 
                             min = 4, max = 42, value = 28, step = 0.1),
                
                selectInput(ns('co2_gasConvertUnits'), 'Atmospheric CO2 Units', 
                            choices = co2_gasChoices),
                
                tags$h6()
    )
    
  )
}



co2_gasModule <- function(input, output, session, 
                          # icTemp, icSal, 
                          # icPh, icAlk, 
                          # icBarometric, 
                          st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       co2_gas_units_default = 'μatm',
                       co2_gas_sl_init = -1,
                       co2_gas_default = co2_gasSet)
  
  # ---- PRESSURE ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$co2_gasConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('co2_gasConvertUnits')
      
      rv$select_init <- 1
      
      co2_gas_units_init <- st()[[x]]
      
      if(length(co2_gas_units_init) == 0)
        co2_gas_units_init <- rv$co2_gas_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'co2_gasConvertUnits', 'Atmospheric CO2 Units', 
                        choices  = co2_gasChoices,
                        selected = co2_gas_units_init)
      
      freezeReactiveValue(input, "co2_gasConvertUnits")
    }
    
    updateStore(session, session$ns("co2_gasConvertUnits"), input$co2_gasConvertUnits)
    
    idx <- which(input$co2_gasConvertUnits == co2_gasChoices)
    
    y <- paste0(session$ns('sl_'), input$co2_gasConvertUnits)
    
    my_co2_gas_value <- st()[[y]]
    
    if(length(my_co2_gas_value) == 0)
      my_co2_gas_value <- rv$co2_gas_default[idx]
    
    updateSliderInput(session, "co2_gasSlider_convert", label = paste0(co2UnitsList[idx], ' CO2'),
                      value = my_co2_gas_value,
                      min = co2_gasMin[idx], max = co2_gasMax[idx], step = co2_gasStep[idx])
    
    freezeReactiveValue(input, "co2_gasSlider_convert")
    
    # update slider value for current units ???
    updateStore(session, y, my_co2_gas_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  # observeEvent(c(input$gasSlider_convert, icTemp(), icSal()), {
  observeEvent(input$co2_gasSlider_convert, {
    
    if(rv$co2_gas_sl_init < 0) {
      
      rv$co2_gas_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$co2_gasConvertUnits == co2_gasChoices)
    
    y <- paste0(session$ns('sl_'), input$co2_gasConvertUnits)
    
    my_co2_gas_value <- st()[[y]]
    
    
    if(length(my_co2_gas_value) == 0)
      my_co2_gas_value <- rv$co2_gas_default[idx]
    else
      my_co2_gas_value <- input$co2_gasSlider_convert
    
    # update slider value for current units
    # updateStore(session, paste0(session$ns('sl_'), input$co2_gasConvertUnits), my_co2_gas_value)
    updateStore(session, y, my_co2_gas_value)
    
    freezeReactiveValue(input, "co2_gasSlider_convert")
    
  })
  
  
  df_co2_gas <- reactive({
    
    # ----------------------------------------*
    idx_co2_g <- which(input$co2_gasConvertUnits == co2_gasChoices)
    
    co2.LL    <- co2_gasMin[idx_co2_g]
    co2.UU    <- co2_gasMax[idx_co2_g]
    co2.units <- co2UnitsList_with_micro[idx_co2_g]
    
    
    str_message <- paste0('Please enter atmospheric CO2 between ', 
                          co2.LL, ' & ', co2.UU, ' ', co2.units)
    
    validate(
      
      need(
        
        try(
          
          input$co2_gasSlider_convert >= co2.LL && 
            input$co2_gasSlider_convert <= co2.UU
        ),
        
        str_message
      )
    )
    # ----------------------------------------*
    
    req(
      input$co2_gasSlider_convert,
      input$co2_gasConvertUnits,
      
      cancelOutput = T
    )
      
    my_co2_mole_fraction <- calcCo2_atmmos_ToIcUnits(input$co2_gasSlider_convert, 
                                                     input$co2_gasConvertUnits)
      
      
      co2_gas_list <- list(val           = input$co2_gasSlider_convert,
                           units         = input$co2_gasConvertUnits,
                           co2_mole_frac = my_co2_mole_fraction)        # added to use in gas_tgp_module.R
                                                                        # NB: BUT already in co2_gas_atm_module.R
      co2_gas_list
    
  })
  
  
  return(df_co2_gas)
  
}