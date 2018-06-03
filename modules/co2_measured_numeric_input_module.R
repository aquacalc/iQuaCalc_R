# DISSOLVED & MEASURED CO2 module for
# gas_tgp_module.R module in
# "iQuaCalc (Lite).R"


co2MeasuredNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                numericInput(ns('co2_input'), HTML(paste0('CO', tags$sub('2'))),
                             value = 1,
                             min = 0, max = 100, step = 0.01),
                
                # selectInput(ns('co2ConvertUnits'), HTML(paste0('CO', tags$sub('2'), ' Units')), 
                selectInput(ns('co2ConvertUnits'), 'CO2 Units', 
                            choices = co2DissolvedChoices),
                
                tags$h6()
                
    )
  )   # END tagList
}



co2MeasuredNumericModule <- function(input, output, session,
                                     icTemp, icSal, 
                                     icPh, icBarometric,
                                     st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(cb_init = -1, 
                       cb_default = 0,
                       select_init = -1,
                       co2_units_default = 'mg/L',
                       co2_sl_init = -1,
                       co2_default = co2_dissolvedSet
                       )
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$co2ConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('co2ConvertUnits')
      
      rv$select_init <- 1
      
      co2_units_init <- st()[[x]]
      
      if(length(co2_units_init) == 0)
        co2_units_init <- rv$co2_units_default
      
      co2DissolvedChoices <- c('mg/L', 'mg/kg', 
                               'mmol/L', 'mmol/kg', 
                               'μmol/L', 'μmol/kg',
                               'mm Hg (torr)', 'atm')
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "co2ConvertUnits",
                        # label = HTML(paste0('CO', tags$sub('2'), ' Units')), 
                        label = 'CO2 Units', 
                        # choices = co2DissolvedChoices,
                        choices = list(
                          'Most Common' = co2DissolvedChoices[c(1, 7)],
                          'Other'       = co2DissolvedChoices[c(2:6, 8)]
                        ),
                        selected = co2_units_init)
    }
    
    updateStore(session, ns("co2ConvertUnits"), input$co2ConvertUnits)
    
    
    idx <- which(input$co2ConvertUnits == co2DissolvedChoices)
    
    y <- paste0(ns('sl_'), input$co2ConvertUnits)
    
    
    my_co2_value <- st()[[y]]
    
    if(length(my_co2_value) == 0)
      my_co2_value <- rv$co2_default[idx]
    
    updateNumericInput(session, "co2_input", label = co2DissolvedChoices_short[idx],
                       value = my_co2_value,
                       min   = co2_dissolvedMin[idx], 
                       max   = co2_dissolvedMax[idx], 
                       step  = co2_dissolvedStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$co2ConvertUnits), my_co2_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$co2_input, {
    
    if(rv$co2_sl_init < 0) {
      
      rv$co2_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$co2ConvertUnits == co2DissolvedChoices)
    
    y <- paste0(ns('sl_'), input$co2ConvertUnits)
    
    my_co2_value <- st()[[y]]
    
    
    if(length(my_co2_value) == 0)
      my_co2_value <- rv$co2_default[idx]
    else
      my_co2_value <- input$co2_input
    
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$co2ConvertUnits), my_co2_value)
    
  })

  
  
  df_co2_measured <- reactive({
    
    # cat('\n\nin co2_measured_numeric_input_module.R/df_co2_measured...\n')
    # cat('INPUT: ', input$co2_input, ' ', input$co2ConvertUnits, '\n')
    
    idx <- which(input$co2ConvertUnits == co2DissolvedChoices)
    
    validate(
      
      need(
        
        try(
          
          input$co2_input >= co2_dissolvedMin[idx] && 
            input$co2_input <= co2_dissolvedMax[idx]
        ),
        
        # HTML(paste0('Please enter a CO', tags$sub('2'), ' concentration between ', 
        HTML(paste0('Please enter a CO2 concentration between ', 
                    co2_dissolvedMin[idx], ' and ', co2_dissolvedMax[idx]))
      )
    )
    
    req(
      input$co2_input, 
      input$co2ConvertUnits,
      
      cancelOutput = T
    )
    
    co2_mg_per_kg <- convertGasFromTo('CO2', 
                                      input$co2_input, input$co2ConvertUnits,
                                      'mg/kg', 
                                      icTemp()$ic, icSal()$ic, icBarometric()$ic)
    
    co2_mg_per_kg <- co2_mg_per_kg$vals  # NB: convertGasFromTo returns a list; get 'vals'
    
    
    co2_mg_per_L  <- convertGasFromTo('CO2', 
                                      input$co2_input, input$co2ConvertUnits,
                                      'mg/L', 
                                      icTemp()$ic, icSal()$ic, icBarometric()$ic)
    
    co2_mg_per_L <- co2_mg_per_L$vals   # NB: convertGasFromTo returns a list; get 'vals'
    
    
    # for option to enter measured CO2 in gas_tgp_module.R,
    # return [CO2] in mg/kg -- or mg/L better?? -- to calculate
    # %, tension, and ΔP.
    co2_measured_list <- list(co2_mg_per_kg = co2_mg_per_kg,
                              co2_mg_per_L  = co2_mg_per_L,
                              val           = input$co2_input,
                              units         = input$co2ConvertUnits
    )

    co2_measured_list
    
  })
  
  
  return(df_co2_measured)
  
}
