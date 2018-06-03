# [Alk] module functions for
# "iQuaCalc (Lite) salinity module.R"

# NB: Split Layout to conform with WQ Map UI for init & target waypoints


alkGreenZoneModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('22%', '22%', '51%', '5%'),
                
                numericInput(ns('alk_green_zone_low'), 'Low [Alk]', 2),
                
                numericInput(ns('alk_green_zone_high'), 'High [Alk]', 3.2),
                
                selectInput(ns('alk_green_zone_units'), '[Alk] Units',
                            choices = alkUnitsList),
                
                tags$h4(' ')
    )
    
    
    # splitLayout(cellWidths = c('30%', '70%'),
    #             
    #             numericInput(ns('alk_green_zone_low'), '[Alk]', 
    #                          min = 0, max = 45, value = 33, step = 0.1),
    #             
    #             selectInput(ns('alk_green_zone_units'), '[Alk] Units', 
    #                         choices = alkUnitsList)
    #             
    # )
    
  )
}



alkGreenZoneModule <- function(input, output, session, 
                               icTemp, icSal, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       alk_units_default = 'meq/kg (mmol/kg)',
                       
                       alk_sl_init_low  = -1,
                       alk_sl_init_high = -1,
                       
                       alk_default_low = c(2.0, 2.0,  90,  90, 7.5), # w/ 2 inputs, make lower != higher
                       alk_default     = c(2.4, 2.4, 120, 120, 8.0)
                       )
  
  
  
  # ---- ALKALINITY ----
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$alk_green_zone_units, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('alk_green_zone_units')
      
      rv$select_init <- 1
      
      alk_units_init <- st()[[x]]
      
      if(length(alk_units_init) == 0)
        alk_units_init <- rv$alk_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'alk_green_zone_units', '[Alk] Units',
                        # choices=alkUnitsList,
                        choices=alkUnitsList,
                        selected = alk_units_init)
    }
    
    updateStore(session, session$ns("alk_green_zone_units"), input$alk_green_zone_units)
    
    idx <- which(input$alk_green_zone_units == alkUnitsList)
    
    
    # handle LOW Alk input ----
    
    y <- paste0(session$ns('sl_low_'), input$alk_green_zone_units)
    
    my_alk_value_low <- st()[[y]]
    
    if(length(my_alk_value_low) == 0)
      my_alk_value_low <- rv$alk_default_low[idx]
    
    # updateNumericInput(session, "alk_green_zone_low", label = alkUnitsList[idx],
    updateNumericInput(session, "alk_green_zone_low", label = 'Low [Alk]',
                      value = my_alk_value_low,
                      min = alkMin[idx], max = alkMax[idx], step = alkStep[idx])
    
    freezeReactiveValue(input, "alk_green_zone_low")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_low_'), input$alk_green_zone_units), my_alk_value_low)
    
    
    # handle HIGH Alk input ----
    
    z <- paste0(session$ns('sl_high_'), input$alk_green_zone_units)
    
    my_alk_value_high <- st()[[z]]
    
    if(length(my_alk_value_high) == 0)
      my_alk_value_high <- rv$alk_default[idx]
    
    # updateNumericInput(session, "alk_green_zone_low", label = alkUnitsList[idx],
    updateNumericInput(session, "alk_green_zone_high", label = 'High [Alk]',
                       value = my_alk_value_high,
                       min = alkMin[idx], max = alkMax[idx], step = alkStep[idx])
    
    freezeReactiveValue(input, "alk_green_zone_high")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_high_'), input$alk_green_zone_units), my_alk_value_high)
    
  })
  
  
  # Observe LOW ALK input, store when changed
  observeEvent(input$alk_green_zone_low, {
    
    if(rv$alk_sl_init_low < 0) {
      
      rv$alk_sl_init_low <- 1
      
      return()
    }
    
    idx <- which(input$alk_green_zone_units == alkUnitsList)
    
    y <- paste0(session$ns('sl_low_'), input$alk_green_zone_units)
    
    my_alk_value_low <- st()[[y]]
    
    
    if(length(my_alk_value_low) == 0)
      my_alk_value_low <- rv$alk_default_low[idx]
    else
      my_alk_value_low <- input$alk_green_zone_low
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_low_'), input$alk_green_zone_units), my_alk_value_low)
    
  })
  
  
  # Observe HIGH ALK input, store when changed
  observeEvent(input$alk_green_zone_high, {
    
    if(rv$alk_sl_init_high < 0) {
      
      rv$alk_sl_init_high <- 1
      
      return()
    }
    
    idx <- which(input$alk_green_zone_units == alkUnitsList)
    
    y <- paste0(session$ns('sl_high_'), input$alk_green_zone_units)
    
    my_alk_value_high <- st()[[y]]
    
    
    if(length(my_alk_value_high) == 0)
      my_alk_value_high <- rv$alk_default[idx]
    else
      my_alk_value_high <- input$alk_green_zone_high
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_high_'), input$alk_green_zone_units), my_alk_value_high)
    
  })
  
  
  df_alk_green_zone <- reactive({
    
    idx <- which(input$alk_green_zone_units == alkUnitsList)
    
    validate(
      
      need(
        
        try(
        
          input$alk_green_zone_low >= alkMin[idx] && 
          input$alk_green_zone_low <= alkMax[idx] 
          
          &&

          input$alk_green_zone_high >= alkMin[idx] &&
          input$alk_green_zone_high <= alkMax[idx]
      ),
      
      paste0('Please enter [Alk] values are between ', alkMin[idx], ' and ', alkMax[idx])
      )
    )
    
    req(
      icTemp(),
      icSal(),
      cancelOutput = T
    )
    
    my_icTemp <- icTemp()$ic
    my_icSal <- icSal()$ic
    
    
    ic_rho <- calcRho(my_icTemp, my_icSal)
    
    my_rho <- ic_rho / 1000.0   # in kg/L
    
    icAlk_low  <- alkToIcUnits(input$alk_green_zone_low,  input$alk_green_zone_units, my_rho)
    icAlk_high <- alkToIcUnits(input$alk_green_zone_high, input$alk_green_zone_units, my_rho)
    
    alk_coords <- tibble(alk_low  = icAlk_low,
                         alk_high = icAlk_high
    )
    
    
    alk_coords
    
  })
  
  
  return(df_alk_green_zone)
  
}