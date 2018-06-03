# gas module functions for
# "iQuaCalc (Lite) gas_module.R"

# originally supports O2 input in gas_tgp_module.R
# generally, made to support input of other (non-CO3?) gasses?


gasModuleInput <- function(id, gas_name, has_check_box, check_box_title) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '45%', '30%'),

                numericInput(ns('gasSlider_convert'), 'gas_name',
                             min = 4, max = 42, value = 28, step = 0.1),

                selectInput(ns('gasConvertUnits'), paste0(gas_name, ' Units'),
                            choices = gasChoices),

                checkboxInput(ns('tc'), check_box_title, value = F)

    )
    
    # fluidRow(
    # 
    #   column( width = 7,
    #           selectInput(ns('gasConvertUnits'), paste0(gas_name, ' Units'),
    #                       choices=gasChoices
    #                       )
    #   ),
    # 
    #   column( width = 5,
    #           checkboxInput(ns('tc'), check_box_title, value = F)
    #   )
    # ),
    # 
    # fluidRow(
    # 
    #   column( width = 11,
    #           numericInput(ns('gasSlider_convert'), gas_name,
    #                        min = 0, max = 45, value = 33, step = 0.01)
    #   )
    # )
    
  )
}


# --> Pass in gasType: 'O2', 'N2', 'Ar' ... 'CO2'??

gasModule <- function(input, output, session, icTemp, icSal, 
                      gas_name, has_check_box, check_box_title, 
                      icBarometric,
                      st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(cb_init = -1, 
                       cb_default = 0,
                       o2_max_factor = 1,
                       select_init = -1,
                       gas_units_default = 'mg/L',
                       gas_sl_init = -1,
                       gas_default = gasSet)
  
  # ---- TC ----
  
  # Observe CHECKBOX_INPUT input, store when changed
  observeEvent(input$tc, {
    
    if(1 == has_check_box) {
      
      if(rv$cb_init < 0) {
        
        rv$cb_init <- 1
        
        x <- session$ns('tc')
        
        my_tc_value <- st()[[x]]
        
        if(length(my_tc_value) == 0)
          my_tc_value <- rv$cb_default
        
        updateCheckboxInput(session, 'tc', check_box_title,
                            value = as.integer(my_tc_value))
        
        updateStore(session, session$ns("tc"), as.integer(my_tc_value))
        
        if(1 == input$tc) {
          
          o2_mole_fraction <- 1.0
          rv$o2_max_factor <- 5.0 # KLUDGE
          
        } else {
          
          o2_mole_fraction <- MFV_O2
          rv$o2_max_factor <- 1.0 # KLUDGE
          
        }
        
        return()
      }
      
      if(1 == input$tc) {
        
        o2_mole_fraction <- 1.0
        rv$o2_max_factor <- 5.0 # KLUDGE
        
      } else {
        
        o2_mole_fraction <- MFV_O2
        rv$o2_max_factor <- 1.0 # KLUDGE
        
      }
      
      updateStore(session, session$ns("tc"), as.integer(input$tc))
      
    }
    
  })
  
  
  observe({
    
    # cat('has_check_box: ', has_check_box, '\n')
    # cat('gas_module.R ... \n')
    
    # [KLUDGE] ...
    # WITHOUT "req(input$tc)", gas_tgp_module.R crashes with 
    # "ERROR: [on_request_read] connection reset by peer" in RStudio browser, but can run in Chrome, Safari
    # but does NOT crash when tgp_module_WITH_MODULES.R runs first or only
    ### RESOLUTION: did NOT call *Input function, so input$tc was NULL
    # req(input$tc)
    
    if(1 == has_check_box) {
      
      # to switch between atmospheric and pure O2
      # atmospheric mole fraction by volume: 0.209476 (MFV_O2)
      # PURE mole fraction by volume: 1.0 
      # ... and ...
      # expand slider scale for pure O2
      
      if(1 == input$tc) {
        
        # o2_mole_fraction <- 1.0
        rv$o2_max_factor <- 5.0    # KLUDGE: 1 / MFV_O2 = 1 / 0.209476 = 4.773817
        
      } else {
        
        # o2_mole_fraction <- MFV_O2
        rv$o2_max_factor <- 1.0
        
      }
      
    }
    
  })
  
  
  
  # ---- PRESSURE ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(c(input$gasConvertUnits, rv$o2_max_factor), priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('gasConvertUnits')
      
      rv$select_init <- 1
      
      gas_units_init <- st()[[x]]
      
      # cat('0: gas_units_init = ', gas_units_init, '\n')
      
      if(length(gas_units_init) == 0)
        gas_units_init <- rv$gas_units_default
      
      # cat('1: gas_units_init = ', gas_units_init, '\n')
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'gasConvertUnits', paste0(gas_name, ' Units'), 
                        choices  = gasChoices,
                        selected = gas_units_init)
      
      # return()
    }
    
    updateStore(session, session$ns("gasConvertUnits"), input$gasConvertUnits)
    
    idx <- which(input$gasConvertUnits == gasUnitsListPref)
    
    y <- paste0(session$ns('sl_'), input$gasConvertUnits, '_tc_', input$tc)
    
    my_gas_value <- st()[[y]]
    
    if(length(my_gas_value) == 0)
      my_gas_value <- rv$gas_default[idx]
    
    updateNumericInput(session, "gasSlider_convert", 
                       label = paste0(gasUnitsListPref_short[idx], ' ', gas_name),
                       value = my_gas_value,
                       min = gasMin[idx], max = rv$o2_max_factor * gasMax[idx], step = gasStep[idx])
    
    # updateSliderInput(session, "gasSlider_convert", label = paste0(gasUnitsListPref[idx], ' ', gas_name),
    #                   value = my_gas_value,
    #                   min = gasMin[idx], max = rv$o2_max_factor * gasMax[idx], step = gasStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$gasConvertUnits, '_tc_', input$tc), my_gas_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  # observeEvent(c(input$gasSlider_convert, icTemp(), icSal()), {
  observeEvent(input$gasSlider_convert, {
    
    if(rv$gas_sl_init < 0) {
      
      rv$gas_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$gasConvertUnits == gasUnitsListPref)
    
    y <- paste0(session$ns('sl_'), input$gasConvertUnits, '_tc_', input$tc)
    
    my_gas_value <- st()[[y]]
    
    
    if(length(my_gas_value) == 0)
      my_gas_value <- rv$gas_default[idx]
    else
      my_gas_value <- input$gasSlider_convert
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$gasConvertUnits, '_tc_', input$tc), my_gas_value)
    
  })
  
  
  df_gas <- reactive({
    
    # ----------------------------------------*
    
    # NB: MUST 'flatten' named gasChoices to gasUnitsListPref ----
    idx_g <- which(input$gasConvertUnits == gasUnitsListPref)
    
    gas.LL    <- gasMin[idx_g]
    gas.UU    <- gasMax[idx_g]
    gas.units <- gasUnitsListPref[idx_g]
    
    
    str_message <- paste0('Please enter a ', gas_name,' value between ', 
                          gas.LL, ' & ', gas.UU, ' ', gas.units)
    
    
    validate(
      
      need(
        
        try(
          
          input$gasSlider_convert >= gas.LL && 
            input$gasSlider_convert <= gas.UU
        ),
        
        str_message
      )
    )
    
    # ----------------------------------------*
    
    my_icTemp       <- icTemp()$ic
    my_icSal        <- icSal()$ic
    my_icBarometric <- icBarometric()$ic
    
    # idx_g <- which(input$gasConvertUnits == gasUnitsListPref)
    
    
    if((input$gasSlider_convert <= rv$o2_max_factor * gasMax[idx_g] && 
        input$gasSlider_convert >= gasMin[idx_g]) &&
       (my_icTemp <= tempMax[3] && my_icTemp >= tempMin[3])) {
      
    # validate(
    #   
    #   need(input$gasSlider_convert <= gasMax[idx_g] && input$gasSlider_convert >= gasMin[idx_g], 
    #        'Whoa 1'),
    #   
    #   need(my_icTemp <= tempMax[3] && my_icTemp >= tempMin[3], 
    #        'Whoa 2')
    # )
      
      icGas <- calcGasToIcUnits(gas_name, input$gasSlider_convert, input$gasConvertUnits, 
                                icTemp()$ic, icSal()$ic, my_icBarometric)
      
      df <- calcGasAllUnits(gas_name, input$gasSlider_convert, input$gasConvertUnits, 
                            my_icTemp, my_icSal, my_icBarometric)
      
      # ** [KLUDGE] ** not use 'h' for formatting,
      #                so cache icSal -- unformatted for use in calcs
    
      hidden.col <- rep(0, 14)
      hidden.col[idx_g] <- 1

      df$h <- hidden.col
      
      gas_list <- list(df = df, ic = icGas)
      
      # print(gas_list)

      gas_list
    }
    
  })
  
  
  return(df_gas)
  
}