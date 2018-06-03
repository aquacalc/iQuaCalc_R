# [Alk] module functions for
# "iQuaCalc (Lite) salinity module.R"

# NB: Split Layout to conform with WQ Map UI for init & target waypoints


alkInitialWpModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('30%', '70%'),
                
                numericInput(ns('alk_numeric_input'), '[Alk]', 
                             min = 0, max = 45, value = 33, step = 0.1),
                
                selectInput(ns('alkConvertUnits'), '[Alk] Units', 
                            choices = alkUnitsList)
                
    )
    
  )
}



alkInitialWpModule <- function(input, output, session, 
                           icTemp, icSal, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       alk_units_default = 'meq/kg (mmol/kg)',
                       alk_sl_init = -1,
                       alk_default = c(2.4, 2.4, 120, 120, 8.0))
  
  
  
  # see: http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
  #      (Scott Ritchie)
  # to strip HTML from icSal()
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  
  # ---- ALKALINITY ----
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$alkConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('alkConvertUnits')
      
      rv$select_init <- 1
      
      alk_units_init <- st()[[x]]
      
      if(length(alk_units_init) == 0)
        alk_units_init <- rv$alk_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'alkConvertUnits', '[Alk] Units',
                        # choices=alkUnitsList,
                        choices=alkUnitsList,
                        selected = alk_units_init)
    }
    
    updateStore(session, session$ns("alkConvertUnits"), input$alkConvertUnits)
    
    idx <- which(input$alkConvertUnits == alkUnitsList)
    
    y <- paste0(session$ns('sl_'), input$alkConvertUnits)
    
    my_alk_value <- st()[[y]]
    
    if(length(my_alk_value) == 0)
      my_alk_value <- rv$alk_default[idx]
    
    # updateNumericInput(session, "alk_numeric_input", label = alkUnitsList[idx],
    updateNumericInput(session, "alk_numeric_input", label = '[Alk]',
                      value = my_alk_value,
                      min = alkMin[idx], max = alkMax[idx], step = alkStep[idx])
    
    freezeReactiveValue(input, "alk_numeric_input")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$alkConvertUnits), my_alk_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$alk_numeric_input, {
    
    if(rv$alk_sl_init < 0) {
      
      rv$alk_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$alkConvertUnits == alkUnitsList)
    
    y <- paste0(session$ns('sl_'), input$alkConvertUnits)
    
    my_alk_value <- st()[[y]]
    
    
    if(length(my_alk_value) == 0)
      my_alk_value <- rv$alk_default[idx]
    else
      my_alk_value <- input$alk_numeric_input
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$alkConvertUnits), my_alk_value)
    
  })
  
  
  # sec.axis label & scale ----
  # used for WQ Map secondary axis
  
  alk_units_sec_label <- reactive({
    
    idx <- which(input$alkConvertUnits == alkUnitsList)
    
    if(1 == idx || 4 == idx) {
      my_units <- alkUnitsList_short[4]
      # my_units <- HTML(paste0('mg CaC)', tags$sub('3'), '/L'))
    } else if (2 == idx || 5 == idx) {
      my_units <- alkUnitsList_short[idx]
    } else {
      my_units <- alkUnitsList_short[3]
      # my_units <- HTML(paste0('mg CaC)', tags$sub('3'), '/kg'))
    }
    
    my_units <- paste0('Alkalinity (', my_units, ')')
    
  })
  
  # [To Do] relate to rho, etc. ----
  # ic units = meq/kg
  
  # alkUnitsList_short <- c('meq/kg', 'meq/L',
  #                         'ppm-m CaCO3', 'ppm-v CaCO3',
  #                         'dKH')
  # meq_L <- round(alk * rho, 4)
  # ppm_m <- round(alk * 50.04345, 3)
  # ppm_v <- round(alk * rho * 50.04345, 3)
  # dkh <- round(alk * 2.8, 4)
  # alk <- round(alk, 4)
  
  alk_units_scale_factor <- reactive({
    
    idx <- which(input$alkConvertUnits == alkUnitsList)
    
    my_rho <- calcRho(icTemp()$ic, icSal()$ic)
    my_rho <- my_rho / 1000.0 # [g/L]
    
    if(1 == idx || 4 == idx) {
      my_scale_factor <- my_rho * 50.04345  # --> ppm-v CaCO3 (mg/L)
    } else if (2 == idx) {
      my_scale_factor <- my_rho             # --> meq/L
    } else if (3 == idx) {
      my_scale_factor <- 50.04345           # --> ppm-m CaCO3 (mg/kg)
    } else {
      my_scale_factor <- 2.8                # --> dHK
    }
    
    my_scale_factor
    
  })
  
  
  
  df_alk <- reactive({
    
    
    idx <- which(input$alkConvertUnits == alkUnitsList)
    
    validate(
      
      need(try(
        
        input$alk_numeric_input >= alkMin[idx] && 
          input$alk_numeric_input <= alkMax[idx]
      ),
      
      paste0('Please enter an [Alk] value between ', alkMin[idx], ' and ', alkMax[idx]))
    )
    
    req(
      icTemp(),
      icSal(),
      cancelOutput = T
    )
    
    my_icTemp <- icTemp()$ic
    my_icSal <- icSal()$ic
    
    if(!is.null(my_icSal) && my_icSal != '' &&
       !is.null(my_icTemp) && my_icTemp != '') {
      
      idx_alk <- which(input$alkConvertUnits == alkUnitsList)
      
      ic_rho <- calcRho(my_icTemp, my_icSal)
      
      my_rho <- ic_rho / 1000.0   # in kg/L
      
      icAlk <- alkToIcUnits(input$alk_numeric_input, input$alkConvertUnits, my_rho)
      
      hidden.col <- c(rep(0, 5))
      hidden.col[idx_alk] <- 1
      
      df <- alkToAllUnits(icAlk, my_rho)
      
      df <- cbind(df, h = hidden.col)
      
      # as in "salinity_module.R," not just "as.data.frame(df)"
      alk_list <- list(df    = df, 
                       ic    = icAlk,
                       val   = input$alk_numeric_input,
                       units = input$alkConvertUnits,
                       
                       label        = alk_units_sec_label(),
                       scale_factor = alk_units_scale_factor()
                       )
      
      alk_list
    }
    
  })
  
  
  return(df_alk)
  
}