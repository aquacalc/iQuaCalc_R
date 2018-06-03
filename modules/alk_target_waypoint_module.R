# [Alk] module functions for
# "iQuaCalc (Lite) salinity module.R"

# NB: Split Layout to conform with WQ Map UI for init & target waypoints


alkTargetWpModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('30%', '70%'),
                
                numericInput(ns('alk_numeric_input'), '[Alk]', 
                             min = 0, max = 45, value = 33, step = 0.1),
                
                tags$div(
                  
                  style = 'padding: 12px 0px 2px 8px;',
                  
                  htmlOutput(ns('alk_convert_units'))
                )
                
                # selectInput(ns('alkConvertUnits'), '[Alk] Units', 
                #             choices = alkUnitsList)
                
    )
    
  )
}



alkTargetWpModule <- function(input, output, session, 
                              initial_alk_units,       # this additional input echoed by target alk
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
  
  
  # render 'echo' alk units ----
  
  output$alk_convert_units <- renderUI({
      
    str_br <- paste0('<br>')
    
    str_echo_alk_units <- paste0(initial_alk_units()$units)
    # str_echo_alk_units <- paste0('ppm-m CaCO3 (mg/kg)')
    
      HTML(paste0(str_br, str_echo_alk_units))
    
  })
  
  
  # ---- ALKALINITY ----
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(initial_alk_units()$units, priority = 50, {
    
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
    
    updateStore(session, session$ns("alkConvertUnits"), initial_alk_units()$units)
    
    idx <- which(initial_alk_units()$units == alkUnitsList)
    
    y <- paste0(session$ns('sl_'), initial_alk_units()$units)
    
    my_alk_value <- st()[[y]]
    
    if(length(my_alk_value) == 0)
      my_alk_value <- rv$alk_default[idx]
    
    # updateNumericInput(session, "alk_numeric_input", label = alkUnitsList[idx],
    updateNumericInput(session, "alk_numeric_input", label = '[Alk]',
                      value = my_alk_value,
                      min = alkMin[idx], max = alkMax[idx], step = alkStep[idx])
    
    freezeReactiveValue(input, "alk_numeric_input")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), initial_alk_units()$units), my_alk_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$alk_numeric_input, {
    
    if(rv$alk_sl_init < 0) {
      
      rv$alk_sl_init <- 1
      
      return()
    }
    
    idx <- which(initial_alk_units()$units == alkUnitsList)
    
    y <- paste0(session$ns('sl_'), initial_alk_units()$units)
    
    my_alk_value <- st()[[y]]
    
    
    if(length(my_alk_value) == 0)
      my_alk_value <- rv$alk_default[idx]
    else
      my_alk_value <- input$alk_numeric_input
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), initial_alk_units()$units), my_alk_value)
    
  })
  
  
  
  df_alk <- reactive({
    
    
    idx <- which(initial_alk_units()$units == alkUnitsList)
    
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
      
      idx_alk <- which(initial_alk_units()$units == alkUnitsList)
      
      ic_rho <- calcRho(my_icTemp, my_icSal)
      
      my_rho <- ic_rho / 1000.0   # in kg/L
      
      icAlk <- alkToIcUnits(input$alk_numeric_input, initial_alk_units()$units, my_rho)
      
      hidden.col <- c(rep(0, 5))
      hidden.col[idx_alk] <- 1
      
      df <- alkToAllUnits(icAlk, my_rho)
      
      df <- cbind(df, h = hidden.col)
      
      # as in "salinity_module.R," not just "as.data.frame(df)"
      alk_list <- list(df = df, 
                       ic = icAlk,
                       val = input$alk_numeric_input,
                       units = initial_alk_units()$units
                       )
      
      alk_list
    }
    
  })
  
  
  return(df_alk)
  
}