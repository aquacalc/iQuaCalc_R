# [Alk] module functions for
# "iQuaCalc (Lite) salinity module.R"

# NB: Split Layout to conform with WQ Map UI for init & target waypoints


caNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                # numericInput(ns('ca_numeric_input'), HTML(paste0('Ca', tags$sup('++'))), 
                numericInput(ns('ca_numeric_input'), 'Ca++', 
                             min = 0, max = 600, value = 412, step = 0.1),
                
                selectInput(ns('caConvertUnits'), 'Ca++ Units', 
                            choices = caUnitsList),
                
                checkboxInput(ns('tc_show_omega_zone'), 'Show', value = F)
                
    )
    
  )
}



caNumericModule <- function(input, output, session, 
                            icTemp, icSal, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       ca_units_default = 'mg/kg',
                       ca_sl_init = -1,
                       ca_default = caSet
  )
  
  
  # # see: http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
  # #      (Scott Ritchie)
  # # to strip HTML from icSal()
  # cleanFun <- function(htmlString) {
  #   return(gsub("<.*?>", "", htmlString))
  # }
  
  
  # ---- Calcium ----
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$caConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('caConvertUnits')
      
      rv$select_init <- 1
      
      ca_units_init <- st()[[x]]
      
      if(length(ca_units_init) == 0)
        ca_units_init <- rv$ca_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'caConvertUnits', 'Ca++ Units',
                        # choices=caUnitsList,
                        choices  = caUnitsList,
                        selected = ca_units_init)
    }
    
    updateStore(session, session$ns("caConvertUnits"), input$caConvertUnits)
    
    idx <- which(input$caConvertUnits == caUnitsList)
    
    y <- paste0(session$ns('sl_'), input$caConvertUnits)
    
    my_ca_value <- st()[[y]]
    
    if(length(my_ca_value) == 0)
      my_ca_value <- rv$ca_default[idx]
    
    # updateNumericInput(session, "ca_numeric_input", label = caUnitsList[idx],
    updateNumericInput(session, "ca_numeric_input", label = caUnitsList[idx],
                      value = my_ca_value,
                      min = caMin[idx], max = caMax[idx], step = caStep[idx])
    
    freezeReactiveValue(input, "ca_numeric_input")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$caConvertUnits), my_ca_value)
    
  })
  
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$ca_numeric_input, {
    
    if(rv$ca_sl_init < 0) {
      
      rv$ca_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$caConvertUnits == caUnitsList)
    
    y <- paste0(session$ns('sl_'), input$caConvertUnits)
    
    my_ca_value <- st()[[y]]
    
    
    if(length(my_ca_value) == 0)
      my_ca_value <- rv$ca_default[idx]
    else
      my_ca_value <- input$ca_numeric_input
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$caConvertUnits), my_ca_value)
    
  })
  
  
  
  df_ca <- reactive({
    
    idx <- which(input$caConvertUnits == caUnitsList)
    
    validate(
      
      need(try(
        
        input$ca_numeric_input >= caMin[idx] && 
          input$ca_numeric_input <= caMax[idx]
      ),
      
      paste0('Please enter a Calcium value between ', caMin[idx], ' and ', caMax[idx]))
    )
    
    req(
      icTemp(),
      icSal(),
      cancelOutput = T
    )
    
    my_icTemp <- icTemp()$ic
    my_icSal  <- icSal()$ic
    
    
    # @my_rho in IC Units, g/L
    # convert to mg/L
    my_rho <- calcRho(my_icTemp, my_icSal)      # [g/L]
    
    my_input_units <- input$caConvertUnits
    my_input_val   <- input$ca_numeric_input
    
    # -----------------------------------------------------------
    # ad hoc [Ca++] conversion ----
    
    MW_CA <- 40.078   # [mg/mmol]
    
    if('mg/kg' == my_input_units) {
      my_input_val <- (my_input_val / MW_CA)
    }
    
    if('mg/L' == my_input_units) {
      my_input_val <- (my_input_val / MW_CA) / (0.001 * my_rho)
    }
    
    if('mmol/kg' == my_input_units) {
      my_input_val <- my_input_val
    }
    
    if('mmol/L' == my_input_units) {
      my_input_val <- my_input_val / (0.001 * my_rho)
    }
    
    # NB: CHANGE my_input_val from mmol/kg to mol/kg
    my_input_val <- my_input_val / 1000.0
    
    # -----------------------------------------------------------
    
    
    input$tc_show_omega_zone
    icCa <- input$ca_numeric_input
    input$caConvertUnits
    
    ca_list <- list(ic                  = my_input_val,
                    val                 = input$ca_numeric_input,
                    units               = input$caConvertUnits,
                    
                    show_omega_ca_zone  = input$tc_show_omega_zone)
  })
  
  
  return(df_ca)
  
}