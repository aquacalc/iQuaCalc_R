# depth module functions for
# "iQuaCalc (Lite)  module.R"


depth_ModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                # tags$h4('Gas Solubilities at ', align = 'right'),
                
                numericInput(ns('depth_z'), 'Depth', 
                             value = 0, min = 0, max = 30, step = 0.1),
                
                selectInput(ns('depth_z_units'), 'Water Depth Units', 
                            choices = c('m', 'ft'), selected = 'm')
                
    )
    
  )
}



# NB: unlike co2_gas_module.R, do not need temp, sal, pH to convert between atmospheric CO2 units
depth_Module <- function(input, output, session, 
                         st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init_depth_z = -1,
                       depth_z_units_default = 'm',
                       depth_z_sl_init = -1,
                       depth_z_default = c(0, 0))
  
  
  # ---- Depth Input update ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$depth_z_units, priority = 50, {
    
    if(rv$select_init_depth_z < 0)  {
      
      x <- session$ns('depth_z_units')
      
      rv$select_init_depth_z <- 1
      
      depth_z_units_init <- st()[[x]]
      
      if(length(depth_z_units_init) == 0)
        depth_z_units_init <- rv$depth_z_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'depth_z_units', 'Water Depth Units', 
                        choices = c('m', 'ft'),
                        selected = depth_z_units_init)
      
      freezeReactiveValue(input, "depth_z_units")
    }
    
    updateStore(session, session$ns("depth_z_units"), input$depth_z_units)
    
    # gas_sat DEPTH UNITS ----
    my_depth_units <- c('m', 'ft')
    
    idx <- which(input$depth_z_units == my_depth_units)
    
    y <- paste0(session$ns('sl_'), input$depth_z_units)
    
    my_depth_z_value <- st()[[y]]
    
    if(length(my_depth_z_value) == 0)
      my_depth_z_value <- rv$depth_z_default[idx]
    
    updateNumericInput(session, "depth_z", label = paste0(my_depth_units[idx]),
                       value = my_depth_z_value,
                       min = 0, max = 50, step = 0.01)
    
    freezeReactiveValue(input, "depth_z")
    
    # update slider value for current units ???
    updateStore(session, y, my_depth_z_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  # observeEvent(c(input$gasSlider_convert, icTemp(), icSal()), {
  observeEvent(input$depth_z, {
    
    if(rv$depth_z_sl_init < 0) {
      
      rv$depth_z_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$depth_z_units == c('m', 'ft'))
    
    y <- paste0(session$ns('sl_'), input$depth_z_units)
    
    my_depth_z_value <- st()[[y]]
    
    
    if(length(my_depth_z_value) == 0)
      my_depth_z_value <- rv$depth_z_default[idx]
    else
      my_depth_z_value <- input$depth_z
    
    # update slider value for current units
    # updateStore(session, paste0(session$ns('sl_'), input$co2_gasConvertUnits), my_co2_gas_value)
    updateStore(session, y, my_depth_z_value)
    
    freezeReactiveValue(input, "my_depth_z_value")
    
  })
  
  
  df_depth <- reactive({
    
    # *** Now, depth only 'm' & 'ft' ----
    
    # ad hoc conversion of 'ft' to 'm', when required...
    if('ft' == input$depth_z_units)
      my_depth_z <- 0.3048 * input$depth_z
    else
      my_depth_z <- input$depth_z  # in meters
    
    # NB: return I.C. depth units, meters
    my_depth_z
    
  })
  
  
  return(df_depth)
  
}