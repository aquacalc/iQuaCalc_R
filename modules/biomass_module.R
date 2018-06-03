# BIOMASS module functions for
# "iQuaCalc (Lite).R"


biomassModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
              
    fluidRow(
      
      column(width = 3,
             
             numericInput(ns('biomass_input'), 'Biomass',
                          value = 1,
                          min = 0, max = 1000, step = 0.01)
      ),
      
      column(width = 3,
             
             selectInput(ns('biomassConvertUnits'), 'Biomass Units', 
                         
                         choices=list('per UNIT AREA' = biomassUnits[1:8], 
                                      'per UNIT VOLUME' = biomassUnits[9:15]))
      ),
      
      column(width = 6,
             
             # tags$h4('DEPTH', align = 'center'),
             
             lengthModuleInput_uioutput_TWO(ns('depth_for_biomass'), 0) # '1' => show conversion DT; '0', hide
             
             # lengthModuleInput(ns('depth_for_biomass'), 'DEPTH')
      )
      
    ) # END fluidRow
  )   # END tagList
}




biomassModule <- function(input, output, session, st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       biomass_units_default = 'kg/m²',
                       biomass_sl_init = -1,
                       biomass_default = c(rep(1, 15))
                       )
  
  
  icDepth <- callModule(lengthModule_uioutput_TWO, 'depth_for_biomass',
                        reactive(st()))
  
  # icDepth <- callModule(lengthModule, 'depth_for_biomass',
  #                       'DEPTH',
  #                       reactive(st()))
  

  
  # Observe SELECT_INPUT input, store when changed ----
  observeEvent(input$biomassConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('biomassConvertUnits')
      
      rv$select_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "biomassConvertUnits",
                        label = 'Biomass Units', 
                        
                        choices=list('per UNIT AREA' = biomassUnits[1:8], 
                                     'per UNIT VOLUME' = biomassUnits[9:15]),
                        
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("biomassConvertUnits"), input$biomassConvertUnits)
    
    
    idx <- which(input$biomassConvertUnits == biomassUnits)
    
    y <- paste0(ns('sl_'), input$biomassConvertUnits)
    
    
    my_biomass_value <- st()[[y]]
    
    if(length(my_biomass_value) == 0)
      my_biomass_value <- rv$biomass_default[idx]
    
    updateNumericInput(session, 'biomass_input', biomassUnits[idx], 
                       value = my_biomass_value)
                       # min = biomassMin[idx], max = biomassMax[idx], step = biomassStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$biomassConvertUnits), my_biomass_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed ----
  observeEvent(input$biomass_input, {
    
    if(rv$biomass_sl_init < 0) {
      
      rv$biomass_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$biomassConvertUnits == biomassUnits)
    
    y <- paste0(ns('sl_'), input$biomassConvertUnits)
    
    my_biomass_value <- st()[[y]]
    
    
    if(length(my_biomass_value) == 0)
      my_biomass_value <- rv$biomass_default[idx]
    else
      my_biomass_value <- input$biomass_input
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$biomassConvertUnits), my_biomass_value)
    
  })

  
  # calc conversion data ----
  df_biomass <- reactive({
    
    req(
      input$biomass_input, input$biomassConvertUnits,
      icDepth(),
      cancelOutput = T
    )
    
    
    if(input$biomassConvertUnits %in% biomassUnits[1:8]) { # PER-UNIT-AREA selected
      
      # get icBiomass -- 'kg/m²' -- for all per-unit-area choices
      icBiomass_per_unit_area <- getInIcUnits(input$biomass_input, input$biomassConvertUnits, biomass.data)
      
      # convert choices biomassUnits[1:4]
      biomass_per_unit_area <- icBiomass_per_unit_area * biomassFactors[1:8]
      
      # DIVIDE icBiomass by icDepth -- 'm' -- as icDepth()$ic to get icBiomass_per_unit_volume
      icBiomass_per_unit_volume <- icBiomass_per_unit_area / icDepth()$ic
      
      # convert choices biomassUnits[5:9]
      biomass_per_unit_volume <- icBiomass_per_unit_volume * biomassFactors[9:15]
      
      df <- data.frame(vals = c(biomass_per_unit_area, biomass_per_unit_volume),
                      units = biomassUnits)
      
    } else { # PER-UNIT-VOLUME selected
      
      # get icBiomass -- 'kg/m³' -- for all per-unit-volume choices
      icBiomass_per_unit_volume <- getInIcUnits(input$biomass_input, input$biomassConvertUnits, biomass.data)
      
      # convert choices biomassUnits[5:9]
      biomass_per_unit_volume <- icBiomass_per_unit_volume * biomassFactors[9:15]
      
      # MULTIPLY icBiomass by icDepth -- 'm' -- as icDepth()$ic to get icBiomass_per_unit_area
      icBiomass_per_unit_area <- icBiomass_per_unit_volume * icDepth()$ic
      
      # convert choices biomassUnits[1:4]
      biomass_per_unit_area <- icBiomass_per_unit_area * biomassFactors[1:8]
      
      df <- data.frame(vals = c(biomass_per_unit_area, biomass_per_unit_volume),
                       units = biomassUnits)
      
    }
    
    
    # format decimal values
    df <- round_values(df)
    
    # format(df, digits = 6, scientific = F)
    
    
    idx_biomass <- which(input$biomassConvertUnits == biomassUnits)
    
    hidden.col <- c(rep(0, nrow(df)))
    hidden.col[idx_biomass] <- 1
       
    df <- cbind(df, h = hidden.col)
    
    biomass_list <- list(df = df, 
                      ic = 0,
                      val = input$biomass_input,
                      units = input$biomassConvertUnits)
    
    biomass_list
    
  })
  
  
  return(df_biomass)
}
