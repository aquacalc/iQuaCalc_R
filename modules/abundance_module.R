# ABUNDANCE module functions for
# "iQuaCalc (Lite).R"


abundanceModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
              
    fluidRow(
      
      column(width = 3,
             
             numericInput(ns('abundance_input'), 'Abundance',
                          value = 1,
                          min = 0, max = 1000, step = 0.01)
      ),
      
      column(width = 3,
             
             selectInput(ns('abundanceConvertUnits'), 'Abundance Units', 
                         
                         choices=list('per UNIT AREA' = abundanceUnits[1:4], 
                                      'per UNIT VOLUME' = abundanceUnits[5:10]))
      ),
      
      column(width = 6,
             
             # tags$h4('DEPTH', align = 'center'),
             
             lengthModuleInput_uioutput_TWO(ns('depth_for_abundance'), 0) # '1' => show conversion DT; '0', hide
             
             # lengthModuleInput(ns('depth_for_biomass'), 'DEPTH')
      )
      
    ) # END fluidRow
  )   # END tagList
}




abundanceModule <- function(input, output, session, st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       abundance_units_default = 'ind/m²',
                       abundance_sl_init = -1,
                       abundance_default = c(rep(1, 10))
                       )
  
  
  icDepth <- callModule(lengthModule_uioutput_TWO, 'depth_for_abundance',
                        reactive(st()))
  
  # icDepth <- callModule(lengthModule, 'depth_for_biomass',
  #                       'DEPTH',
  #                       reactive(st()))
  

  
  # Observe SELECT_INPUT input, store when changed ----
  observeEvent(input$abundanceConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('abundanceConvertUnits')
      
      rv$select_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "abundanceConvertUnits",
                        label = 'Abundance Units', 
                        
                        choices=list('per UNIT AREA' = abundanceUnits[1:4], 
                                     'per UNIT VOLUME' = abundanceUnits[5:10]),
                        
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("abundanceConvertUnits"), input$abundanceConvertUnits)
    
    
    idx <- which(input$abundanceConvertUnits == abundanceUnits)
    
    y <- paste0(ns('sl_'), input$abundanceConvertUnits)
    
    
    my_abundance_value <- st()[[y]]
    
    if(length(my_abundance_value) == 0)
      my_abundance_value <- rv$abundance_default[idx]
    
    updateNumericInput(session, 'abundance_input', abundanceUnits[idx], 
                       value = my_abundance_value)
                       # min = biomassMin[idx], max = biomassMax[idx], step = biomassStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$abundanceConvertUnits), my_abundance_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed ----
  observeEvent(input$abundance_input, {
    
    if(rv$abundance_sl_init < 0) {
      
      rv$abundance_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$abundanceConvertUnits == abundanceUnits)
    
    y <- paste0(ns('sl_'), input$abundanceConvertUnits)
    
    my_abundance_value <- st()[[y]]
    
    
    if(length(my_abundance_value) == 0)
      my_abundance_value <- rv$abundance_default[idx]
    else
      my_abundance_value <- input$abundance_input
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$abundanceConvertUnits), my_abundance_value)
    
  })

  
  # calc conversion data ----
  df_abundance <- reactive({
    
    req(
      input$abundance_input, input$abundanceConvertUnits,
      icDepth(),
      cancelOutput = T
    )
    
    
    # abundanceUnits <- c('ind/m²', 'ind/ft²', 'ind/ha', 'ind/acre',
    #                     'ind/m³', 'ind/L', 'ind/gal (US)', 'ind/gal (UK)', 
    #                     'ind/ft³', 'ind/acre-ft')
    
    if(input$abundanceConvertUnits %in% abundanceUnits[1:4]) { # PER-UNIT-AREA selected
      
      # get icAbundance -- 'ind/m²' -- for all (both) per-unit-area choices
      icAbundance_per_unit_area <- getInIcUnits(input$abundance_input, input$abundanceConvertUnits, abundance.data)
      
      # convert choices abundanceUnits[1:4]
      abundance_per_unit_area <- icAbundance_per_unit_area * abundanceFactors[1:4]
      
      # DIVIDE icBiomass by icDepth -- 'm' -- as icDepth()$ic to get icAbundance_per_unit_volume
      icAbundance_per_unit_volume <- icAbundance_per_unit_area / icDepth()$ic
      
      # convert choices abundanceUnits[5:10]
      abundance_per_unit_volume <- icAbundance_per_unit_volume * abundanceFactors[5:10]
      
      df <- data.frame(vals = c(abundance_per_unit_area, abundance_per_unit_volume),
                      units = abundanceUnits)
      
    } else { # PER-UNIT-VOLUME selected
      
      # get icBiomass -- 'kg/m³' -- for all per-unit-volume choices
      icAbundance_per_unit_volume <- getInIcUnits(input$abundance_input, input$abundanceConvertUnits, abundance.data)
      
      # convert choices abundanceUnits[5:10]
      abundance_per_unit_volume <- icAbundance_per_unit_volume * abundanceFactors[5:10]
      
      # MULTIPLY icBiomass by icDepth -- 'm' -- as icDepth()$ic to get icAbundance_per_unit_area
      icAbundance_per_unit_area <- icAbundance_per_unit_volume * icDepth()$ic
      
      # convert choices abundanceUnits[1:4]
      abundance_per_unit_area <- icAbundance_per_unit_area * abundanceFactors[1:4]
      
      df <- data.frame(vals = c(abundance_per_unit_area, abundance_per_unit_volume),
                       units = abundanceUnits)
      
    }
    
    
    # format decimal values
    df <- round_values(df)
    
    # format(df, digits = 6, scientific = F)
    
    
    idx_abundance <- which(input$abundanceConvertUnits == abundanceUnits)
    
    hidden.col <- c(rep(0, nrow(df)))
    hidden.col[idx_abundance] <- 1
       
    df <- cbind(df, h = hidden.col)
    
    abundance_list <- list(df = df, 
                      ic = 0,
                      val = input$abundance_input,
                      units = input$abundanceConvertUnits)
    
    abundance_list
    
  })
  
  
  return(df_abundance)
}
