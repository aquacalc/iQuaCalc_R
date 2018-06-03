# [Alk] module functions for
# "iQuaCalc (Lite) salinity module.R"


alkModuleInput <- function(id, show_tables) {
  
  ns <- NS(id)
  
  tagList(
    
    # fluidRow(
    #   column( width = 9,
    #           selectInput(ns('alkConvertUnits'), 'Alkalinity Units', 
    #                       choices=alkUnitsList)
    #           )
    #   ),
    # 
    # fluidRow(
    #   column( width = 11,
    #           sliderInput(ns('alkSlider_convert'), 'Alkalinity', 
    #                       min = 0, max = 45, value = 33, step = 0.1)
    #   )
    # ),
    
    splitLayout(cellWidths = c('35%', '65%'),
                
                numericInput(ns('alkSlider_convert'), 'Alkalinity', 
                             min = 0, max = 8, value = 2.3, step = 0.1),
                
                selectInput(ns('alkConvertUnits'), 'Alkalinity Units', 
                            choices = alkUnitsList)
                
    ),
    
    conditionalPanel(sprintf("%s > 0", show_tables), # if show_tables > 0, show temperature conversions DT
                     
                     fluidRow( width = 11, style = "padding: 0px 12px 0px 12px;",
                               
                               conversionDisplayModuleInput(ns('alk_conversion_result'))
                               
                     ),
                     
                     fluidRow( width = 11,
                               
                               datatableModuleInput(ns('alk_convert_dt'), 11, '130')
                     )
    )
    
    # fluidRow( width = 11, style = "padding: 0px 12px 0px 12px;",
    #           
    #           conversionDisplayModuleInput(ns('alk_conversion_result'))
    #           
    # ),
    # 
    # fluidRow( width = 11,
    #           
    #           datatableModuleInput(ns('alk_convert_dt'), 11, '130')
    # )
    
  )
}



alkModule <- function(input, output, session, 
                      icTemp, icSal, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       alk_units_default = 'meq/kg (mmol/kg)',
                       alk_sl_init = -1,
                       alk_default = alkSet)
  
  
  # ---- for displaying a single conversion result from table cell click
  callModule(conversionDisplayModule, 'alk_conversion_result',
             reactive(input$alkSlider_convert), 
             # reactive(input$alkConvertUnits),
             reactive(df_alk()$units), # look it: no need to send separately, as this & alkSlider_convert in df_alk
             reactive(df_alk()), 
             reactive(alk_dt()$info), # table id + "_cell_clicked'
             reactive(alk_dt()$selected),
             reactive(additional_conversion_info()),
             reactive(st()))
  
  
  # datatableModule displays conversion table
  alk_dt <- callModule(datatableModule,
                       'alk_convert_dt',
                       reactive(df_alk()),
                       5, 3)     # nrow, ncol
  
  
  # additional_conversion_info ----
  
  additional_conversion_info <- reactive({
    
    str_temp <- paste0(icTemp()$val, ' ', icTemp()$units)
    str_sal  <- paste0(icSal()$val, ' ', icSal()$units)
    # str_pH   <- paste0(icPh()$val, ' ', icPh()$units)
    
    HTML(paste(tags$h5('(', str_temp, ', ', str_sal, ')',
                       style = "text-align: center;"
                       )
               )
    )
    
    # NULL   # no additional info to display for temperature
    
  })
  
  
  
  # ---- ALKALINITY ----
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$alkConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('alkConvertUnits')
      
      rv$select_init <- 1
      
      # alk_units_init <- st()[[x]]
      # 
      # if(length(alk_units_init) == 0)
      #   alk_units_init <- rv$alk_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'alkConvertUnits', 'Alkalinity Units',
                        # choices=alkUnitsList,
                        choices=alkUnitsList,
                        selected = st()[[x]])
    }
    
    updateStore(session, session$ns("alkConvertUnits"), input$alkConvertUnits)
    
    idx <- which(input$alkConvertUnits == alkUnitsList)
    
    y <- paste0(session$ns('sl_'), input$alkConvertUnits)
    
    my_alk_value <- st()[[y]]
    
    if(length(my_alk_value) == 0)
      my_alk_value <- rv$alk_default[idx]
    # else
    #   my_alk_value <- input$alkSlider_convert
    
    updateNumericInput(session, "alkSlider_convert", label = alkUnitsList_short[idx],
                      value = my_alk_value,
                      min = alkMin[idx], max = alkMax[idx], step = alkStep[idx])
    
    freezeReactiveValue(input, "alkSlider_convert")
    
    # update slider value for current units ???
    # updateStore(session, paste0(session$ns('sl_'), input$alkConvertUnits), my_alk_value)
    # NB: cover case of re-launch with out-of-bounds temp ----
    
    if(input$alkSlider_convert < alkMin[idx] || input$alkSlider_convert > alkMax[idx])
      return()
    else
      updateStore(session, paste0(session$ns('sl_'), input$alkConvertUnits), my_alk_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$alkSlider_convert, {
    
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
      my_alk_value <- input$alkSlider_convert
    
    
    # update slider value for current units
    # updateStore(session, paste0(session$ns('sl_'), input$alkConvertUnits), my_alk_value)
    # NB: cover case of re-launch with out-of-bounds temp ----
    if(input$alkSlider_convert < alkMin[idx] || 
       input$alkSlider_convert > alkMax[idx] ||
       is.na(input$alkSlider_convert))
      return()
    else
      updateStore(session, paste0(session$ns('sl_'), input$alkConvertUnits), my_alk_value)
    
  })
  
  
  
  df_alk <- reactive({
    
    # ******************************* BEGIN VALIDATE
    idx_alk <- which(input$alkConvertUnits == alkUnitsList)
    
    alk.LL <- alkMin[idx_alk]
    alk.UU <- alkMax[idx_alk]
    alk.units.short <- alkUnitsList_short[idx_alk]
    
    str_message <- paste0('Please enter an [Alk] between ',
                          alk.LL, ' and ', alk.UU, ' ', alk.units.short)
    
    validate(

      need(

        try(

          input$alkSlider_convert >= alk.LL &&
            input$alkSlider_convert <= alk.UU
        ),

        str_message
      )
    )
    
    # ******************************* END VALIDATE
    
    # req(
    #   icTemp(),
    #   icSal(),
    #   cancelOutput = T
    # )
    
    my_icTemp <- icTemp()$ic
    my_icSal <- icSal()$ic
    
    # if(!is.null(my_icSal) && my_icSal != '' &&
    #    !is.null(my_icTemp) && my_icTemp != '') {
      
      idx_alk <- which(input$alkConvertUnits == alkUnitsList)
      
      ic_rho <- calcRho(my_icTemp, my_icSal)
      
      my_rho <- ic_rho / 1000.0   # in kg/L
      
      icAlk <- alkToIcUnits(input$alkSlider_convert, input$alkConvertUnits, my_rho)
      
      hidden.col <- c(rep(0, 5))
      hidden.col[idx_alk] <- 1
      
      df <- alkToAllUnits(icAlk, my_rho)
      
      df <- cbind(df, h = hidden.col)
      
      
      # want to display 'short' list from selectInput, alkUnitsList_short, as 'units'
      current_alk_units_short <- alkUnitsList_short[idx_alk]
      
      # as in "salinity_module.R," not just "as.data.frame(df)"
      alk_list <- list(df = df, 
                       ic = icAlk,
                       val = input$alkSlider_convert,
                       units = current_alk_units_short
                       # units = input$alkConvertUnits
                       )
      
      return(alk_list)
      
    # }
    
  })
  
  
  return(df_alk)
  
}