# FLOW RATE module functions for
# "iQuaCalc (Lite).R"


flowRateModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    tabsetPanel(

      tabPanel(title = 'Flow Rate ➔ Flow Rate',
               
               hr(),
               tags$h3('Convert FLOW RATE units',
                       align = 'center', style = 'font-weight: bold;'),
               hr(),
               
               fluidRow(
                 
                 column( width = 6, offset = 3,
                         
                         wellPanel(style = 'padding: 5px;',
                                   
                                   splitLayout(cellWidths = c('40%', '50%'),
                                               
                                               numericInput(ns('flow_rate_input'), 'Flow Rate', 
                                                            value = 1, 
                                                            min = 0, max = 45, step = 0.01),
                                               
                                               selectInput(ns('flow_rateConvertUnits'), 'Flow Rate Units', 
                                                           
                                                           choices=list('per SECOND' = flowRateUnitsList[19:24],
                                                                        'per MINUTE' = flowRateUnitsList[13:18],
                                                                        'per HOUR'   = flowRateUnitsList[7:12], 
                                                                        'per DAY'    = flowRateUnitsList[1:6])
                                               )
                                   )
                         )
                 )
               ),
               
               fluidRow(
                 
                 column(width = 3,
                        
                        h3('per SECOND', align = 'center'),
                        datatableModuleInput(ns('flow_rate_conversion_1_dt'),
                                             col_width = 12,
                                             font_size = '115')
                 ),
                 
                 column(width = 3,
                        
                        h3('per MINUTE', align = 'center'),
                        datatableModuleInput(ns('flow_rate_conversion_2_dt'),
                                             col_width = 12,
                                             font_size = '115')
                 ),
                 
                 column(width = 3,
                        style = 'padding: 0px',
                        
                        h3('per HOUR', align = 'center'),
                        datatableModuleInput(ns('flow_rate_conversion_3_dt'),
                                             col_width = 12,
                                             font_size = '115')
                 ),
                 
                 column(width = 3,
                        
                        h3('per DAY', align = 'center'),
                        datatableModuleInput(ns('flow_rate_conversion_4_dt'),
                                             col_width = 12,
                                             font_size = '115')
                 )
               )    # END fluidRow DT
      ),
      
      tabPanel(title = 'Volume / Time ➔ Flow Rate',
               
               hr(),
               tags$h3('Compute FLOW RATE as Volume divided by Time',
                       align = 'center', style = 'font-weight: bold;'),
               hr(),
               
               fluidRow(
                 
                 column( width = 6, offset = 3
                   
                 )
               )  # END fluidRow for Volume / Time ➔ Flow Rate'
        
      ),

      tabPanel(title = 'Volume / Flow Rate ➔ Turnover',
               
               hr(),
               tags$h3('Compute TURNOVER as Volume divided by Flow Rate',
                       align = 'center', style = 'font-weight: bold;'),
               hr(),
               
               fluidRow(
                 
                 column( width = 6, offset = 3,
                         
                         wellPanel(style = 'padding: 5px; 0px; 5px; 25px;',
                                   
                                   # areaModuleInput(ns('turnover_area_input')),
                                   
                                   volumeNumericModuleInput(ns('turnover_volume_input')),
                                   
                                   # splitLayout(cellWidths = c('40%', '50%'),
                                   splitLayout(cellWidths = c('25%', '50%', '25%'),
                                               
                                               numericInput(ns('turnover_flow_rate_input'), 'Flow Rate', 
                                                            value = 1, 
                                                            min = 0, max = 45, step = 0.01),
                                               
                                               selectInput(ns('turnover_flow_rate_ConvertUnits'), 'Flow Rate Units', 
                                                           
                                                           choices=list('per SECOND' = flowRateUnitsList[19:24],
                                                                        'per MINUTE' = flowRateUnitsList[13:18],
                                                                        'per HOUR'   = flowRateUnitsList[7:12], 
                                                                        'per DAY'    = flowRateUnitsList[1:6])
                                               ),
                                               
                                               tags$h6()
                                   )
                         )
                 )
               ),
               
               fluidRow(
                 
                 column(width = 2),
                 
                 column(width = 4,
                        
                        h3('Turnover Time', align = 'center'),
                        datatableModuleInput(ns('turnover_conversion_3_dt'),
                                             col_width = 12,
                                             font_size = '130')
                 ),
                 
                 column(width = 4,
                        
                        h3('Specific Turnover Rate', align = 'center'),
                        datatableModuleInput(ns('turnover_rate_conversion_4_dt'),
                                             col_width = 12,
                                             font_size = '130')
                 ),
                 
                 column(width = 2)
                 
               )    # END fluidRow DT
      )  # END tabPanel 2/2
      
    )  # END tabsetPanel
    
  )   # END tagList
}




flowRateModule <- function(input, output, session, st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       flow_rate_units_default = 'm³/d',
                       flow_rate_sl_init = -1,
                       flow_rate_default = c(rep(1, 24)),
                       
                       select_init_turnover = -1,
                       turnover_flow_rate_units_default = 'm³/d',
                       turnover_flow_rate_sl_init = -1,
                       turnover_flow_rate_default = c(rep(1, 24)),
                       
                       select_init_hydraulic = -1,
                       hydraulic_flow_rate_units_default = 'm³/d',
                       hydraulic_flow_rate_sl_init = -1,
                       hydraulic_flow_rate_default = c(rep(1, 24))
                       )
  
  
  # flow rate AUX modules ----
  
  # send rows 19:24 to datatableModule
  flow_rate_dt_1 <- callModule(datatable_split_Module, 'dummy_flow_rate_1', 
                               reactive(df_flow_rate()),
                               19, 24 # row numbers to send
  )
  
  # send rows 13:18 to datatableModule
  flow_rate_dt_2 <- callModule(datatable_split_Module, 'dummy_flow_rate_2', 
                               reactive(df_flow_rate()),
                               13, 18 # row numbers to send
  )
  
  # send rows 7:12 to datatableModule
  flow_rate_dt_3 <- callModule(datatable_split_Module, 'dummy_flow_rate_3', 
                               reactive(df_flow_rate()),
                               7, 12 # row numbers to send
  )
  
  # send rows 1:18 to datatableModule
  flow_rate_dt_4 <- callModule(datatable_split_Module, 'dummy_flow_rate_4', 
                               reactive(df_flow_rate()),
                               1, 6 # row numbers to send
  )
  
  callModule(datatableModule,
             'flow_rate_conversion_1_dt',
             reactive(flow_rate_dt_1()),
             6, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'flow_rate_conversion_2_dt',
             reactive(flow_rate_dt_2()),
             6, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'flow_rate_conversion_3_dt',
             reactive(flow_rate_dt_3()),
             6, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'flow_rate_conversion_4_dt',
             reactive(flow_rate_dt_4()),
             6, 3 # nrow, ncol
  )
  
  
  # turnover AUX modules ----
  
  ic_volume <- callModule(volumeNumericModule, 'turnover_volume_input', 
                          reactive(st()))
  
  # send rows 1:4 to datatableModule
  turnover_dt_3 <- callModule(datatable_split_Module, 'dummy_turnover_3', 
                              reactive(df_turnover()),
                              1, 4 # row numbers to send
  )
  
  # send rows 5:8 to datatableModule
  turnover_dt_4 <- callModule(datatable_split_Module, 'dummy_turnover_4',
                               reactive(df_turnover()),
                               5, 8 # row numbers to send
  )
  
  
  callModule(datatableModule,
             'turnover_conversion_3_dt',
             reactive(turnover_dt_3()),
             4, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'turnover_rate_conversion_4_dt',
             reactive(turnover_dt_4()),
             4, 3 # nrow, ncol
  )
  
  
  # hydraulic AUX modules ----
  
  ic_area <- callModule(areaModule, 'hydraulic_area_input',
                        reactive(st()))
  
  # send rows 1:5 to datatableModule
  hydraulic_dt_1 <- callModule(datatable_split_Module, 'dummy_hydraulic_1', 
                               reactive(df_hydraulic()),
                               1, 5 # row numbers to send
  )
  
  # send rows 1:5 to datatableModule
  hydraulic_dt_2 <- callModule(datatable_split_Module, 'dummy_hydraulic_2', 
                              reactive(df_hydraulic()),
                              6, 10 # row numbers to send
  )
  
  # send rows 6:10 to datatableModule
  hydraulic_dt_3 <- callModule(datatable_split_Module, 'dummy_hydraulic_3', 
                              reactive(df_hydraulic()),
                              11, 15 # row numbers to send
  )
  
  # send rows 11:15 to datatableModule
  hydraulic_dt_4 <- callModule(datatable_split_Module, 'dummy_hydraulic_4', 
                              reactive(df_hydraulic()),
                              16, 20 # row numbers to send
  )
  
  callModule(datatableModule,
             'hydraulic_conversion_1_dt',
             reactive(hydraulic_dt_1()),
             5, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'hydraulic_conversion_2_dt',
             reactive(hydraulic_dt_2()),
             5, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'hydraulic_conversion_3_dt',
             reactive(hydraulic_dt_3()),
             5, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'hydraulic_conversion_4_dt',
             reactive(hydraulic_dt_4()),
             5, 3 # nrow, ncol
  )

  
  
  # Observe FLOW-RATE controls ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$flow_rateConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('flow_rateConvertUnits')
      
      rv$select_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "flow_rateConvertUnits",
                        label = 'Flow Rate Units', 
                        # choices = flowRateUnitsList,
                        
                        choices=list('per SECOND' = flowRateUnitsList[19:24],
                                     'per MINUTE' = flowRateUnitsList[13:18],
                                     'per HOUR'   = flowRateUnitsList[7:12], 
                                     'per DAY'    = flowRateUnitsList[1:6]),
                        
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("flow_rateConvertUnits"), input$flow_rateConvertUnits)
    
    
    idx <- which(input$flow_rateConvertUnits == flowRateUnitsList)
    
    y <- paste0(ns('sl_'), input$flow_rateConvertUnits)
    
    
    my_flow_rate_value <- st()[[y]]
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$flow_rate_default[idx]
    
    updateNumericInput(session, 'flow_rate_input', flowRateUnitsList[idx], 
                       value = my_flow_rate_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
                       )
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$flow_rateConvertUnits), my_flow_rate_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed ----
  observeEvent(input$flow_rate_input, {
    
    if(rv$flow_rate_sl_init < 0) {
      
      rv$flow_rate_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$flow_rateConvertUnits == flowRateUnitsList)  # previously: flow_rateUnitsChoices
    
    y <- paste0(ns('sl_'), input$flow_rateConvertUnits)
    
    my_flow_rate_value <- st()[[y]]
    
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$flow_rate_default[idx]
    else
      my_flow_rate_value <- input$flow_rate_input
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$flow_rateConvertUnits), my_flow_rate_value)
    
  })

  
  # calc FLOW-RATE conversion data ----
  
  df_flow_rate <- reactive({
    
    req(
      input$flow_rate_input, input$flow_rateConvertUnits,
      cancelOutput = T
    )
    
    icFlowRate <- getInIcUnits(input$flow_rate_input, input$flow_rateConvertUnits, flow_rate.data)
    
    df <- convertAll(icFlowRate, 'm³/d', flow_rate.data)
    
    
    
    # format decimal values
    df <- round_values(df)
    
    
    idx_flow_rate <- which(input$flow_rateConvertUnits == flowRateUnitsList)
    
    hidden.col <- c(rep(0, nrow(df)))
    hidden.col[idx_flow_rate] <- 1
    
    # df_x <- data.frame(vals = c(rep(3, 9)), units = c(rep('w', 9)), stringsAsFactors = F)
       
    df <- cbind(df, h = hidden.col)
    
    flow_rate_list <- list(df = df, 
                      ic = icFlowRate,
                      val = input$flow_rate_input,
                      units = input$flow_rateConvertUnits)
    
    flow_rate_list
    
  })
  
  
  # Observe TURNOVER controls ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$turnover_flow_rate_ConvertUnits, priority = 100, {
    
    if(rv$select_init_turnover < 0)  {
      
      x <- ns('turnover_flow_rate_ConvertUnits')
      
      rv$select_init_turnover <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "turnover_flow_rate_ConvertUnits",
                        label = 'Flow Rate Units', 
                        
                        choices=list('per SECOND' = flowRateUnitsList[19:24],
                                     'per MINUTE' = flowRateUnitsList[13:18],
                                     'per HOUR'   = flowRateUnitsList[7:12], 
                                     'per DAY'    = flowRateUnitsList[1:6]),
                        
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("turnover_flow_rate_ConvertUnits"), input$turnover_flow_rate_ConvertUnits)
    
    
    idx <- which(input$turnover_flow_rate_ConvertUnits == flowRateUnitsList)
    
    y <- paste0(ns('sl_turnover_'), input$turnover_flow_rate_ConvertUnits)
    
    
    my_flow_rate_value <- st()[[y]]
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$turnover_flow_rate_default[idx]
    
    updateNumericInput(session, 'turnover_flow_rate_input', flowRateUnitsList[idx], 
                       value = my_flow_rate_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
    )
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_turnover_'), input$turnover_flow_rate_ConvertUnits), my_flow_rate_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed ----
  observeEvent(input$turnover_flow_rate_input, {
    
    if(rv$turnover_flow_rate_sl_init < 0) {
      
      rv$turnover_flow_rate_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$turnover_flow_rate_ConvertUnits == flowRateUnitsList)  # previously: flow_rateUnitsChoices
    
    y <- paste0(ns('sl_turnover_'), input$turnover_flow_rate_ConvertUnits)
    
    my_flow_rate_value <- st()[[y]]
    
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$turnover_flow_rate_default[idx]
    else
      my_flow_rate_value <- input$turnover_flow_rate_input
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_turnover_'), input$turnover_flow_rate_ConvertUnits), my_flow_rate_value)
    
  })
  
  
  
  # DF_TURNOVER conversion data ----
  
  df_turnover <- reactive({
    
    req(
      input$turnover_flow_rate_input, input$turnover_flow_rate_ConvertUnits,
      
      cancelOutput = T
    )
    
    icFlowRate <- getInIcUnits(input$turnover_flow_rate_input, input$turnover_flow_rate_ConvertUnits, flow_rate.data)
    
    # df <- convertAll(icFlowRate, 'm³/d', flow_rate.data)
    
    # format decimal values
    # df <- round_values(df)
    
    
    idx_flow_rate <- which(input$turnover_flow_rate_ConvertUnits == flowRateUnitsList)
    
    # NB: Not select a df row, just display...
    # hidden.col <- c(rep(0, nrow(df)))
    # hidden.col[idx_flow_rate] <- 1
    # hidden.col[idx_flow_rate] <- 0
    
    # df_x <- data.frame(vals = c(rep(3, 9)), units = c(rep('w', 9)), stringsAsFactors = F)
    
    # df <- cbind(df, h = hidden.col)
    
    # TURNoVER TIME
    days    <- (ic_volume()$ic / 1000.0) / icFlowRate
    hours   <- days * 24
    minutes <- hours * 60
    seconds <- minutes * 60
    
    # TURNOVERS PER PERIOD
    per_day <- 100 / days
    per_hr  <- 100 / hours
    per_min <- 100 / minutes
    per_sec <- 100 / seconds
    
    df_tau <- tibble(vals  = c( days,    hours,   minutes,   seconds,
                                per_day, per_hr,  per_min,   per_sec),
                     units = c('days',  'hours', 'minutes', 'seconds',
                               '%/day', '%/hr',  '%/min',   '%/sec'),
                     h     = c(rep(0, 8)))
    
    
    df_tau <- round_values(df_tau)
    
    turnover_list <- list(df      = df_tau,
                          ic      = icFlowRate,
                          val     = input$turnover_flow_rate_input,
                          units   = input$turnover_flow_rate_ConvertUnits,
                          days    = days,
                          hours   = hours,
                          minutes = minutes,
                          seconds = seconds)
    
    turnover_list
    
  })
  
  
  # Observe HYDRAULIC controls ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$hydraulic_flow_rate_ConvertUnits, priority = 100, {
    
    if(rv$select_init_hydraulic < 0)  {
      
      x <- ns('hydraulic_flow_rate_ConvertUnits')
      
      rv$select_init_hydraulic <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "hydraulic_flow_rate_ConvertUnits",
                        label = 'Flow Rate Units', 
                        
                        choices=list('per SECOND' = flowRateUnitsList[19:24],
                                     'per MINUTE' = flowRateUnitsList[13:18],
                                     'per HOUR'   = flowRateUnitsList[7:12], 
                                     'per DAY'    = flowRateUnitsList[1:6]),
                        
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("hydraulic_flow_rate_ConvertUnits"), input$hydraulic_flow_rate_ConvertUnits)
    
    
    idx <- which(input$hydraulic_flow_rate_ConvertUnits == flowRateUnitsList)
    
    y <- paste0(ns('sl_hydraulic_'), input$hydraulic_flow_rate_ConvertUnits)
    
    
    my_flow_rate_value <- st()[[y]]
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$hydraulic_flow_rate_default[idx]
    
    updateNumericInput(session, 'hydraulic_flow_rate_input', flowRateUnitsList[idx], 
                       value = my_flow_rate_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
    )
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_hydraulic_'), input$hydraulic_flow_rate_ConvertUnits), my_flow_rate_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed ----
  observeEvent(input$hydraulic_flow_rate_input, {
    
    if(rv$hydraulic_flow_rate_sl_init < 0) {
      
      rv$hydraulic_flow_rate_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$hydraulic_flow_rate_ConvertUnits == flowRateUnitsList)  # previously: flow_rateUnitsChoices
    
    y <- paste0(ns('sl_hydraulic_'), input$hydraulic_flow_rate_ConvertUnits)
    
    my_flow_rate_value <- st()[[y]]
    
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$hydraulic_flow_rate_default[idx]
    else
      my_flow_rate_value <- input$hydraulic_flow_rate_input
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_hydraulic_'), input$hydraulic_flow_rate_ConvertUnits), my_flow_rate_value)
    
  })
  
  
  
  # [DEPRECATED] calc HYDRAULIC ----

  df_hydraulic <- reactive({
    
    req(
      input$hydraulic_flow_rate_input, 
      input$hydraulic_flow_rate_ConvertUnits,

      cancelOutput = T
    )
    

    icFlowRate <- getInIcUnits(input$hydraulic_flow_rate_input, 
                               input$hydraulic_flow_rate_ConvertUnits, 
                               flow_rate.data)
    

    df <- convertAll(icFlowRate, 'm³/d', flow_rate.data)
    

    # icUnits = 'm³/d'
    # c("m³/d", "liter/d", "gal/d (US)", "gal/d (UK)", "ft³/d", "acre-ft/d",
    #   "m³/hr", "liter/hr", "gal/hr (US)", "gal/hr (UK)", "ft³/hr", "acre-ft/hr",
    #   "m³/min", "liter/min", "gal/min (US)", "gal/min (UK)", "ft³/min", "acre-ft/min")

    # icUnits = 'ha'
    # c("ha","m²","cm²","mm²",
    #   "acre","yd²","ft²","in²")

    # Want... (m³/d) / m² , (gal/min (US)) / ft² ,

    my_square_meters <- as.numeric(ic_area()$df$vals[2])
    my_square_feet   <- as.numeric(ic_area()$df$vals[7])
    

    # HYDRAULIC -- per DAY
    # c("m³/d/m²", "liter/d/m²")
    df_hydraulic_per_day_metric  <- tibble(vals  = df$vals[1:2] / my_square_meters,
                                           units = c('(m³/d) / m²', '(L/d) / m²'))

    # c("gal/d (US)/ft²", "gal/d (UK)/ft²", "ft³/d/ft²")
    df_hydraulic_per_day_english <- tibble(vals = df$vals[3:5] / my_square_feet,
                                           units = c('(gal (US)/d) / ft²', '(gal (UK)/d) / ft²', '(ft³/d) / ft²'))


    # HYDRAULIC -- per HOUR
    # c("m³/hr/m²", "liter/hr/m²")
    df_hydraulic_per_hour_metric  <- tibble(vals  = df$vals[7:8] / my_square_meters,
                                            units = c('(m³/hr) / m²', '(L/hr) / m²'))

    # c("gal/hr (US)/ft²", "gal/hr (UK)/ft²", "ft³/hr/ft²")
    df_hydraulic_per_hour_english <- tibble(vals  = df$vals[9:11] / my_square_feet,
                                            units = c('(gal (US)/hr / ft²', '(gal (UK)/hr) / ft²', '(ft³/hr) / ft²'))



    # HYDRAULIC -- per MINUTE
    # c("m³/min/m²", "liter/min/m²")
    df_hydraulic_per_minute_metric  <- tibble(vals  = df$vals[13:14] / my_square_meters,
                                              units = c('(m³/min) / m²', '(L/min) / m²'))

    # c("gal/min (US)/ft²", "gal/min (UK)/ft²", "ft³/min/ft²")
    df_hydraulic_per_minute_english <- tibble(vals  = df$vals[15:17] / my_square_feet,
                                              units = c('(gal (US)/min) / ft²', "(gal (UK)/min) / ft²", "(ft³/min) / ft²"))



    # HYDRAULIC -- per SECOND
    # c("m³/s/m²", "liter/s/m²")
    df_hydraulic_per_second_metric  <- tibble(vals  = df$vals[18:19] / my_square_meters,
                                              units = c('(m³/s) / m²', '(L/s) / m²'))

    # c("gal/min (US)/ft²", "gal/min (UK)/ft²", "ft³/min/ft²")
    df_hydraulic_per_second_english <- tibble(vals  = df$vals[20:22] / my_square_feet,
                                              units = c('(gal (US)/s) / ft²', "(gal (UK)/s) / ft²", "(ft³/s) / ft²"))


    hydraulic_tibble <- bind_rows(df_hydraulic_per_second_metric, df_hydraulic_per_second_english,
                                  df_hydraulic_per_minute_metric, df_hydraulic_per_minute_english,
                                  df_hydraulic_per_hour_metric,   df_hydraulic_per_hour_english,
                                  df_hydraulic_per_day_metric,    df_hydraulic_per_day_english)

    
    # cat('\n=============================\n')
    # # print(sapply(df_hydraulic_per_second_metric, class))
    # # print(sapply(df_hydraulic_per_second_english, class))
    # cat('\n-----------------------------\n')
    # # print(sapply(hydraulic_tibble, class))
    # cat('\n\n-+++++++++++++++++++++++-\n\n\n')
    

    # format decimal values
    hydraulic_tibble <- round_values(hydraulic_tibble)


    idx_flow_rate <- which(input$hydraulic_flow_rate_ConvertUnits == flowRateUnitsList)

    hidden.col <- c(rep(0, nrow(hydraulic_tibble)))
    # [KLUDGE] set df hidden.col to '0',
    # as displayed units different from either input units (area & flow rate)
    # hidden.col[idx_flow_rate] <- 1


    hydraulic_tibble <- cbind(hydraulic_tibble, h = hidden.col)

    # print(hydraulic_tibble)
    # cat('=======================\n\n\n')


    hydraulic_list <- list(df    = hydraulic_tibble,

                           ic    = icFlowRate,
                           val   = input$hydraulic_flow_rate_input,
                           units = input$hydraulic_flow_rate_ConvertUnits)
    
    
    cat('\n=======================\n')
    print(hydraulic_list)
    cat('=======================\n\n\n')
    
    hydraulic_list

  })
  
  
  
  # NB: ORIGINAL module returned df for flow-rate (only) calc
  return(df_flow_rate)
}
