# HYDRAULIC LOAD module functions for
# "iQuaCalc (Lite).R"


hydraulicLoadModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    tabsetPanel(
      
      # 1. UI - CONVERT ----
      tabPanel(title = 'Hydraulic Load Rate ➔ Hydraulic Load Rate',
               
               hr(),
               tags$h3('Convert between Hydraulic Load Rate units', 
                       align = 'center', style = 'font-weight: bold;'),
               hr(),
               
               fluidRow(
                 
                 column( width = 6, offset = 3,
                         
                         wellPanel(style = 'padding: 5px; 0px; 5px; 25px;',
                                   
                                   splitLayout(cellWidths = c('25%', '50%', '25%'),
                                               
                                               numericInput(ns('hydraulic_load_input'), 'Hydraulic Load', 
                                                            value = 1, 
                                                            min = 0, max = 45, step = 0.01),
                                               
                                               selectInput(ns('hydraulic_load_ConvertUnits'), 'Hydraulic Load Units', 
                                                           
                                                           choices=list('per SECOND' = hydraulicUnits[1:5], 
                                                                        'per MINUTE' = hydraulicUnits[6:10], 
                                                                        'per HOUR'   = hydraulicUnits[11:15], 
                                                                        'per DAY'    = hydraulicUnits[16:20])
                                               ),
                                               
                                               tags$h6()
                                   )
                         )
                 )
               ),
               
               fluidRow(
                 
                 column(width = 3,
                        
                        h3('per SECOND', align = 'center'),
                        datatableModuleInput(ns('hydraulic_seconds_conversion_1_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        
                        h3('per MINUTE', align = 'center'),
                        datatableModuleInput(ns('hydraulic_minutes_conversion_2_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        style = 'padding: 0px',
                        
                        h3('per HOUR', align = 'center'),
                        datatableModuleInput(ns('hydraulic_hours_conversion_3_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        
                        h3('per DAY', align = 'center'),
                        datatableModuleInput(ns('hydraulic_days_conversion_4_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 )
               )    # END fluidRow DT
      ),  # END tabPanel 'Conversion'
      
      
      # 2. UI - Flow / A ➔ HLR ----
      tabPanel(title = 'Flow Rate / Area ➔ HLR',
               
               hr(),
               tags$h3('Calculate HYDRAULIC LOAD RATE from Flow Rate & Area', 
                       align = 'center', style = 'font-weight: bold;'),
               hr(),
               
               fluidRow(
                 
                 column( width = 6, offset = 3,
                         
                         wellPanel(style = 'padding: 5px; 0px; 5px; 25px;',
                                   
                                   # splitLayout(cellWidths = c('40%', '50%'),
                                   splitLayout(cellWidths = c('25%', '50%', '25%'),
                                               
                                               numericInput(ns('hydraulic_flow_rate_input'), 'Flow Rate', 
                                                            value = 1, 
                                                            min = 0, max = 45, step = 0.01),
                                               
                                               selectInput(ns('hydraulic_flow_rate_ConvertUnits'), 'Flow Rate Units', 
                                                           
                                                           choices=list('per SECOND' = flowRateUnitsList[19:24],
                                                                        'per MINUTE' = flowRateUnitsList[13:18],
                                                                        'per HOUR'   = flowRateUnitsList[7:12], 
                                                                        'per DAY'    = flowRateUnitsList[1:6])
                                               ),
                                               
                                               tags$h6()
                                   ),
                                   
                                   areaModuleInput(ns('hydraulic_area_input'))
                         )
                 )
               ),
               
               fluidRow(
                 
                 column(width = 3,
                        
                        h3('per SECOND', align = 'center'),
                        datatableModuleInput(ns('hydraulic_conversion_1_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        
                        h3('per MINUTE', align = 'center'),
                        datatableModuleInput(ns('hydraulic_conversion_2_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        style = 'padding: 0px',
                        
                        h3('per HOUR', align = 'center'),
                        datatableModuleInput(ns('hydraulic_conversion_3_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        
                        h3('per DAY', align = 'center'),
                        datatableModuleInput(ns('hydraulic_conversion_4_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 )
               )    # END fluidRow DT
      ),  # END tabPanel '(Flow Rate) / (Area)'
      
      
      # 3. UI - HLR * A ➔ FLOW ----
      tabPanel(title = 'HLR * Area ➔ Flow Rate',
               
               hr(),
               tags$h3('Calculate FLOW RATE from Hydraulic Load Rate & Area',
                       align = 'center', style = 'font-weight: bold;'),
               hr(),
               
               fluidRow(
                 
                 column( width = 6, offset = 3,
                         
                         wellPanel(style = 'padding: 5px; 0px; 5px; 25px;',
                                   
                                   splitLayout(cellWidths = c('25%', '50%', '25%'),
                                               
                                               numericInput(ns('hlr_input_for_flow'), 'Hydraulic Load',
                                                            value = 1,
                                                            min = 0, max = 45, step = 0.01),

                                               selectInput(ns('hlr_ConvertUnits_for_flow'), 'Hydraulic Load Units',

                                                           choices=list('per SECOND' = hydraulicUnits[1:5],
                                                                        'per MINUTE' = hydraulicUnits[6:10],
                                                                        'per HOUR'   = hydraulicUnits[11:15],
                                                                        'per DAY'    = hydraulicUnits[16:20])
                                               ),
                                               
                                               tags$h6()
                                   ),
                                   
                                   areaModuleInput(ns('hlr_for_flow_area_input'))
                         )
                 )
               ),
               
               fluidRow(
                 
                 column(width = 3,
                        
                        h3('per SECOND', align = 'center'),
                        datatableModuleInput(ns('hlr_for_flow_1_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        
                        h3('per MINUTE', align = 'center'),
                        datatableModuleInput(ns('hlr_for_flow_2_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        style = 'padding: 0px',
                        
                        h3('per HOUR', align = 'center'),
                        datatableModuleInput(ns('hlr_for_flow_3_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 ),
                 
                 column(width = 3,
                        
                        h3('per DAY', align = 'center'),
                        datatableModuleInput(ns('hlr_for_flow_4_dt'),
                                             col_width = 12,
                                             font_size = '105')
                 )
               )    # END fluidRow DT
      ),
      
      
      # 4. UI - Flow / HLR ➔ AREA ----
      tabPanel(title = 'Flow Rate / HLR ➔ Area',
               
               hr(),
               tags$h3('Calculate AREA from Flow Rate & Hydraulic Load Rate', 
                       align = 'center', style = 'font-weight: bold;'),
               hr(),
               
               fluidRow(
                 
                 column( width = 6, offset = 3,
                         
                         wellPanel(style = 'padding: 5px; 0px; 5px; 25px;',
                                   
                                   # splitLayout(cellWidths = c('40%', '50%'),
                                   splitLayout(cellWidths = c('25%', '50%', '25%'),
                                               
                                               numericInput(ns('hlr_flow_for_area_input'), 'Flow Rate', 
                                                            value = 1, 
                                                            min = 0, max = 45, step = 0.01),
                                               
                                               selectInput(ns('hlr_flow_for_area_ConvertUnits'), 'Flow Rate Units', 
                                                           
                                                           choices=list('per SECOND' = flowRateUnitsList[19:24],
                                                                        'per MINUTE' = flowRateUnitsList[13:18],
                                                                        'per HOUR'   = flowRateUnitsList[7:12], 
                                                                        'per DAY'    = flowRateUnitsList[1:6])
                                               ),
                                               
                                               tags$h6()
                                   ),
                                   
                                   splitLayout(cellWidths = c('25%', '50%', '25%'),
                                               
                                               numericInput(ns('hlr_for_area_input'), 'Hydraulic Load',
                                                            value = 1,
                                                            min = 0, max = 45, step = 0.01),
                                               
                                               selectInput(ns('hlr_for_area_ConvertUnits'), 'Hydraulic Load Units',
                                                           
                                                           choices=list('per SECOND' = hydraulicUnits[1:5],
                                                                        'per MINUTE' = hydraulicUnits[6:10],
                                                                        'per HOUR'   = hydraulicUnits[11:15],
                                                                        'per DAY'    = hydraulicUnits[16:20])
                                               ),
                                               
                                               tags$h6()
                                   )
                         )
                 )
               ),
               
               fluidRow(
                 
                 column(width = 4
                 ),
                 
                 column(width = 4,
                        
                        # h3('Area', align = 'center'),
                        datatableModuleInput(ns('hlr_for_area_1_dt'),
                                             col_width = 12,
                                             font_size = '135')
                 ),
                 
                 column(width = 4
                 )
               )    # END fluidRow DT
      ),
      
      
      tabPanel(title = 'HLR Info',
               
               column(width = 1),
               
               column(width = 10,
                      
                      box(
                        width = '400px',
                        solidHeader = T,
                        status = 'primary',
                        background = 'light-blue',
                        
                        # hr(),
                        
                        wellPanel(
                          style = "background-color: #000;",
                          
                          h4(tags$li('Hydraulic Load Rate (HLR) is used in design of water-treatment processes, such as...')),
                          h4(tags$ul('...gas exchangers')),
                          h4(tags$ul('...biofilters')),
                          h4(tags$ul('...constructed wetlands')),
                          
                          # hr(),
                          
                          h4(tags$li('HLR is defined as the ratio of flow rate to surface area')),
                          
                          h4(tags$li('flow rate refers to the volume of water delivered per unit time')),
                          
                          h4(tags$li('(in some cases, flow rate is in terms of mass per unit time)')),
                          
                          h4(tags$li('surface area is the exposed area contacted by the input flow')),
                          
                          h4(tags$li('e.g., 200 m³/day pumped over 4 m² ➔ HLR of 50 (m³/d)/m², or 50 m/d')),
                          
                          hr(),
                          
                          h4(tags$li('common metric units:  (m³/day)/m²')),
                          
                          h4(tags$li('common English units: (gal/min)/ft²')),
                          
                          h4(tags$li('values range from HLR < 1 to HLR > 1000, depending upon the process'))
                        )
                        
                      )      # END box
               ),
               
               column(width = 1)
               
      )  # END tabPanel 'Hydraulic Load Info'
      
    )  # END tabsetPanel
    
  )   # END tagList
}




hydraulicLoadModule <- function(input, output, session, st) {
  
  ns <- session$ns
  
  # REACTIVE_VALUES ----
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(
                       # 1. hydraulic load CONVERT
                       hydraulic_convert_select_init = -1,
                       hydraulic_convert_units_default = '(m³/d) / m²',
                       hydraulic_convert_sl_init = -1,
                       hydraulic_convert_default = c(rep(1, 20)),
                       
                       # 2. calc: Flow Rate / Area ➔ HLR'
                       flow_rate_select_init = -1,
                       flow_rate_units_default = 'm³/min',
                       flow_rate_sl_init = -1,
                       flow_rate_default = c(rep(1, 24)),
                       
                       # 3. calc: HLR * Area ➔ Flow Rate
                       # hlr_input_for_flow & hlr_ConvertUnits_for_flow
                       hlr_for_flow_select_init = -1,
                       hlr_for_flow_units_default = '(m³/d) / m²',
                       hlr_for_flow_sl_init = -1,
                       hlr_for_flow_default = c(rep(1, 20)),
                       
                       # 4a. calc: *Flow Rate* / HLR ➔ Area
                       # hlr_flow_for_area_input & hlr_flow_for_area_ConvertUnits
                       flow_for_area_select_init = -1,
                       flow_for_area_units_default = 'm³/d',
                       flow_for_area_sl_init = -1,
                       flow_for_area_default = c(rep(1, 24)),
                       
                       # 4b. calc: Flow Rate / *HLR* ➔ Area
                       # hlr_input_for_flow & hlr_ConvertUnits_for_flow
                       hlr_for_area_select_init = -1,
                       hlr_for_area_units_default = '(m³/d) / m²',
                       hlr_for_area_sl_init = -1,
                       hlr_for_area_default = c(rep(1, 20))
                       )
  
  # Auxiliary Modules ----
  
  # 1. AUX mods CONVERT ----
  
  # send rows 1:5 to datatableModule
  hydraulic_convert_dt_1 <- callModule(datatable_split_Module, 'dummy_hydraulic_convert_1',
                                       reactive(df_hydraulic_convert()),
                                       1, 5 # row numbers to send
  )
  
  # send rows 6:10 to datatableModule
  hydraulic_convert_dt_2 <- callModule(datatable_split_Module, 'dummy_hydraulic_convert_2',
                                       reactive(df_hydraulic_convert()),
                                       6, 10 # row numbers to send
  )
  
  # send rows 11:15 to datatableModule
  hydraulic_convert_dt_3 <- callModule(datatable_split_Module, 'dummy_hydraulic_convert_3',
                                       reactive(df_hydraulic_convert()),
                                       11, 15 # row numbers to send
  )
  
  # send rows 16:20 to datatableModule
  hydraulic_convert_dt_4 <- callModule(datatable_split_Module, 'dummy_hydraulic_convert_4',
                                       reactive(df_hydraulic_convert()),
                                       16, 20 # row numbers to send
  )


  callModule(datatableModule,
             'hydraulic_seconds_conversion_1_dt',
             reactive(hydraulic_convert_dt_1()),
             5, 3 # nrow, ncol
  )

  callModule(datatableModule,
             'hydraulic_minutes_conversion_2_dt',
             reactive(hydraulic_convert_dt_2()),
             5, 3 # nrow, ncol
  )

  callModule(datatableModule,
             'hydraulic_hours_conversion_3_dt',
             reactive(hydraulic_convert_dt_3()),
             5, 3 # nrow, ncol
  )

  callModule(datatableModule,
             'hydraulic_days_conversion_4_dt',
             reactive(hydraulic_convert_dt_4()),
             5, 3 # nrow, ncol
  )
  
  
  
  
  # 2. AUX mods calc HLR ----
  
  ic_area <- callModule(areaModule, 'hydraulic_area_input',
                        reactive(st()))
  
  # send rows 1:5 to datatableModule
  hydraulic_dt_1 <- callModule(datatable_split_Module, 'dummy_hydraulic_1',
                              reactive(df_hydraulic()),
                              1, 5 # row numbers to send
  )

  # send rows 6:10 to datatableModule
  hydraulic_dt_2 <- callModule(datatable_split_Module, 'dummy_hydraulic_2',
                              reactive(df_hydraulic()),
                              6, 10 # row numbers to send
  )

  # send rows 11:15 to datatableModule
  hydraulic_dt_3 <- callModule(datatable_split_Module, 'dummy_hydraulic_3',
                               reactive(df_hydraulic()),
                               11, 15 # row numbers to send
  )

  # send rows 16:20 to datatableModule
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
  
  
  
  # 3. AUX mods calc FLOW ----
  
  ic_area_flow <- callModule(areaModule, 'hlr_for_flow_area_input',
                             reactive(st()))
  
  # send rows 1:5 to datatableModule
  hlr_flow_dt_1 <- callModule(datatable_split_Module, 'dummy_hlr_flow_1',
                               reactive(df_hydraulic_flow()),
                               1, 5 # row numbers to send
  )

  # send rows 6:10 to datatableModule
  hlr_flow_dt_2 <- callModule(datatable_split_Module, 'dummy_hlr_flow_2',
                               reactive(df_hydraulic_flow()),
                               6, 10 # row numbers to send
  )

  # send rows 11:15 to datatableModule
  hlr_flow_dt_3 <- callModule(datatable_split_Module, 'dummy_hlr_flow_3',
                               reactive(df_hydraulic_flow()),
                               11, 15 # row numbers to send
  )

  # send rows 16:20 to datatableModule
  hlr_flow_dt_4 <- callModule(datatable_split_Module, 'dummy_hlr_flow_4',
                               reactive(df_hydraulic_flow()),
                               16, 20 # row numbers to send
  )


  callModule(datatableModule,
             'hlr_for_flow_1_dt',
             reactive(hlr_flow_dt_1()),
             5, 3 # nrow, ncol
  )

  callModule(datatableModule,
             'hlr_for_flow_2_dt',
             reactive(hlr_flow_dt_2()),
             5, 3 # nrow, ncol
  )

  callModule(datatableModule,
             'hlr_for_flow_3_dt',
             reactive(hlr_flow_dt_3()),
             5, 3 # nrow, ncol
  )

  callModule(datatableModule,
             'hlr_for_flow_4_dt',
             reactive(hlr_flow_dt_4()),
             5, 3 # nrow, ncol
  )
  
  
  
  # 4. AUX mods calc AREA ----
  
  callModule(datatableModule,
             'hlr_for_area_1_dt',
             reactive(df_hydraulic_area()),
             8, 3 # nrow, ncol
  )
  
  
  # Observers ----
  
  # 1. HYDRAULIC Convert controls ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$hydraulic_load_ConvertUnits, priority = 100, {

    if(rv$hydraulic_convert_select_init < 0)  {

      x <- ns('hydraulic_load_ConvertUnits')

      rv$hydraulic_convert_select_init <- 1

      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "hydraulic_load_ConvertUnits",
                        label = 'Hydraulic Load Units',
                        
                        choices=list('per SECOND' = hydraulicUnits[1:5], 
                                     'per MINUTE' = hydraulicUnits[6:10], 
                                     'per HOUR'   = hydraulicUnits[11:15], 
                                     'per DAY'    = hydraulicUnits[16:20]),

                        selected = st()[[x]])
    }

    updateStore(session, ns("hydraulic_load_ConvertUnits"), input$hydraulic_load_ConvertUnits)


    idx <- which(input$hydraulic_load_ConvertUnits == hydraulicUnits)

    y <- paste0(ns('sl_hydraulic_load_convert_'), input$hydraulic_load_ConvertUnits)


    my_hydraulic_load_convert_value <- st()[[y]]

    if(length(my_hydraulic_load_convert_value) == 0)
      my_hydraulic_load_convert_value <- rv$hydraulic_convert_default[idx]
    
    # cat('UNITS...in hydraulic_load_module.R...\n')
    # cat('input$hydraulic_load_ConvertUnits = ', input$hydraulic_load_ConvertUnits, '\n')
    # cat('                                y = ', y, '\n')
    # cat('  my_hydraulic_load_convert_value = ', my_hydraulic_load_convert_value, '\n')
    # cat('       input$hydraulic_load_input = ', input$hydraulic_load_input, '\n\n\n')

    updateNumericInput(session, 
                       'hydraulic_load_input', 
                       hydraulicUnits[idx],
                       value = my_hydraulic_load_convert_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
    )

    # update slider value for current units ???
    updateStore(session, 
                paste0(ns('sl_hydraulic_load_convert_'), input$hydraulic_load_ConvertUnits), 
                my_hydraulic_load_convert_value)

  })


  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$hydraulic_load_input, {

    if(rv$hydraulic_convert_sl_init < 0) {

      rv$hydraulic_convert_sl_init <- 1

      # return()
    }

    idx <- which(input$hydraulic_load_ConvertUnits == hydraulicUnits)  # previously: flow_rateUnitsChoices

    y <- paste0(ns('sl_hydraulic_load_convert_'), input$hydraulic_load_ConvertUnits)

    my_hydraulic_convert_value <- st()[[y]]


    if(length(my_hydraulic_convert_value) == 0)
      my_hydraulic_convert_value <- rv$hydraulic_convert_default[idx]
    else
      my_hydraulic_convert_value <- input$hydraulic_load_input
    
    # cat('VALUE...in hydraulic_load_module.R...\n')
    # cat('input$hydraulic_load_ConvertUnits = ', input$hydraulic_load_ConvertUnits, '\n')
    # cat('                                y = ', y, '\n')
    # cat('       my_hydraulic_convert_value = ', my_hydraulic_convert_value, '\n')
    # cat('       input$hydraulic_load_input = ', input$hydraulic_load_input, '\n\n\n')

    # update input value for current units
    updateStore(session, 
                paste0(ns('sl_hydraulic_load_convert_'), input$hydraulic_load_ConvertUnits), 
                my_hydraulic_convert_value)

  })
  
  
  # -----------*
  
  
  # 2. HYDRAULIC Calc controls ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$hydraulic_flow_rate_ConvertUnits, priority = 100, {
    
    if(rv$flow_rate_select_init < 0)  {
      
      x <- ns('hydraulic_flow_rate_ConvertUnits')
      
      rv$flow_rate_select_init <- 1
      
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
      my_flow_rate_value <- rv$flow_rate_default[idx]
    
    updateNumericInput(session, 
                       'hydraulic_flow_rate_input', 
                       flowRateUnitsList[idx], 
                       value = my_flow_rate_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
    )
    
    # update slider value for current units ???
    updateStore(session, 
                paste0(ns('sl_hydraulic_'), input$hydraulic_flow_rate_ConvertUnits), 
                my_flow_rate_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$hydraulic_flow_rate_input, {
    
    if(rv$flow_rate_sl_init < 0) {
      
      rv$flow_rate_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$hydraulic_flow_rate_ConvertUnits == flowRateUnitsList)  # previously: flow_rateUnitsChoices
    
    y <- paste0(ns('sl_hydraulic_'), input$hydraulic_flow_rate_ConvertUnits)
    
    my_flow_rate_value <- st()[[y]]
    
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$flow_rate_default[idx]
    else
      my_flow_rate_value <- input$hydraulic_flow_rate_input
    
    # update input value for current units
    updateStore(session, 
                paste0(ns('sl_hydraulic_'), input$hydraulic_flow_rate_ConvertUnits), 
                my_flow_rate_value)
    
  })
  
  
  # -----------*
  
  
  # 3. FLOW Calc controls ----
  # hlr_for_flow...
  # hlr_input_for_flow & hlr_ConvertUnits_for_flow
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$hlr_ConvertUnits_for_flow, priority = 100, {
    
    if(rv$hlr_for_flow_select_init < 0)  {
      
      x <- ns('hlr_ConvertUnits_for_flow')
      
      rv$hlr_for_flow_select_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "hlr_ConvertUnits_for_flow",
                        label = 'Hydraulic Load Units',
                        
                        choices=list('per SECOND' = hydraulicUnits[1:5], 
                                     'per MINUTE' = hydraulicUnits[6:10], 
                                     'per HOUR'   = hydraulicUnits[11:15], 
                                     'per DAY'    = hydraulicUnits[16:20]),
                        
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("hlr_ConvertUnits_for_flow"), input$hlr_ConvertUnits_for_flow)
    
    
    idx <- which(input$hlr_ConvertUnits_for_flow == hydraulicUnits)
    
    y <- paste0(ns('sl_hydraulic_hlr_for_flow_'), input$hlr_ConvertUnits_for_flow)
    
    
    my_flow_rate_value <- st()[[y]]
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$hlr_for_flow_default[idx]
    
    updateNumericInput(session, 
                       'hlr_input_for_flow', 
                       hydraulicUnits[idx], 
                       value = my_flow_rate_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
    )
    
    # update slider value for current units ???
    updateStore(session, 
                paste0(ns('sl_hydraulic_hlr_for_flow_'), input$hlr_ConvertUnits_for_flow), 
                my_flow_rate_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$hlr_input_for_flow, {
    
    if(rv$hlr_for_flow_sl_init < 0) {
      
      rv$hlr_for_flow_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$hlr_ConvertUnits_for_flow == hydraulicUnits)  # previously: flow_rateUnitsChoices
    
    y <- paste0(ns('sl_hydraulic_hlr_for_flow_'), input$hlr_ConvertUnits_for_flow)
    
    my_flow_rate_value <- st()[[y]]
    
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$hlr_for_flow_default[idx]
    else
      my_flow_rate_value <- input$hlr_input_for_flow
    
    # update input value for current units
    updateStore(session, 
                paste0(ns('sl_hydraulic_hlr_for_flow_'), input$hlr_ConvertUnits_for_flow), 
                my_flow_rate_value)
    
  })
  
  
  # -----------*
  
  
  # 4a. AREA Calc controls ----
  
  # 4a. calc: *Flow Rate* / HLR ➔ Area
  # hlr_flow_for_area_input
  # hlr_flow_for_area_ConvertUnits
  
  # flow_for_area_select_init = -1,
  # flow_for_area_units_default = 'm³/d',
  # flow_for_area_sl_init = -1,
  # flow_for_area_default = c(rep(1, 24))
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$hlr_flow_for_area_ConvertUnits, priority = 100, {
    
    if(rv$flow_for_area_select_init < 0)  {
      
      x <- ns('hlr_flow_for_area_ConvertUnits')
      
      rv$flow_for_area_select_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      
      updateSelectInput(session, "hlr_flow_for_area_ConvertUnits",
                        label = 'Flow Rate Units', 
                        
                        choices=list('per SECOND' = flowRateUnitsList[19:24],
                                     'per MINUTE' = flowRateUnitsList[13:18],
                                     'per HOUR'   = flowRateUnitsList[7:12], 
                                     'per DAY'    = flowRateUnitsList[1:6]),
                        
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("hlr_flow_for_area_ConvertUnits"), input$hlr_flow_for_area_ConvertUnits)
    
    
    idx <- which(input$hlr_flow_for_area_ConvertUnits == flowRateUnitsList)
    
    y <- paste0(ns('sl_hlr_flow_for_area_ConvertUnits_'), input$hlr_flow_for_area_ConvertUnits)
    
    
    my_flow_rate_value <- st()[[y]]
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$flow_for_area_default[idx]
    
    updateNumericInput(session, 
                       'hlr_flow_for_area_input', 
                       flowRateUnitsList[idx], 
                       value = my_flow_rate_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
    )
    
    # update slider value for current units ???
    updateStore(session, 
                paste0(ns('sl_hlr_flow_for_area_ConvertUnits_'), input$hlr_flow_for_area_ConvertUnits), 
                my_flow_rate_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$hlr_flow_for_area_input, {
    
    if(rv$flow_for_area_sl_init < 0) {
      
      rv$flow_for_area_sl_init <- 1
      
      # return()
    }
    
    idx <- which(input$hlr_flow_for_area_ConvertUnits == flowRateUnitsList)  # previously: flow_rateUnitsChoices
    
    y <- paste0(ns('sl_hlr_flow_for_area_ConvertUnits_'), input$hlr_flow_for_area_ConvertUnits)
    
    my_flow_rate_value <- st()[[y]]
    
    
    if(length(my_flow_rate_value) == 0)
      my_flow_rate_value <- rv$flow_for_area_default[idx]
    else
      my_flow_rate_value <- input$hlr_flow_for_area_input
    
    # update input value for current units
    updateStore(session, 
                paste0(ns('sl_hlr_flow_for_area_ConvertUnits_'), input$hlr_flow_for_area_ConvertUnits), 
                my_flow_rate_value)
    
  })
  
  
  # -----------*
  
  
  # 4b. AREA Calc controls ----
  
  # 4b. calc: Flow Rate / *HLR* ➔ Area
  # hlr_for_area_input
  # hlr_for_area_ConvertUnits
  
  # hlr_for_area_select_init = -1,
  # hlr_for_area_units_default = '(m³/d) / m²',
  # hlr_for_area_sl_init = -1,
  # hlr_for_area_default = c(rep(1, 20))
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$hlr_for_area_ConvertUnits, priority = 100, {

    if(rv$hlr_for_area_select_init < 0)  {

      x <- ns('hlr_for_area_ConvertUnits')

      rv$hlr_for_area_select_init <- 1

      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'hlr_for_area_ConvertUnits', 
                        label = 'Hydraulic Load Units',
                  
                        choices = list('per SECOND' = hydraulicUnits[1:5],
                                       'per MINUTE' = hydraulicUnits[6:10],
                                       'per HOUR'   = hydraulicUnits[11:15],
                                       'per DAY'    = hydraulicUnits[16:20]),
                        
                        selected = st()[[x]])
    }

    updateStore(session, ns("hlr_for_area_ConvertUnits"), input$hlr_for_area_ConvertUnits)


    idx <- which(input$hlr_for_area_ConvertUnits == hydraulicUnits)

    y <- paste0(ns('sl_hlr_for_area_ConvertUnits_'), input$hlr_for_area_ConvertUnits)


    my_hlr_value <- st()[[y]]
    
    # cat('0. stored HLR value, st[[', y, ']] = ', st()[[y]], '\n')

    if(length(my_hlr_value) == 0)
      my_hlr_value <- rv$hlr_for_area_default[idx]

    updateNumericInput(session,
                       'hlr_for_area_input',
                       label = hydraulicUnits[idx],
                       value = my_hlr_value
                       # min = flow_rateMin[idx], max = flow_rateMax[idx], step = flow_rateStep[idx]
    )

    # update slider value for current units ???
    updateStore(session,
                paste0(ns('sl_hlr_for_area_ConvertUnits_'), input$hlr_for_area_ConvertUnits),
                my_hlr_value)

  })


  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$hlr_for_area_input, {

    if(rv$hlr_for_area_sl_init < 0) {

      rv$hlr_for_area_sl_init <- 1

      # return()
    }

    idx <- which(input$hlr_for_area_ConvertUnits == hydraulicUnits)  # previously: flow_rateUnitsChoices

    y <- paste0(ns('sl_hlr_for_area_ConvertUnits_'), input$hlr_for_area_ConvertUnits)

    my_hlr_value <- st()[[y]]
    
    # cat('1. stored HLR value, st[[', y, ']] = ', st()[[y]], '\n')


    if(length(my_hlr_value) == 0)
      my_hlr_value <- rv$hlr_for_area_default[idx]
    else
      my_hlr_value <- input$hlr_for_area_input
    
    # cat('1. my_hlr_value = ', my_hlr_value, '\n\n\n')

    # update input value for current units
    updateStore(session,
                paste0(ns('sl_hlr_for_area_ConvertUnits_'), input$hlr_for_area_ConvertUnits),
                my_hlr_value)

  })
  
  
  
  # Calculations ----
  
  # 1. DF_HYDRAULIC convert ----
  
  df_hydraulic_convert <- reactive({
    
    req(
      input$hydraulic_load_input, input$hydraulic_load_ConvertUnits,
      
      cancelOutput = T
    )
    
    icHydraulicLoad <- getInIcUnits(input$hydraulic_load_input, input$hydraulic_load_ConvertUnits, hydraulic.data)
    
    df <- convertAll(icHydraulicLoad, '(m³/d) / m²', hydraulic.data)
    
    # format decimal values
    df <- round_values(df)
    
    # print(df)
    # cat('\n\n\n')
    
    hidden.col <- c(rep(0, nrow(df)))
    # [KLUDGE] set df hidden.col to '0',
    # as displayed units different from either input units (area & flow rate)
    # hidden.col[idx_flow_rate] <- 1
    
    
    hydraulic_convert_df <- cbind(df, h = hidden.col)
    
    hydraulic_convert_list <- list(df = hydraulic_convert_df)
                           # ic    = icFlowRate,
                           # val   = input$hydraulic_flow_rate_input,
                           # units = input$hydraulic_flow_rate_ConvertUnits)
    
    hydraulic_convert_list
    
  })
  
  
  #  2. DF_HYDRAULIC calc ----
  
  df_hydraulic <- reactive({
    
    req(
      input$hydraulic_flow_rate_input, 
      input$hydraulic_flow_rate_ConvertUnits,
      ic_area(),
      
      cancelOutput = T
    )
    
    icFlowRate <- getInIcUnits(input$hydraulic_flow_rate_input, 
                               input$hydraulic_flow_rate_ConvertUnits, 
                               flow_rate.data)
    
    df <- convertAll(icFlowRate, 'm³/d', flow_rate.data)
    
    # cat('Flow Rate Data...\n')
    # cat('       input$hydraulic_flow_rate_input = ', input$hydraulic_flow_rate_input, ' \n')
    # cat('input$hydraulic_flow_rate_ConvertUnits = ', input$hydraulic_flow_rate_ConvertUnits, ' \n')
    # print(df)
    # cat('-------------------\n\n')
    # cat('AREA = ', ic_area()$df$vals[2], ' m\n')
    # cat('AREA = ', ic_area()$df$vals[7], ' ft\n')
    
    
    # FLOW RATE UNITS
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
    # NB: for now, NOT USING acre-ft / [day, hour, minute, sec]
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
    df_hydraulic_per_second_metric  <- tibble(vals  = df$vals[19:20] / my_square_meters,
                                              units = c('(m³/sec) / m²', '(L/sec) / m²'))
    
    # c("gal/s (US)/ft²", "gal/s (UK)/ft²", "ft³/s/ft²")
    df_hydraulic_per_second_english <- tibble(vals  = df$vals[21:23] / my_square_feet,
                                              units = c('(gal (US)/sec) / ft²', "(gal (UK)/sec) / ft²", "(ft³/sec) / ft²"))
    
    
    
    hydraulic_tibble <- bind_rows(df_hydraulic_per_second_metric, df_hydraulic_per_second_english, 
                                  df_hydraulic_per_minute_metric, df_hydraulic_per_minute_english,
                                  df_hydraulic_per_hour_metric,   df_hydraulic_per_hour_english,
                                  df_hydraulic_per_day_metric,    df_hydraulic_per_day_english)
    
    
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
    
    hydraulic_list
    
  })
  
  
  observe({
    df_hydraulic_flow()
  })
  
  
  # 3. DF_FLOW calc ----
  # HLR * Area ➔ Flow Rate
  
  df_hydraulic_flow <- reactive({
    
    req(
      input$hlr_input_for_flow, 
      input$hlr_ConvertUnits_for_flow,
      ic_area_flow(),
      
      cancelOutput = T
    )
    
    icHlr <- getInIcUnits(input$hlr_input_for_flow, 
                          input$hlr_ConvertUnits_for_flow, 
                          hydraulic.data)
    
    df <- convertAll(icHlr, '(m³/d) / m²', hydraulic.data)
    
    my_square_meters <- as.numeric(ic_area_flow()$df$vals[2])
    my_square_feet   <- as.numeric(ic_area_flow()$df$vals[7])
    
    
    
    # HLR...
    
    # c('(m³/s) / m²',   '(L/s) / m²',   '(gal (US)/s) / ft²',   '(gal (UK)/s) / ft²',   '(ft³/s) / ft²',
    #   '(m³/min) / m²', '(L/min) / m²', '(gal (US)/min) / ft²', '(gal (UK)/min) / ft²', '(ft³/min) / ft²',
    #   '(m³/hr) / m²',  '(L/hr) / m²',  '(gal (US)/hr / ft²',   '(gal (UK)/hr) / ft²',  '(ft³/hr) / ft²',
    #   '(m³/d) / m²',   '(L/d) / m²',   '(gal (US)/d) / ft²',   '(gal (UK)/d) / ft²',   '(ft³/d) / ft²')
    
    # ...times AREA...
    
    # ...equals FLOW RATE...
    
    # c("m³/d",   "liter/d",   "gal/d (US)",   "gal/d (UK)",   "ft³/d",   "acre-ft/d",
    #   "m³/hr",  "liter/hr",  "gal/hr (US)",  "gal/hr (UK)",  "ft³/hr",  "acre-ft/hr", 
    #   "m³/min", "liter/min", "gal/min (US)", "gal/min (UK)", "ft³/min", "acre-ft/min", 
    #   "m³/sec", "liter/sec", "gal/sec (US)", "gal/sec (UK)", "ft³/sec", "acre-ft/sec")
    
    df_hydraulic_per_day_metric  <- tibble(vals  = df$vals[1:2] * my_square_meters, 
                                           units = c('m³/d', 'L/d'))
    
    # c("gal/d (US)/ft²", "gal/d (UK)/ft²", "ft³/d/ft²")
    df_hydraulic_per_day_english <- tibble(vals  = df$vals[3:5] * my_square_feet, 
                                           units = c('gal (US)/d', 'gal (UK)/d', 'ft³/d'))
    
    
    # HYDRAULIC -- per HOUR
    # c("m³/hr/m²", "liter/hr/m²")
    df_hydraulic_per_hour_metric  <- tibble(vals  = df$vals[6:7] * my_square_meters,
                                            units = c('m³/hr', 'L/hr'))
    
    # c("gal/hr (US)/ft²", "gal/hr (UK)/ft²", "ft³/hr/ft²")
    df_hydraulic_per_hour_english <- tibble(vals  = df$vals[8:10] * my_square_feet,
                                            units = c('gal (US)/hr', 'gal (UK)/hr', 'ft³/hr'))
    
    
    # HYDRAULIC -- per MINUTE
    # c("m³/min/m²", "liter/min/m²")
    df_hydraulic_per_minute_metric  <- tibble(vals  = df$vals[11:12] * my_square_meters,
                                              units = c('m³/min', 'L/min'))
    
    # c("gal/min (US)/ft²", "gal/min (UK)/ft²", "ft³/min/ft²")
    df_hydraulic_per_minute_english <- tibble(vals  = df$vals[13:15] * my_square_feet,
                                              units = c('gal (US)/min', "gal (UK)/min", "ft³/min"))
    
    
    # HYDRAULIC -- per SECOND
    # c("m³/s/m²", "liter/s/m²")
    df_hydraulic_per_second_metric  <- tibble(vals  = df$vals[16:17] * my_square_meters,
                                              units = c('m³/sec', 'L/sec'))
    
    # c("gal/s (US)/ft²", "gal/s (UK)/ft²", "ft³/s/ft²")
    df_hydraulic_per_second_english <- tibble(vals  = df$vals[18:20] * my_square_feet,
                                              units = c('gal/sec (US)', "gal/sec (UK)", "ft³/sec"))
                                              # units = c('gal (US)/sec', "gal (UK)/sec", "ft³/sec"))
    
    
    flow_tibble <- bind_rows(df_hydraulic_per_second_metric, df_hydraulic_per_second_english, 
                             df_hydraulic_per_minute_metric, df_hydraulic_per_minute_english,
                             df_hydraulic_per_hour_metric,   df_hydraulic_per_hour_english,
                             df_hydraulic_per_day_metric,    df_hydraulic_per_day_english)
    
    
    # format decimal values
    flow_tibble <- round_values(flow_tibble)
    
    idx_hlr <- which(input$hlr_ConvertUnits_for_flow == hydraulicUnits)
    
    hidden.col <- c(rep(0, nrow(flow_tibble)))
    # [KLUDGE] set df hidden.col to '0',
    # as displayed units different from either input units (area & flow rate)
    # hidden.col[idx_flow_rate] <- 1
    
    
    flow_tibble <- cbind(flow_tibble, h = hidden.col)
    
    # print(flow_tibble)
    # cat('=======================\n\n\n')
    
    
    flow_list <- list(df    = flow_tibble, 
                      ic    = icHlr,
                      val   = input$hlr_input_for_flow,
                      units = input$hlr_ConvertUnits_for_flow)
    
    
    flow_list
    
  })
  
  
  # 4. DF_AREA calc ----
  # Flow Rate / HLR ➔ Area'
  
  df_hydraulic_area <- reactive({
    
    req(
      input$hlr_flow_for_area_input, 
      input$hlr_flow_for_area_ConvertUnits,
      
      input$hlr_for_area_input, 
      input$hlr_for_area_ConvertUnits,
      
      cancelOutput = T
    )
    
    # get...all ??...FLOW data
    icFlow <- getInIcUnits(input$hlr_flow_for_area_input, 
                           input$hlr_flow_for_area_ConvertUnits, 
                           flow_rate.data)
    
    # df_flow <- convertAll(icFlow, 'm³/d', flow_rate.data)
    
    
    # get...all ??...HLR data
    icHlr <- getInIcUnits(input$hlr_for_area_input, 
                          input$hlr_for_area_ConvertUnits, 
                          hydraulic.data)
    
    # df_hlr <- convertAll(icHlr, '(m³/d) / m²', hydraulic.data)
    
    
    my_square_meters <- icFlow / icHlr # [m³/d] / [(m³/d) / m²] -> m²
    
    
    # cat('\n++++++++++++++++++++++++++++++\n')
    # cat(input$hlr_flow_for_area_input, ' ', input$hlr_flow_for_area_ConvertUnits,
    #     ' -> ', icFlow, ' m³/d \n')
    # 
    # cat(input$hlr_for_area_input, ' ', input$hlr_for_area_ConvertUnits,
    #     ' -> ', icHlr, ' (m³/d) / m² \n')
    # 
    # cat('So...AREA = ', my_square_meters, ' m² \n')
    # cat('++++++++++++++++++++++++++++++\n\n')
    
    
    # Check Values
    # ++++++++++++++++++++++++++++++
    #   1   m³/sec  ->  86400.03  m³/d 
    # 1   (m³/s) / m²  ->  86400.03  (m³/d) / m² 
    # So...AREA =  1  m² 
    # ++++++++++++++++++++++++++++++
    #   
    #   ++++++++++++++++++++++++++++++
    #   1   m³/sec  ->  86400.03  m³/d 
    # 1   (L/s) / m²  ->  86.40003  (m³/d) / m² 
    # So...AREA =  1000  m² 
    # ++++++++++++++++++++++++++++++
    #   
    #   ++++++++++++++++++++++++++++++
    #   1   m³/sec  ->  86400.03  m³/d 
    # 1   (m³/min) / m²  ->  1440  (m³/d) / m² 
    # So...AREA =  60.00002  m² 
    # ++++++++++++++++++++++++++++++
    #   
    #   ++++++++++++++++++++++++++++++
    #   1   m³/min  ->  1440  m³/d 
    # 1   (m³/min) / m²  ->  1440  (m³/d) / m² 
    # So...AREA =  0.9999999  m² 
    # ++++++++++++++++++++++++++++++
    #   
    #   ++++++++++++++++++++++++++++++
    #   1   m³/min  ->  1440  m³/d 
    # 1   (gal (US)/min) / ft²  ->  58.67395  (m³/d) / m² 
    # So...AREA =  24.54241  m² 
    # ++++++++++++++++++++++++++++++
    
    
    df_area <- convertAll(my_square_meters, 'm²', area.data)
    
    df_area <- df_area %>% filter(units %in% c('ha','m²', 'acre','yd²','ft²'))
    
    df_area <- round_values(df_area)
    
    # FLOW RATE...
    
    # c("m³/d",   "liter/d",   "gal/d (US)",   "gal/d (UK)",   "ft³/d",   "acre-ft/d",
    #   "m³/hr",  "liter/hr",  "gal/hr (US)",  "gal/hr (UK)",  "ft³/hr",  "acre-ft/hr", 
    #   "m³/min", "liter/min", "gal/min (US)", "gal/min (UK)", "ft³/min", "acre-ft/min", 
    #   "m³/sec", "liter/sec", "gal/sec (US)", "gal/sec (UK)", "ft³/sec", "acre-ft/sec")
    
    # ...divided by HLR...
    
    # c('(m³/s) / m²',   '(L/s) / m²',   '(gal (US)/s) / ft²',   '(gal (UK)/s) / ft²',   '(ft³/s) / ft²',
    #   '(m³/min) / m²', '(L/min) / m²', '(gal (US)/min) / ft²', '(gal (UK)/min) / ft²', '(ft³/min) / ft²',
    #   '(m³/hr) / m²',  '(L/hr) / m²',  '(gal (US)/hr / ft²',   '(gal (UK)/hr) / ft²',  '(ft³/hr) / ft²',
    #   '(m³/d) / m²',   '(L/d) / m²',   '(gal (US)/d) / ft²',   '(gal (UK)/d) / ft²',   '(ft³/d) / ft²')
    
    # ...equals AREA...
    
    # c("ha","m²","cm²","mm²",
    #   "acre","yd²","ft²","in²")
    
    
    hidden.col <- c(rep(0, nrow(df_area)))
    
    df_area <- cbind(df_area, h = hidden.col)
    
    
    area_list <- list(df    = df_area) 
                      # ic    = icHlr,
                      # val   = input$hlr_input_for_flow,
                      # units = input$hlr_ConvertUnits_for_flow)
    
    area_list
    
  })
  
  
  # NB: ORIGINAL module returned df for flow-rate (only) calc
  # return(df_flow_rate)
}
