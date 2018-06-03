# TAN module functions for
# "iQuaCalc (Lite)" dashboard


tanConversionModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column( width = 4,
              
              wellPanel(
                
                phModuleInput(ns('ph_inner_ammonia_convert')),
                
                hr(),
                
                temperatureModuleInput(ns('temp_inner_ammonia_convert'), 0),
                
                hr(),
                
                salinityModuleInput(ns('sal_inner_ammonia_convert'), 0)
              )
              
      ),   # END column 1 of 2
      
      column( width = 8,
              
              fluidRow(
                
                column( width = 8,
                        sliderInput(ns('ammoniaSlider_convert'), 'Ammonia', 
                                    min = 0, max = 45, value = 33, step = 0.1)
                ),
                
                # column( width = 3, offset = 9,
                column( width = 4, style="padding:0px;",
                        br(),
                        selectInput(ns('ammoniaConvertUnits'), 'Ammonia Units', 
                                    choices = list(
                                      'Total Ammonia-N' = tanUnitsList[1:8],
                                      'Un-ionized Ammonia-N' = tanUnitsList[9:16],
                                      # 'Ionized Ammonia-N' = tanUnitsList[13:18],
                                      
                                      # 'Total Ammonia' = tanUnitsList[19:24],
                                      # 'Un-ionized Ammonia' = tanUnitsList[25:30],
                                      'Total Ammonia' = tanUnitsList[17:24],
                                      'Un-ionized Ammonia' = tanUnitsList[25:32]
                                      # 'Ionized Ammonia' = tanUnitsList[31:36]
                                      )
                                    )
                )
              ),
              
              fluidRow(
                
                column(width = 12,
                  conversionDisplayModuleInput(ns('tan_conversion_result'))
                )
                
              ),
              
              fluidRow(
              
                column(width = 12,
                       div(DT::dataTableOutput(ns('ammonia_convert_dt')), 
                           style = 'font-size:130%')
                )
              )
        
      )  # END column 2 of 2
      
    )  # END fluidRow
    
  )
}



tanConversionModule <- function(input, output, session, st) {
  
  
  # ---- callModule for T, S, pH  ----
  icTemp <- callModule(temperatureModule, 'temp_inner_ammonia_convert', reactive(st()))
  icSal  <- callModule(salinityModule, 'sal_inner_ammonia_convert', reactive(icTemp()), reactive(st()))
  icPh   <- callModule(phModule, 'ph_inner_ammonia_convert', reactive(icTemp()), reactive(icSal()), reactive(st()))
  
  # ---- for displaying a single conversion result from table cell click
  callModule(conversionDisplayModule, 'tan_conversion_result',
             reactive(input$ammoniaSlider_convert), 
             reactive(input$ammoniaConvertUnits),
             reactive(df_tan()), 
             reactive(input$ammonia_convert_dt_cell_clicked), # table id + "_cell_clicked'
             reactive(input$ammonia_convert_dt_cells_selected),
             reactive(additional_conversion_info()),
             reactive(st()))
  
  
  
  # "*_init" flags when app is (re-)launched ----
  rv <- reactiveValues(select_init = -1,
                       tan_units_default = 'mg/L TA-N',
                       tan_sl_init = -1,
                       duct_tape_2 = -1,
                       tan_default = c(2, 2, 20, 20, 0.2, 0.2,
                                       1, 1, 10, 10, 0.1, 0.1,
                                       
                                       2, 2, 20, 20, 0.2, 0.2,
                                       1, 1, 10, 10, 0.1, 0.1
                                       )
                       )
  
  
  # ---- TAN ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$ammoniaConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('ammoniaConvertUnits')
      
      rv$select_init <- 1
      
      tan_units_init <- st()[[x]]
      
      if(length(tan_units_init) == 0)
        tan_units_init <- rv$tan_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'ammoniaConvertUnits', 'Ammonia Units', 
                        choices = list(
                          'Total Ammonia-N' = tanUnitsList[1:8],
                          'Un-ionized Ammonia-N' = tanUnitsList[9:16],
                          # 'Ionized Ammonia-N' = tanUnitsList[13:18],
                          
                          # 'Total Ammonia' = tanUnitsList[19:24],
                          # 'Un-ionized Ammonia' = tanUnitsList[25:30],
                          'Total Ammonia' = tanUnitsList[17:24],
                          'Un-ionized Ammonia' = tanUnitsList[25:32]
                          # 'Ionized Ammonia' = tanUnitsList[31:36]
                        ),
                        selected = tan_units_init)
    }
    
    updateStore(session, session$ns("ammoniaConvertUnits"), input$ammoniaConvertUnits)
    
    idx <- which(input$ammoniaConvertUnits == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), input$ammoniaConvertUnits)
    
    my_tan_value <- st()[[y]]
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    
    updateSliderInput(session, "ammoniaSlider_convert", label = tanUnitsList[idx],
                      value = my_tan_value,
                      min = tanUnitsMin[idx], max = tanUnitsMax[idx], step = tanUnitsStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$ammoniaConvertUnits), my_tan_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$ammoniaSlider_convert, {
    
    if(rv$tan_sl_init < 0) {
      
      rv$tan_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$ammoniaConvertUnits == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), input$ammoniaConvertUnits)
    
    my_tan_value <- st()[[y]]
    
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    else
      my_tan_value <- input$ammoniaSlider_convert
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$ammoniaConvertUnits), my_tan_value)
    
  })
  
  
  df_tan <- reactive({
    
    if(-1 == rv$duct_tape_2) {
      
      progress <- shiny::Progress$new()
      
      on.exit(progress$close())
      
      progress$set(message = 'Initializing Ammonia Conversion Data', value = 0)
      
    }
    
    
    my_icTemp <- icTemp()$ic
    my_icSal  <- icSal()$ic
    my_icPh   <- icPh()$ic
    
    if(!is.null(my_icSal) && my_icSal != '' &&
       !is.null(my_icTemp) && my_icTemp != '') {
      
      idx_tan <- which(input$ammoniaConvertUnits == tanUnitsList)
      
      ic_rho <- calcRho(my_icTemp, my_icSal)
      
      uia_posto_free <- percentNh3ForTemp(my_icTemp, my_icSal, my_icPh)  # on FREE scale
      
      uia_posto <- uia_posto_free - 
        log10(ahFreeToSwsFactor(my_icSal, my_icTemp, 0)) - 
        log10(ahSwsToNbsFactor(my_icSal, my_icTemp, 0))                  # on NBS scale
      
      icTan <- tanToIcUnits(input$ammoniaSlider_convert, input$ammoniaConvertUnits, ic_rho, uia_posto)
      
      
      df <- tanToAllUnits(icTan, ic_rho, uia_posto, 4, 4)
      
      
      df <- data.frame(TAN   = df$vals[1:8],
                       UIAN  = df$vals[9:16],
                       TA    = df$vals[17:24],
                       UIA   = df$vals[25:32],
                       Units = df$units[1:8],
                       
                       stringsAsFactors = F
                       )
      
      
      # hidden.col <- c(rep(0, nrow(df)))
      hidden.col <- c(rep(0, 32))  # NB: HARD-WIRE as "24" rows for number of current units
      hidden.col[idx_tan] <- 1
      
      df <- cbind(df, h = hidden.col)
      
      # as in "salinity_module.R," not just "as.data.frame(df)"
      tan_list <- list(df = df, 
                       ic = icTan)
      
      # 2/2 "duct tape" solution ... ----
      rv$duct_tape_2 <- 2
      
      tan_list
    }
    
  })
  
  
  # ---- DT clicks ----
  
  additional_conversion_info <- reactive({
    
    str_temp <- paste0(icTemp()$val, ' ', icTemp()$units)
    str_sal  <- paste0(icSal()$val, ' ', icSal()$units)
    str_pH   <- paste0(icPh()$val, ' ', icPh()$units)
    
    HTML(paste(tags$h5('(at pH ', str_pH, ', ', str_temp, ', & ', str_sal, ')',
                       style = "text-align: center;"
                       )
               )
         )
    
  })
  
  
  # see: tgp_module_WITH_MODULES.R for similar implementation of DT
  
  proxy_ammonia_convert_dt = dataTableProxy(session$ns('ammonia_convert_dt'))

  #   # see: http://www.datatables.net/reference/option/dom
  #   # dom = 'tp' option for table + pagination only
  #   options = list(dom = 'tp', 'bSort' = F, pageLength = 5)

  # *** NB: replaceData doesn't work in module namespace ***
  # see: https://github.com/rstudio/DT/issues/359 for workaround
  observe({

    # replaceData(proxy_dt_data, my_df()$df, rownames = F, resetPaging = FALSE)
    dataTableAjax(session, df_tan()$df, rownames = F, outputId = 'ammonia_convert_dt')
    reloadData(proxy_ammonia_convert_dt, resetPaging = FALSE)

  })


  output$ammonia_convert_dt <- DT::renderDataTable({

    rv$duct_tape_2
    
    datatable( isolate(df_tan()$df),
               
               colnames = c('TA-N', 'UIA-N', 'TA', 'UIA', 'units', 'h'),

               rownames = F,
               
               # see: https://yihui.shinyapps.io/DT-selection/
               selection = list(mode = 'single', target = 'cell'),

               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              pageLength = 8,

                              # columnDefs = list(list(targets = 2, visible = F)),
                              columnDefs = list(list(targets = 6 - 1, visible = F),
                                                list(className = 'dt-right', targets = 0:3)),

                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '#000'});",
                                "}")
               )
               
               # see: https://stackoverflow.com/questions/42099418/how-can-i-reduce-row-height-in-dt-datatables
               # %>% formatStyle('h', target = 'row', lineHeight = '70%')
    )

  }
  
    # %>% formatStyle('h', target = 'row',
                  # # backgroundColor = styleEqual(c(0, 1), c('#6699FF', '#FFFF66')),
                  #   fontWeight = styleEqual(c(0, 1), c('normal', 'bold')))
  )
  
  
  # return(df_tan)
  
}