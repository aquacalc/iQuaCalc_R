# gas conversion module functions for
# "iQuaCalc (Lite)" dashboard


gasConversionModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
                         
    # wellPanel(style = 'padding-bottom: 0px;',
              
              splitLayout(cellWidths = c('50%', '50%'),
                          
                          radioButtons(ns('rb_for_gas_convert'), label = 'Gasses',
                                       choices = c('O2', 'N2', 'Ar'),
                                       # choices = c('O2', 'N2', 'Ar', 'CO2'), 
                                       selected = 'O2', inline = T),
                          
                          tags$div(style='background-color: azure; border-style: solid;',
                                   htmlOutput(ns('my_sat_po_sto'))
                          )
              ),
              
              
              column( width = 5,
                      
                      # CO2 entry options... ----
                      # wellPanel(style = 'padding-bottom: 0px;',
                                
                                tabsetPanel(id = ns('gas_tabset_panel'),
                                            
                                            tabPanel(title = 'Gas by measurement',
                                                     style = 'padding-top: 8px',
                                                     
                                                     
                                                     splitLayout(cellWidths = c('25%', '50%', '25%'),
                                                                 
                                                                 numericInput(ns('gas_convert'), 'Gas', 
                                                                              min = 0, max = 45, value = 33, step = 0.1),
                                                                 
                                                                 selectInput(ns('gasConvertUnits'), 'Gas Units', 
                                                                             choices = gasUnitsListPref),
                                                                 
                                                                 tags$h6()
                                                     ),
                                                     
                                                     co2MeasuredNumericModuleInput(ns('co2_measured_for_gas_convert_co2'))
                                            ),
                                            
                                            tabPanel(title = 'CO2 by pH & [Alk]',
                                                     style = 'padding-top: 5px',
                                                     
                                                     phModuleInput(ns('ph_for_gas_convert_co2')),
                                                     
                                                     alkModuleInput(ns('alk_for_gas_convert_co2'), 0)
                                            )
                                ),  # END tabsetPanel "CO2..."
                      # ),
                      
                      tabsetPanel(id = 'water_or_atmosphere',
                                  
                                  tabPanel(title = 'water',
                                           
                                           # wellPanel(style = 'padding-bottom: 0px;',
                                                     
                                                     temperatureModuleInput(ns('temp_for_gas_convert'), 0),
                                                     
                                                     salinityModuleInput(ns('sal_for_gas_convert'), 0)
                                           # )
                                  ),
                                  
                                  tabPanel(title = 'atmosphere',
                                           
                                           # wellPanel(style = 'padding-bottom: 0px;',
                                                     
                                                     barometricNumericModuleInput(ns('barometric_for_gas_convert')),
                                                     
                                                     co2_gasModuleInput(ns('co2_atm_for_gas_convert_co2'))
                                           # )
                                  )
                      ),  # END 'water_or_atmosphere' tabsetPanel
                      
                      # wellPanel(style = 'padding-bottom: 0px;',
                                
                                depth_ModuleInput(ns('depth_for_gas_convert'))
                      # )
                      
              ),   # END column 1 of 2
              
              column( width = 5,
                      
                      # fluidRow(
                      #   
                      #   column(width = 12,
                      #          
                      #          conversionDisplayModuleInput(ns('gas_conversion_result'))
                      #   )
                      #   
                      # ),
                      
                      fluidRow(
                        
                        column(width = 12,
                               div(DT::dataTableOutput(ns('gas_convert_dt')), 
                                   style = 'font-size:100%')
                        )
                      )
                      
              )  # END column 2 of 2
              
    # )  # END wellPanel
  )
}



gasConversionModule <- function(input, output, session, st) {
  
  
  # ---- callModule for T, S, pH  ----
  icTemp       <- callModule(temperatureModule, 'temp_for_gas_convert', 
                             reactive(st()))
  icSal        <- callModule(salinityModule, 'sal_for_gas_convert', reactive(icTemp()), 
                             reactive(st()))
  icPh         <- callModule(phModule, 'ph_for_gas_convert', reactive(icTemp()), reactive(icSal()), 
                             reactive(st()))
  icBarometric <- callModule(barometricNumericModule, 'barometric_for_gas_convert', 
                             reactive(st()))
  
  icDepth      <- callModule(depth_Module, 'depth_for_gas_convert',
                             reactive(st()))
  
  
  # ---- for displaying a single conversion result from table cell click
  callModule(conversionDisplayModule, 'gas_conversion_result',
             reactive(input$gas_convert), 
             reactive(input$gasConvertUnits),
             reactive(df_gas()), 
             reactive(input$gas_convert_dt_cell_clicked), # table id + "_cell_clicked'
             reactive(input$gas_convert_dt_cells_selected),
             reactive(additional_conversion_info()),
             reactive(st()))
  
  
  
  # "*_init" flags when app is (re-)launched ----
  rv <- reactiveValues(select_init = -1,
                       gas_units_default = 'mg/L',
                       gas_sl_init = -1,
                       duct_tape_2 = -1,
                       gas_default = gasSet
                       )
  
  
  # # massage O2 & N2 to sub-scripts ----
  # if('O2' == input$rb_for_gas_convert)
  #   my_gas <- HTML(paste0('O', tags$sub('2')))
  # else if('N2' == input$rb_for_gas_convert)
  #   my_gas <- HTML(paste0('N', tags$sub('2')))
  # else
  #   my_gas <- 'Ar'
  
  # ---- TAN ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$gasConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('gasConvertUnits')
      
      rv$select_init <- 1
      
      gas_units_init <- st()[[x]]
      
      if(length(gas_units_init) == 0)
        gas_units_init <- rv$gas_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'gasConvertUnits', 
                        label = paste0(input$rb_for_gas_convert, ' Units'), 
                        choices = gasChoices,
                        gas_units_init)
    }
    
    updateStore(session, session$ns("gasConvertUnits"), input$gasConvertUnits)
    
    idx <- which(input$gasConvertUnits == gasUnitsListPref)
    
    # y <- paste0(session$ns('sl_'), input$gasConvertUnits)
    y <- paste0(session$ns('sl_'), input$gasConvertUnits, '_', input$rb_for_gas_convert)
    
    my_gas_value <- st()[[y]]
    
    if(length(my_gas_value) == 0)
      my_gas_value <- rv$gas_default[idx]
    
    updateNumericInput(session, "gas_convert", 
                       label = paste0(gasUnitsListPref_short[idx], ' ', input$rb_for_gas_convert),
                       value = my_gas_value,
                       min = gasMin[idx], max = gasMax[idx], step = gasStep[idx])
    
    # update slider value for current units ???
    # updateStore(session, paste0(session$ns('sl_'), input$gasConvertUnits), my_gas_value)
    updateStore(session, paste0(session$ns('sl_'), 
                                input$gasConvertUnits, 
                                '_', 
                                input$rb_for_gas_convert), my_gas_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$gas_convert, {
    
    if(rv$gas_sl_init < 0) {
      
      rv$gas_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$gasConvertUnits == gasUnitsListPref)
    
    # y <- paste0(session$ns('sl_'), input$gasConvertUnits)
    y <- paste0(session$ns('sl_'), input$gasConvertUnits, '_', input$rb_for_gas_convert)
    
    my_gas_value <- st()[[y]]
    
    
    if(length(my_gas_value) == 0)
      my_gas_value <- rv$gas_default[idx]
    else
      my_gas_value <- input$gas_convert
    
    
    # update slider value for current units
    # updateStore(session, paste0(session$ns('sl_'), input$gasConvertUnits), my_gas_value)
    updateStore(session, paste0(session$ns('sl_'), 
                                input$gasConvertUnits, 
                                '_', 
                                input$rb_for_gas_convert), my_gas_value)
    
  })
  
  
  # observeEvent -- radiobuttons ----
  observeEvent(input$rb_for_gas_convert, {
    
    idx <- which(input$gasConvertUnits == gasUnitsListPref)
    
    # ******************************************
    # y <- paste0(session$ns('sl_'), input$gasConvertUnits)
    y <- paste0(session$ns('sl_'), input$gasConvertUnits, '_', input$rb_for_gas_convert)
    
    my_gas_value <- st()[[y]]
    
    if(length(my_gas_value) == 0)
      my_gas_value <- rv$gas_default[idx]
    # ******************************************
    
    updateNumericInput(session, "gas_convert", 
                       label = paste0(gasUnitsListPref_short[idx], ' ', input$rb_for_gas_convert),
                       value = my_gas_value,
                       min = gasMin[idx], max = gasMax[idx], step = gasStep[idx])
    
    updateSelectInput(session, 'gasConvertUnits', 
                      label = paste0(input$rb_for_gas_convert, ' Units')) 
                      # choices = gasChoices,
                      # gas_units_init)
  })
  
  
  # * * PERCENT SATURATION ----
  calcPoStoSaturation <- function() {
    
    # in I.C. units, μmol/kg
    gas_sat_for_this_temp_sal_bp <- calcGasSat(gas_type, temp, sal, bp_atm)
    
    # cat('gas_sat_for_this_temp_sal_bp = ', gas_sat_for_this_temp_sal_bp, 'μmol/kg \n')
    # print(df_gas()$df[5])
    # cat('-----\n')
    # cat(100.0 * df_gas()$df[5] / gas_sat_for_this_temp_sal_bp, '% \n')
    # cat('=====\n\n\n')
    
    # divide entered concentration (as μmol/k) by gas_sat_for_this_temp_sal_bp, then * 100
    
  }
  
  
  df_gas <- reactive({
    
    # cat('in gas_conversion_module.R, input$rb_for_gas_convert = ', input$rb_for_gas_convert, '\n\n')
    
    if(-1 == rv$duct_tape_2) {
      
      progress <- shiny::Progress$new()
      
      on.exit(progress$close())
      
      progress$set(message = 'Initializing Gas Conversion Data', value = 0)
      
    }
    
    
    my_icTemp       <- icTemp()$ic
    my_icSal        <- icSal()$ic
    my_icPh         <- icPh()$ic
    my_icBarometric <- icBarometric()$ic
    
    # if(!is.null(my_icSal) && my_icSal != '' &&
    #    !is.null(my_icTemp) && my_icTemp != '') {
      
      idx_gas <- which(input$gasConvertUnits == gasUnitsListPref)
      
      ic_rho <- calcRho(my_icTemp, my_icSal)
      
      uia_posto_free <- percentNh3ForTemp(my_icTemp, my_icSal, my_icPh)  # on FREE scale
      
      uia_posto <- uia_posto_free - 
        log10(ahFreeToSwsFactor(my_icSal, my_icTemp, 0)) - 
        log10(ahSwsToNbsFactor(my_icSal, my_icTemp, 0))                  # on NBS scale
      
      
      icGas <- calcGasToIcUnits(input$rb_for_gas_convert, 
                                input$gas_convert, 
                                input$gasConvertUnits, 
                                my_icTemp, my_icSal, my_icBarometric)
      
      df <- calcGasAllUnits(input$rb_for_gas_convert, 
                            icGas, 
                            'μmol/kg', 
                            my_icTemp, my_icSal, my_icBarometric)
      
      # ----------*
      
      hydrostatic_rate     <- getPressureIncreaseWithDepth(my_icTemp, my_icSal)
      hydrostatic_pressure <- hydrostatic_rate * icDepth()
      
      barometric_plus_hydrostatic_atm <- my_icBarometric + hydrostatic_pressure / 759.999952
      
      # in I.C. units, μmol/kg
      gas_sat_for_this_temp_sal_bp <- calcGasSat(input$rb_for_gas_convert, 
                                                 my_icTemp, 
                                                 my_icSal, 
                                                 barometric_plus_hydrostatic_atm)
      
      
      gas_sat_po_sto <- round(100.0 * as.numeric(df$vals[5]) / gas_sat_for_this_temp_sal_bp, 1)
      
      # cat('gas_sat_for_this_temp_sal_bp = ', gas_sat_for_this_temp_sal_bp, 'μmol/kg \n')
      # cat('icGas = ', icGas, 'μmol/kg \n\n')
      # print(df)
      # cat('-----\n\n')
      # cat(gas_sat_po_sto, '% \n')
      # cat('=====\n\n\n')
      
      # ----------*
      
      
      hidden.col <- c(rep(0, nrow(df)))
      hidden.col[idx_gas] <- 1
      
      df <- cbind(df, h = hidden.col)
      
      # as in "salinity_module.R," not just "as.data.frame(df)"
      gas_list <- list(df         = df,
                       ic         = icGas,
                       val        = input$gas_convert,
                       units      = input$gasConvertUnits,
                       sat_po_sto = gas_sat_po_sto)
      
      # 2/2 "duct tape" solution ... ----
      # rv$duct_tape_2 <- 2
      
      gas_list
    # }
    
  })
  
  
  # ---- additional conversion output ----
  
  output$my_sat_po_sto <- renderUI({
    
    req(df_gas())
    
    if('O2' == input$rb_for_gas_convert)
      gas <- HTML(paste0('O', tags$sub('2')))
    else if('N2' == input$rb_for_gas_convert)
      gas <- HTML(paste0('N', tags$sub('2')))
    else
      gas <- 'Ar'

    HTML(paste(tags$h3(gas, ' sat: ', df_gas()$sat_po_sto, '%', style = "text-align: center;")))
    
  })
  
    
    
  additional_conversion_info <- reactive({
    
    str_temp <- paste0(icTemp()$val, ' ', icTemp()$units)
    str_sal  <- paste0(icSal()$val, ' ', icSal()$units)
    str_bar  <- paste0(icBarometric()$val, ' ', icBarometric()$units)
    
    # HTML(paste(tags$h5('(at pH ', str_pH, ', ', str_temp, ', & ', str_sal, ')',
    #                    style = "text-align: center;")
    #            )
    #      )
    
    HTML(paste(tags$h5('(at ', str_temp, ', ', str_sal, ', & ', str_bar, ')',
                       style = "text-align: center;")
               )
         )
  })
  
  
  
  # ---- DT clicks ----
  
  # see: tgp_module_WITH_MODULES.R for similar implementation of DT
  
  proxy_gas_convert_dt = dataTableProxy(session$ns('gas_convert_dt'))

  #   # see: http://www.datatables.net/reference/option/dom
  #   # dom = 'tp' option for table + pagination only
  #   options = list(dom = 'tp', 'bSort' = F, pageLength = 5)

  # *** NB: replaceData doesn't work in module namespace ***
  # see: https://github.com/rstudio/DT/issues/359 for workaround
  observe({
    
    # replaceData(proxy_dt_data, my_df()$df, rownames = F, resetPaging = FALSE)
    dataTableAjax(session, df_gas()$df, rownames = F, outputId = 'gas_convert_dt')
    reloadData(proxy_gas_convert_dt, resetPaging = FALSE)

  })


  output$gas_convert_dt <- DT::renderDataTable({
    
    datatable( isolate(df_gas()$df),
               
               colnames = c('', '', 'hidden.col'),

               rownames = F,
               
               # see: https://yihui.shinyapps.io/DT-selection/
               selection = list(mode = 'single', target = 'cell'),

               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              
                              # pageLength = 5,
                              pageLength = nrow(isolate(df_gas()$df)),

                              columnDefs = list(list(targets = 2, visible = F),
                                                list(className = 'dt-right', targets = 0)),
                              # columnDefs = list(list(targets = 6 - 1, visible = F),
                              #                   list(className = 'dt-right', targets = 0:3)),

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
  
  
  # return(df_gas)
  
}