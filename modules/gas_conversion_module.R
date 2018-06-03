# gas conversion module functions for
# "iQuaCalc (Lite)" dashboard


gasConversionModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    tabsetPanel(
      
      tabPanel(title = 'Oxygen, Nitrogen, & Argon',
               
               fluidRow(
                 
                 column( width = 1
                         
                 ),
                 
                 column( width = 5,
                         
                         br(),
                         
                         wellPanel(style = 'padding-bottom: 0px;',
                           
                           # splitLayout(cellWidths = c('50%', '50%'),
                                       
                                       radioButtons(ns('rb_for_gas_convert'), label = 'Gasses',
                                                    choices = c('O2', 'N2', 'Ar'),
                                                    # choices = c('O2', 'N2', 'Ar', 'CO2'), 
                                                    selected = 'O2', inline = T),
                                       
                                       # tags$div(style='background-color: azure; border-style: solid;',
                                       #   htmlOutput(ns('my_sat_po_sto'))
                                       # )
                           # ),
                             
                             splitLayout(cellWidths = c('25%', '50%', '25%'),
                                         
                                         numericInput(ns('gas_convert'), 'Gas', 
                                                      min = 0, max = 45, value = 33, step = 0.1),
                                         
                                         selectInput(ns('gasConvertUnits'), 'Gas Units', 
                                                     choices = gasUnitsListPref),
                                         
                                         tags$h6()
                             )
                           
                         ),
                         
                         tabsetPanel(id = 'water_or_atmosphere_o2_n2_ar',
                                     
                                     tabPanel(title = 'water',
                                              
                                              wellPanel(style = 'padding-bottom: 0px;',
                                                        
                                                        temperatureModuleInput(ns('temp_for_gas_convert'), 0),
                                                        
                                                        salinityModuleInput(ns('sal_for_gas_convert'), 0),
                                                        
                                                        depth_ModuleInput(ns('depth_for_gas_convert'))
                                              )
                                     ),
                                     
                                     tabPanel(title = 'atmosphere',
                                              
                                              wellPanel(style = 'padding-bottom: 20px;',
                                                        
                                                        barometricNumericModuleInput(ns('barometric_for_gas_convert'))
                                                        
                                                        # co2_gasModuleInput(ns('co2_atm_for_gas_convert_co2'))
                                              )
                                     )
                         )  # END CO2 'water_or_atmosphere' tabsetPanel
                         
                 ),   # END column 1 of 2
                 
                 column( width = 5,
                         
                         br(), br(), br(),
                         
                         fluidRow(
                           
                           column(width = 2
                                  
                           ),
                           
                           column(width = 8,
                                  
                                  tags$div(style='background-color: azure; border-style: solid;',
                                           htmlOutput(ns('my_sat_po_sto')))
                                  # conversionDisplayModuleInput(ns('gas_conversion_result'))
                           ),
                           
                           column(width = 2
                                  
                           )
                           
                         ),
                         
                         br(), br(),
                         
                         fluidRow(
                           
                           column(width = 6,
                                  
                                  h4('CONCENTRATION', align = 'center'),
                                  datatableModuleInput(ns('gas_conversion_1_dt'),
                                                       col_width = 12,
                                                       font_size = '115')
                           ),
                           
                           column(width = 6,
                                  
                                  h4('PRESSURE', align = 'center'),
                                  datatableModuleInput(ns('gas_conversion_2_dt'),
                                                       col_width = 12,
                                                       font_size = '115')
                           )
                           
                           # column(width = 12,
                           #        div(DT::dataTableOutput(ns('gas_convert_dt')), 
                           #            style = 'font-size:100%')
                           # )
                         )
                         
                 )  # END column 2 of 2
                 
               )  # END fluidRow
               
      ),  # END tabPanel, O2, N2, & Ar
      
      
      tabPanel(title = 'Carbon dioxide',
               
               fluidRow(
                 
                 column( width = 1
                         
                 ),
                 
                 column( width = 5,
                         
                         # CO2 entry options... ----

                         tabsetPanel(id = ns('co2_tabset_panel'),
                                     
                                     tabPanel(title = 'CO2 by measurement', style = 'padding-top: 8px',
                                              
                                              wellPanel(style = 'padding-bottom: 0px;',
                                                        
                                                        co2MeasuredNumericModuleInput(ns('co2_measured_for_gas_convert_co2'))
                                              )
                                     ),
                                     
                                     tabPanel(title = 'CO2 by pH & [Alk]', style = 'padding-top: 5px',
                                              
                                              wellPanel(style = 'padding-bottom: 0px;',
                                                        
                                                        phModuleInput(ns('ph_for_gas_convert_co2')),
                                                        
                                                        alkModuleInput(ns('alk_for_gas_convert_co2'), 0)
                                              )
                                     )
                         ),
                         
                         tabsetPanel(id = 'water_or_atmosphere',
                                     
                                     tabPanel(title = 'water',
                                              
                                              wellPanel(style = 'padding-bottom: 0px;',
                                                        
                                                        temperatureModuleInput(ns('temp_for_gas_convert_co2'), 0),
                                                        
                                                        salinityModuleInput(ns('sal_for_gas_convert_co2'), 0),
                                                        
                                                        depth_ModuleInput(ns('depth_for_gas_convert_co2'))
                                              )
                                     ),
                                     
                                     tabPanel(title = 'atmosphere',
                                              
                                              wellPanel(style = 'padding-bottom: 20px;',
                                                        
                                                        barometricNumericModuleInput(ns('barometric_for_gas_convert_co2')),
                                                        
                                                        co2_gasModuleInput(ns('co2_atm_for_gas_convert_co2'))
                                              )
                                     )
                         )  # END CO2 'water_or_atmosphere' tabsetPanel
                         
                 ),   # END column 1 of 2
                 
                 column( width = 5,
                         
                         br(), br(), br(),
                         
                         fluidRow(
                           
                           column(width = 2
                                  
                           ),
                           
                           column(width = 8,
                                  
                                  tags$div(style='background-color: azure; border-style: solid;',
                                           htmlOutput(ns('my_sat_po_sto_co2')))
                                  # conversionDisplayModuleInput(ns('gas_conversion_result_co2'))
                           ),
                           
                           column(width = 2
                             
                           )
                           
                         ),
                         
                         br(), br(),
                         
                         fluidRow(
                           
                           column(width = 6,
                                  
                                  h4('CONCENTRATION', align = 'center'),
                                  datatableModuleInput(ns('co2_conversion_1_dt'),
                                                       col_width = 12,
                                                       font_size = '115')
                           ),
                           
                           column(width = 6,
                                  
                                  h4('PRESSURE', align = 'center'),
                                  datatableModuleInput(ns('co2_conversion_2_dt'),
                                                       col_width = 12,
                                                       font_size = '115')
                           )
                           
                           # column(width = 12,
                           #        div(DT::dataTableOutput(ns('gas_convert_dt_co2')), 
                           #            style = 'font-size:110%')
                           # )
                         )
                         
                 )  # END column 2 of 2
                 
               )  # END fluidRow
        
      )  # END tabPanel, CO2
      
    )  # END tabSetPanel
    
  )
}



gasConversionModule <- function(input, output, session, st) {
  
  
  # ---- callModules for O2, N2, Ar ----
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
  
  
  # ---- O2, N2, Ar: display single conversion from cell click
  callModule(conversionDisplayModule, 'gas_conversion_result',
             reactive(input$gas_convert), 
             reactive(input$gasConvertUnits),
             reactive(df_gas()), 
             reactive(input$gas_convert_dt_cell_clicked), # table id + "_cell_clicked'
             reactive(input$gas_convert_dt_cells_selected),
             reactive(additional_conversion_info()),
             reactive(st()))
  
  
  
  # send rows 19:24 to datatableModule
  gas_dt_1 <- callModule(datatable_split_Module, 'dummy_co2_1', 
                         reactive(df_gas()),
                         1, 6 # row numbers to send
  )
  
  # send rows 13:18 to datatableModule
  gas_dt_2 <- callModule(datatable_split_Module, 'dummy_co2_2', 
                         reactive(df_gas()),
                         7, 12 # row numbers to send
  )
  
  
  callModule(datatableModule,
             'gas_conversion_1_dt',
             reactive(gas_dt_1()),
             6, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'gas_conversion_2_dt',
             reactive(gas_dt_2()),
             6, 3 # nrow, ncol
  )
  
  
  
  
  
  # ---- callModules for CO2 ----
  icTemp_co2       <- callModule(temperatureModule, 'temp_for_gas_convert_co2', 
                                 reactive(st()))
  icSal_co2        <- callModule(salinityModule, 'sal_for_gas_convert_co2', reactive(icTemp_co2()), 
                                 reactive(st()))
  icPh_co2         <- callModule(phModule, 'ph_for_gas_convert_co2', 
                                 reactive(icTemp_co2()), reactive(icSal_co2()), 
                                 reactive(st()))
  icAlk_co2        <- callModule(alkModule, 'alk_for_gas_convert_co2', 
                                 reactive(icTemp_co2()), 
                                 reactive(icSal_co2()),
                                 reactive(st()))
  
  icBarometric_co2 <- callModule(barometricNumericModule, 'barometric_for_gas_convert_co2', 
                                 reactive(st()))
  
  icDepth_co2      <- callModule(depth_Module, 'depth_for_gas_convert_co2',
                                 reactive(st()))
  
  icCo2_atm        <- callModule(co2_gasModule, 'co2_atm_for_gas_convert_co2',
                                 reactive(st()))
  
  icCo2            <- callModule(co2MeasuredNumericModule, 'co2_measured_for_gas_convert_co2', 
                                 reactive(icTemp_co2()), 
                                 reactive(icSal_co2()),
                                 reactive(icPh_co2()),
                                 reactive(icBarometric_co2()),
                                 reactive(st()))
  
  
  # ---- CO2: display single conversion from cell click
  callModule(conversionDisplayModule, 'gas_conversion_result_co2',
             reactive(co2_val_units_from_ph_alk()$val), 
             reactive(co2_val_units_from_ph_alk()$units),
             reactive(df_co2()), 
             reactive(input$gas_convert_dt_co2_cell_clicked), # table id + "_cell_clicked'
             reactive(input$gas_convert_dt_co2_cells_selected),
             reactive(additional_conversion_info_co2()),
             reactive(st()))
  
  
  # *********************************************************
  
  # send rows 19:24 to datatableModule
  co2_dt_1 <- callModule(datatable_split_Module, 'dummy_co2_1', 
                         reactive(df_co2()),
                         1, 6 # row numbers to send
  )
  
  # send rows 13:18 to datatableModule
  co2_dt_2 <- callModule(datatable_split_Module, 'dummy_co2_2', 
                         reactive(df_co2()),
                         7, 12 # row numbers to send
  )
  
  
  callModule(datatableModule,
             'co2_conversion_1_dt',
             reactive(co2_dt_1()),
             6, 3 # nrow, ncol
  )
  
  callModule(datatableModule,
             'co2_conversion_2_dt',
             reactive(co2_dt_2()),
             6, 3 # nrow, ncol
  )
  
  # *********************************************************
  
  
  # CO2 val-units for directly entered or pH-Alk calculated ----
  co2_val_units_from_ph_alk <- reactive({
    
    if('CO2 by measurement' == input$co2_tabset_panel) {
      
      co2_val   <- icCo2()$val
      co2_units <- icCo2()$units
      
    } else {
      
      my_icTemp       <- icTemp_co2()$ic
      my_icSal        <- icSal_co2()$ic
      my_icPh         <- icPh_co2()$ic
      my_icAlk        <- icAlk_co2()$ic / 1000.0
      
      co2_actual_mol_kg <- alphaZero(my_icTemp, my_icSal, my_icPh) * calcDicOfAlk(my_icAlk, my_icPh, my_icTemp, my_icSal)
      
      co2_in_mg_per_L <- 1000.0 * co2_actual_mol_kg * MW_CO2 * (calcRho(my_icTemp, my_icSal) / 1000.0)
      
      
      co2_val   <- formatC(co2_in_mg_per_L, format = 'f', digits = 4)
      co2_units <- 'mg/L'
    }
    
    my_co2_data <- tibble(  val = co2_val,
                          units = co2_units)
  })
  
  
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
  
  
  # -- DF_CO2() ----
  
  df_co2 <- reactive({
    
    my_icTemp       <- icTemp_co2()$ic
    my_icSal        <- icSal_co2()$ic
    my_icPh         <- icPh_co2()$ic
    my_icAlk        <- icAlk_co2()$ic / 1000.0
    my_icBarometric <- icBarometric_co2()$ic
    
    req(icCo2()$val,
        icCo2()$units)
    
    
    if('CO2 by measurement' == input$co2_tabset_panel) {
      
      # co2_in_mg_per_L <- icCo2()$co2_mg_per_L
      
      co2_measured_df <- calcGasAllUnits('CO2', icCo2()$val, icCo2()$units, my_icTemp, my_icSal, my_icBarometric)

    } else {

      co2_actual_mol_kg <- alphaZero(my_icTemp, my_icSal, my_icPh) * calcDicOfAlk(my_icAlk, my_icPh, my_icTemp, my_icSal)

      co2_in_mg_per_L <- 1000.0 * co2_actual_mol_kg * MW_CO2 * (calcRho(my_icTemp, my_icSal) / 1000.0)
      
      co2_measured_df <- calcGasAllUnits('CO2', co2_in_mg_per_L, 'mg/L', my_icTemp, my_icSal, my_icBarometric)
    }

    
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    hydrostatic_rate     <- getPressureIncreaseWithDepth(my_icTemp, my_icSal)
    hydrostatic_pressure <- hydrostatic_rate * icDepth_co2()
    
    barometric_plus_hydrostatic_atm <- my_icBarometric + hydrostatic_pressure / 759.999952
    
    
    # NB: icCo2_atm() only returns atmospheric mole fraction of CO2 in μatm
    my_co2_mole_fraction <- icCo2_atm()$val
    
    co2_sat <- calc_CO2_gasSat_microMol_kg(my_icTemp, my_icSal, 
                                           barometric_plus_hydrostatic_atm, 
                                           my_co2_mole_fraction)
    
    co2_sat_df      <- calcGasAllUnits('CO2', co2_sat, 'μmol/kg', my_icTemp, my_icSal, my_icBarometric)
    
    # print(co2_sat_df)
    # co2_measured_df <- calcGasAllUnits('CO2', icCo2()$val, icCo2()$units, my_icTemp, my_icSal)
    
    
    # RE-ARRANGE rows, 1:7 for concentration, 8:14 for pressure
    # Why? Instead of one table, splitting into two, side-by-side:
    # one for conc, one for pressure
    
    # re-order rows to separate concentrations from pressures
    # co2_measured_df <- co2_measured_df[c(3, 6, 4, 7, 12, 5, 14, 13, 2:1, 8:11), ]
    co2_measured_df <- co2_measured_df[c(3, 6, 4, 7, 12, 5, 2:1, 8:11), ]
    
    co2_measured_df$units[7] <- 'mm Hg'
    
    hidden.col <- c(rep(0, nrow(co2_measured_df)))
    # hidden.col[idx_gas] <- 0
    
    # cat('----------------\n')
    # print(co2_measured_df)
    
    co2_po_sto <- round(100.0 * as.numeric(co2_measured_df$val[1]) / as.numeric(co2_sat_df$val[3]), 1)
    
    # cat('CO2 saturation: ', as.numeric(co2_measured_df$val[1]), ' / ', as.numeric(co2_sat_df$val[3]), 
    #     ' = ', co2_po_sto, '% \n')
    # cat('----------------\n')
    
    co2_measured_df <- cbind(co2_measured_df, h = hidden.col)
    
    co2_list <- list(df         = co2_measured_df,
                     ic         = 'nema',
                     val        = icCo2()$val,
                     units      = icCo2()$units,
                     sat_po_sto = co2_po_sto)
    
    co2_list
    
  })
  
  
  # -- DF_GAS() ----
  
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
    my_icBarometric <- icBarometric_co2()$ic
    
    # if(!is.null(my_icSal) && my_icSal != '' &&
    #    !is.null(my_icTemp) && my_icTemp != '') {
      
      idx_gas <- which(input$gasConvertUnits == gasUnitsListPref)
      
      ic_rho <- calcRho(my_icTemp, my_icSal)
      
      # uia_posto_free <- percentNh3ForTemp(my_icTemp, my_icSal, my_icPh)  # on FREE scale
      # 
      # uia_posto <- uia_posto_free - 
      #   log10(ahFreeToSwsFactor(my_icSal, my_icTemp, 0)) - 
      #   log10(ahSwsToNbsFactor(my_icSal, my_icTemp, 0))                  # on NBS scale
      
      
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
      
      barometric_plus_hydrostatic_atm <- icBarometric()$ic + hydrostatic_pressure / 759.999952
      
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
      
      df <- df[c(3, 6, 4, 7, 12, 5, 2:1, 8:11), ]
      
      df$units[7] <- 'mm Hg'
      
      
      hidden.col <- c(rep(0, nrow(df)))
      # hidden.col[idx_gas] <- 1
      
      df <- cbind(df, h = hidden.col)
      
      # as in "salinity_module.R," not just "as.data.frame(df)"
      gas_list <- list(df         = df,
                       ic         = icGas,
                       val        = input$gas_convert,
                       units      = input$gasConvertUnits,
                       sat_po_sto = gas_sat_po_sto)
      
      # 2/2 "duct tape" solution ... ----
      rv$duct_tape_2 <- 2
      
      gas_list
    
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
    
    HTML(paste(tags$h3(gas, ': ', df_gas()$sat_po_sto, '% saturation', style = "text-align: center;")))
    
  })
  
  
  output$my_sat_po_sto_co2 <- renderUI({
    
    req(df_co2())
    
    gas <- HTML(paste0('CO', tags$sub('2')))
    
    HTML(paste(tags$h3(gas, ': ', df_co2()$sat_po_sto, '% saturation', style = "text-align: center;")))
    
  })
  
  
  # -- additional conversion info ----
  
  additional_conversion_info <- reactive({
    
    str_temp <- paste0(icTemp()$val, ' ', icTemp()$units)
    str_sal  <- paste0(icSal()$val, ' ', icSal()$units)
    str_bar  <- paste0(icBarometric()$val, ' ', icBarometric()$units)
    
    HTML(paste(tags$h5('(at ', str_temp, ', ', str_sal, ', & ', str_bar, ')',
                       style = "text-align: center;")
    )
    )
  })
  
    
  additional_conversion_info_co2 <- reactive({
    
    str_temp <- paste0(icTemp_co2()$val, ' ', icTemp_co2()$units)
    str_sal  <- paste0(icSal_co2()$val, ' ', icSal_co2()$units)
    str_bar  <- paste0(icBarometric_co2()$val, ' ', icBarometric_co2()$units)
    
    HTML(paste(tags$h5('(at ', str_temp, ', ', str_sal, ', & ', str_bar, ')',
                       style = "text-align: center;")
               )
         )
  })
  
  
  
  # ---- O2, N2, Ar DT clicks ----
  
  # see: tgp_module_WITH_MODULES.R for similar implementation of DT
  
  proxy_gas_convert_dt <- dataTableProxy(session$ns('gas_convert_dt'))
  # proxy_gas_convert_dt <- dataTableProxy('gas_convert_dt')

  #   # see: http://www.datatables.net/reference/option/dom
  #   # dom = 'tp' option for table + pagination only
  #   options = list(dom = 'tp', 'bSort' = F, pageLength = 5)

  # *** NB: replaceData doesn't work in module namespace ***
  # see: https://github.com/rstudio/DT/issues/359 for workaround
  observe({
    
    print(df_gas()$df)
    replaceData(proxy_gas_convert_dt, df_gas()$df, rownames = F, resetPaging = FALSE)
    
    # dataTableAjax(session, df_gas()$df, rownames = F, outputId = 'gas_convert_dt')
    # reloadData(proxy_gas_convert_dt, resetPaging = FALSE)

  })


  output$gas_convert_dt <- DT::renderDataTable({
    
    rv$duct_tape_2
    
    datatable( 
               # isolate(df_gas()$df),
               df_gas()$df,
               
               colnames = c('', '', 'hidden.col'),

               rownames = F,
               
               # see: https://yihui.shinyapps.io/DT-selection/
               selection = list(mode = 'single', target = 'cell'),

               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              
                              # pageLength = 5,
                              # pageLength = nrow(isolate(df_gas()$df)),
                              pageLength = nrow(df_gas()$df),

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
  
  
  
  # ---- CO2 DT clicks ----
  
  # see: tgp_module_WITH_MODULES.R for similar implementation of DT
  
  proxy_gas_convert_dt_co2 <- dataTableProxy(session$ns('gas_convert_dt_co2'))
  
  #   # see: http://www.datatables.net/reference/option/dom
  #   # dom = 'tp' option for table + pagination only
  #   options = list(dom = 'tp', 'bSort' = F, pageLength = 5)
  
  # *** NB: replaceData doesn't work in module namespace ***
  # see: https://github.com/rstudio/DT/issues/359 for workaround
  observe({
    
    # replaceData(proxy_dt_data, my_df()$df, rownames = F, resetPaging = FALSE)
    dataTableAjax(session, df_co2()$df, rownames = F, outputId = 'gas_convert_dt_co2')
    reloadData(proxy_gas_convert_dt_co2, resetPaging = FALSE)
    
  })
  
  
  output$gas_convert_dt_co2 <- DT::renderDataTable({
    
    rv$duct_tape_2
    
    datatable( df_co2()$df,
               
               colnames = c('', '', 'hidden.col'),
               
               rownames = F,
               
               # see: https://yihui.shinyapps.io/DT-selection/
               selection = list(mode = 'single', target = 'cell'),
               
               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              
                              # pageLength = 5,
                              pageLength = nrow(df_gas()$df),
                              
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
  
}