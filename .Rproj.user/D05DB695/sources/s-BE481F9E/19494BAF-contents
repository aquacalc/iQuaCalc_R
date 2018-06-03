# gas module functions for
# "iQuaCalc (Lite) salinity module.R"


tgpModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 5,
             
             tabsetPanel(id = 'my_tabset_tgp_controls', type = 'pills', selected = NULL,
                         
                         tabPanel('Input Data', value = 'tgp_input_data',
                                  
                                  fluidRow(

                                    # T, S, pH, & Alk column
                                    column(width = 12,

                                           fluidRow(
                                             column(width = 12,

                                                    wellPanel(style = 'padding-bottom: 0px;',
                                                      
                                                      temperatureModuleInput(ns('temp_for_tgp_'), 0),
                                                      
                                                      salinityModuleInput(ns('sal_for_tgp_'), 0)
                                                    ),
                                                    
                                                    fluidRow(
                                                      column(width = 12,
                                                             
                                                             wellPanel(style = 'padding-bottom: 0px;',
                                                               
                                                               barometricNumericModuleInput(ns('barometric_for_tgp_')),
                                                               
                                                               co2_gas_atm_ModuleInput(ns('co2_atm_for_gas_sat'))
                                                             )
                                                      )
                                                    ),
                                                    
                                                    fluidRow(
                                                      column(width = 12,
                                                             wellPanel(style = 'padding-bottom: 0px;',
                                                               
                                                               depth_ModuleInput(ns('depth_for_gas_sat'))
                                                             )
                                                      )
                                                    )
                                             )
                                           )
                                    )
                                  )  # END TOP fluidRow
                         ),   # END tabPanel 'T & S'
                         
                         
                         tabPanel('TGP Info', value = 'tgp_info',
                                  
                                  fluidRow(
                                    
                                    column(width = 12,
                                           
                                           fluidRow(
                                             column(width = 12,
                                                    
                                                    wellPanel(
                                                      
                                                      h4(paste0('Not Yet Added...'), align = 'center')
                                                    )
                                             )
                                           )
                                    )
                                  )  # END TOP fluidRow
                                  
                         )    # END tabPanel Atmos
                         
             ) # END tabsetPanel
      ),
      
      column(width = 7,
               
             fluidRow(
               
               column(width = 11,
                      
                      div(DT::dataTableOutput(ns('gas_sat_dt')), 
                          style = 'font-size:125%')
               )
             ) # END fluidRow DT
             
      )   # END column #2/2
      
    )   # END fluidRow
    
  )  # END tagList
    
}  # tgpModuleInput



tgpModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       tgp_units_default = 'Δ mm Hg (torr)',
                       tgp_sl_init = -1,
                       tgp_default = tgpSet,
                       duct_tape_2 = -1,
                       
                       select_init_co2_gas = -1,
                       co2_gas_units_default = 'μatm',
                       co2_gas_sl_init = -1,
                       co2_gas_default = co2_gasSet,
                       
                       select_init_depth_z = -1,
                       depth_z_units_default = 'm',
                       depth_z_sl_init = -1,
                       depth_z_default = c(0, 0)
                       )
  
  
  # ---- load T, S, barometric modules ----
  
  icTemp <- callModule(temperatureModule, 'temp_for_tgp_', reactive(st()))

  icSal <- callModule(salinityModule, 'sal_for_tgp_',
                      reactive(icTemp()),
                      reactive(st()))
  
  icBarometric <- callModule(barometricNumericModule, 'barometric_for_tgp_',
                             reactive(st()))
  
  icCo2_atm    <- callModule(co2_gas_atm_Module, 'co2_atm_for_gas_sat',
                             reactive(st()))
  
  icDepth      <- callModule(depth_Module, 'depth_for_gas_sat',
                             reactive(st()))
  
  
  # # ---- CO2 (atm) update ----
  # 
  # # Observe SELECT_INPUT input, store when changed
  # observeEvent(input$co2_gasConvertUnits, priority = 50, {
  #   
  #   if(rv$select_init_co2_gas < 0)  {
  #     
  #     x <- session$ns('co2_gasConvertUnits')
  #     
  #     rv$select_init_co2_gas <- 1
  #     
  #     co2_gas_units_init <- st()[[x]]
  #     
  #     if(length(co2_gas_units_init) == 0)
  #       co2_gas_units_init <- rv$co2_gas_units_default
  #     
  #     # user (re-)opened app. Is store$select empty?
  #     updateSelectInput(session, 'co2_gasConvertUnits', 'Atmospheric CO2', 
  #                       choices = co2_gasChoices,
  #                       selected = co2_gas_units_init)
  #     
  #     freezeReactiveValue(input, "co2_gasConvertUnits")
  #   }
  #   
  #   updateStore(session, session$ns("co2_gasConvertUnits"), input$co2_gasConvertUnits)
  #   
  #   idx <- which(input$co2_gasConvertUnits == co2_gasChoices)
  #   
  #   y <- paste0(session$ns('sl_'), input$co2_gasConvertUnits)
  #   
  #   my_co2_gas_value <- st()[[y]]
  #   
  #   if(length(my_co2_gas_value) == 0)
  #     my_co2_gas_value <- rv$co2_gas_default[idx]
  #   
  #   updateSliderInput(session, "co2_gasSlider_convert", label = paste0(co2UnitsList[idx], ' CO2'),
  #                     value = my_co2_gas_value,
  #                     min = co2_gasMin[idx], max = co2_gasMax[idx], step = co2_gasStep[idx])
  #   
  #   freezeReactiveValue(input, "co2_gasSlider_convert")
  #   
  #   # update slider value for current units ???
  #   updateStore(session, y, my_co2_gas_value)
  #   
  # })
  # 
  # 
  # # Observe SLIDER_INPUT input, store when changed
  # # observeEvent(c(input$gasSlider_convert, icTemp(), icSal()), {
  # observeEvent(input$co2_gasSlider_convert, {
  #   
  #   if(rv$co2_gas_sl_init < 0) {
  #     
  #     rv$co2_gas_sl_init <- 1
  #     
  #     return()
  #   }
  #   
  #   idx <- which(input$co2_gasConvertUnits == co2_gasChoices)
  #   
  #   y <- paste0(session$ns('sl_'), input$co2_gasConvertUnits)
  #   
  #   my_co2_gas_value <- st()[[y]]
  #   
  #   
  #   if(length(my_co2_gas_value) == 0)
  #     my_co2_gas_value <- rv$co2_gas_default[idx]
  #   else
  #     my_co2_gas_value <- input$co2_gasSlider_convert
  #   
  #   # update slider value for current units
  #   # updateStore(session, paste0(session$ns('sl_'), input$co2_gasConvertUnits), my_co2_gas_value)
  #   updateStore(session, y, my_co2_gas_value)
  #   
  #   freezeReactiveValue(input, "co2_gasSlider_convert")
  #   
  # })
  # 
  

  # ---- ECHO INPUT ----
  
  # # ECHO gas inputs
  # output$gas_input_echo <- renderUI({
  #   
  #   req(icBarometric())
  # 
  #   str1 <- tags$strong(paste0('Temperature: ', icTemp()$val,' ', icTemp()$units))
  #   str2 <- tags$strong(paste0('   Salinity: ', icSal()$val,' ', icSal()$units))
  #   # str5 <- tags$strong(paste0(' Barometric: ', icBarometric()$val,' ', icBarometric()$units))
  #   
  #   # if units of 'altitude', then display estimated pressure in mm Hg
  #   if(icBarometric()$units %in% c('km', 'm', 'ft')) {
  #     
  #     # returns ic barometric in atm, convert to mm Hg
  #     barometric_in_mmHg <- calcBarometricToIcUnits(icBarometric()$val, icBarometric()$units) * 759.999952
  #     barometric_in_mmHg <- round(barometric_in_mmHg, 1)
  #     
  #     str5 <- tags$strong(paste0(icBarometric()$val,' ', icBarometric()$units, 
  #                                ' (~ ', barometric_in_mmHg, ' mm Hg)'))
  #   } else {
  #     
  #     str5 <- tags$strong(paste0(' Barometric: ', icBarometric()$val,' ', icBarometric()$units))
  #   }
  #   
  #   str6 <- tags$strong(paste0('  CO₂ (atm): ', input$co2_gasSlider_convert,' ', input$co2_gasConvertUnits))
  #   # str6 <- tags$strong(paste0('  CO₂ (atm): ', co2_data()$val,' ', co2_data()$units))
  #   # str6 <- paste0('CO', tags$sub(2), ' (atm): ', icCO2_tgp()$val,' ', icCO2_tgp()$units)
  # 
  #   HTML(paste(tags$h4(str1), tags$h4(str2), tags$hr(),
  #              tags$h4(str5), tags$h4(str6)))
  #              # sep = '<br/>'))
  #   # HTML(paste(tags$h4(str1)))
  # 
  # })
  
  
  
  # ---- DT: GAS SAT ----
  
  df_gas_sat <- reactive({
    
    if(-1 == rv$duct_tape_2) {
      
      progress <- shiny::Progress$new()
      
      on.exit(progress$close())
      
      progress$set(message = 'Initializing Saturation Data', value = 0)
    }
    
    req(
      # input$tgpSlider_convert, input$tgpConvertUnits,
      icTemp(), icSal(),
      # input$depth_z_units,
      # input$depth_z,
      # icO2(),
      # co2_data(),
      icBarometric(),
      cancelOutput = T
    )
    
    
    # for c('O2', 'N2', 'Ar') ...
    # 1. calcGasSat(gas_type, temp, sal, bp_atm)
    # in I.C. units, 'μmol/kg'
    
    # add hydrostatic pressure to barometric pressure to calc saturation at depth m
    # NB: 1. hydrostatic_rate <- getPressureIncreaseWithDepth(temp_ic, sal_ic) returns mm Hg/m
    #     2. hydrostatic_rate in mm Hg/m times depth in meters -> mm Hg
    #     3. convert mm Hg to atm
    #     4. add to icBarometric()$ic
    
    # *** Now, depth only in 'm' and 'ft'
    
    # ad hoc conversion of 'ft' to 'm', when required...
    # if('ft' == input$depth_z_units)
    #   my_depth_z <- 0.3048 * input$depth_z
    # else
    #   my_depth_z <- input$depth_z  # in meters
    
    hydrostatic_rate     <- getPressureIncreaseWithDepth(icTemp()$ic, icSal()$ic)
    hydrostatic_pressure <- hydrostatic_rate * icDepth()
    
    barometric_plus_hydrostatic_atm <- icBarometric()$ic + hydrostatic_pressure / 759.999952
    
    # cat('in gas_sat_module_WITH_MODULES.R ... \n')
    # cat('                           hydrostatic_rate = ', hydrostatic_rate, ' mm Hg / meter \n')
    # cat('hydrostatic_pressure  at ', my_depth_z, ' m = ', 
    #     hydrostatic_pressure, ' mm Hg (', hydrostatic_pressure / 759.999952, ' atm) \n')
    # cat('barometric pressure = ', icBarometric()$ic, ' atm \n')
    # cat('total pressure at ', my_depth_z, ' m = ', icBarometric()$ic, ' atm + ', 
    #     hydrostatic_pressure / 759.999952, ' atm = ', barometric_plus_hydrostatic_atm, ' atm \n')
    # 
    # cat('O2 sat at ', my_depth_z, ' m = ', 
    #     calcGasSat('O2', icTemp()$ic, icSal()$ic, barometric_plus_hydrostatic_atm), '\n')
    
    
    o2_sat      <- calcGasSat('O2', icTemp()$ic, icSal()$ic, barometric_plus_hydrostatic_atm)
    o2_sat_pure <- calcPureO2Sat(   icTemp()$ic, icSal()$ic, barometric_plus_hydrostatic_atm)
    
    n2_sat <- calcGasSat('N2', icTemp()$ic, icSal()$ic, barometric_plus_hydrostatic_atm)
    ar_sat <- calcGasSat('Ar', icTemp()$ic, icSal()$ic, barometric_plus_hydrostatic_atm)
    
    # o2_sat <- calcGasSat('O2', icTemp()$ic, icSal()$ic, icBarometric()$ic)
    # n2_sat <- calcGasSat('N2', icTemp()$ic, icSal()$ic, icBarometric()$ic)
    # ar_sat <- calcGasSat('Ar', icTemp()$ic, icSal()$ic, icBarometric()$ic)
    
    # 2. calcGasAllUnits(gasType, gasVal, 'μmol/kg', temp, sal)
    # NB: calcGasSat returns μmol/kg, but calcPureO2Sat returns mg/L ----
    o2_sat_df      <- calcGasAllUnits('O2', o2_sat,      'μmol/kg', icTemp()$ic, icSal()$ic, icBarometric()$ic)
    o2_sat_pure_df <- calcGasAllUnits('O2', o2_sat_pure, 'mg/L',    icTemp()$ic, icSal()$ic, icBarometric()$ic)
    
    n2_sat_df <- calcGasAllUnits('N2', n2_sat, 'μmol/kg', icTemp()$ic, icSal()$ic, icBarometric()$ic)
    ar_sat_df <- calcGasAllUnits('Ar', ar_sat, 'μmol/kg', icTemp()$ic, icSal()$ic, icBarometric()$ic)
    
    
    # 3. collect in df
    # 4. work on CO2
    
    # from CarbCalc.R ...
    # return: μmol/kg-soln  (**WITH** subtracting VP)
    # NB: NO NEED for vp_ic in original function, as calcVP(t, s) implementd in function body
    
    # @co2_data()$ic is icCO2Gas <- calcCo2_atmmos_ToIcUnits(
    #                                  input$co2_gasSlider_convert, input$co2_gasConvertUnits)
    #                  my_co2_mole_fraction <- icCO2Gas
    
    # NB: icCo2_atm() only returns atmospheric mole fraction of CO2 in μatm
    my_co2_mole_fraction <- icCo2_atm()
    
    
    co2_sat <- calc_CO2_gasSat_microMol_kg(icTemp()$ic, icSal()$ic, 
                                           barometric_plus_hydrostatic_atm, 
                                           my_co2_mole_fraction)
                                           # icBarometric()$ic, my_co2_mole_fraction)
                                           # icBarometric()$ic, co2_data()$ic)
    
    
    co2_sat_df <- calcGasAllUnits('CO2', co2_sat, 'μmol/kg', icTemp()$ic, icSal()$ic, icBarometric()$ic)
  
    
    df_gasses <- data.frame(         O2 = o2_sat_df$vals[-c(13:14)], 
                            'O2 (pure)' = o2_sat_pure_df$vals[-c(13:14)], 
                                     N2 = n2_sat_df$vals[-c(13:14)], 
                                     Ar = ar_sat_df$vals[-c(13:14)], 
                                    CO2 = co2_sat_df$vals[-c(13:14)],
                                  Units = ar_sat_df$units[-c(13:14)]
                            )
    
    
    hidden.col <- rep(0, 12)
    # hidden.col <- rep(0, 14)
    # hidden.col[idx_g] <- 1
    
    df_gasses$h <- hidden.col
    
    df_gas_sat_list <- list(df = df_gasses)
    
    # 2/2 "duct tape" solution ... ----
    rv$duct_tape_2 <- 2
    
    df_gas_sat_list
    
  })
  
  
  # proxy = dataTableProxy('foo')
  # observe({
  #   replaceData(proxy, loopData(), resetPaging = FALSE)
  # })
  # output$foo = DT::renderDataTable(isolate(loopData()))
  
  
  proxy_gas_sat_dt = dataTableProxy(session$ns('gas_sat_dt'))
  
  #   # see: http://www.datatables.net/reference/option/dom
  #   # dom = 'tp' option for table + pagination only
  #   options = list(dom = 'tp', 'bSort' = F, pageLength = 5)
  
  # *** NB: replaceData doesn't work in module namespace ***
  # see: https://github.com/rstudio/DT/issues/359 for workaround 
  observe({
    
    # req(df_gas_sat(), cancelOutput = T)
    
    replaceData(proxy_gas_sat_dt, df_gas_sat()$df, rownames = F, resetPaging = FALSE)
    
    # dataTableAjax(session, df_gas_sat()$df, rownames = F, outputId = 'gas_sat_dt')
    # reloadData(proxy_gas_sat_dt, resetPaging = FALSE)
    
  })
  
  
  output$gas_sat_dt <- DT::renderDataTable({
    
    rv$duct_tape_2
    
    datatable( 
      
      # isolate(df_gas_sat()$df),
      df_gas_sat()$df,
               
               colnames = c('O₂ (air)', 'O₂ (pure)', 'N₂', 'Ar', 'CO₂', 'units', 'h'),
               
               rownames = F,
               
               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              pageLength = 12,
                              
                              # columnDefs = list(list(targets = 2, visible = F)),
                              columnDefs = list(list(targets = 7 - 1, visible = F),
                                                list(className = 'dt-right', targets = 0:4)),
                              
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '#000'});",
                                "}")
               ) 
    )
    # %>%
    #   # formatStyle(isolate(df_temp()$h), 'h',
    #   formatStyle('h', target = 'row',
    #               # backgroundColor = styleEqual(c(0, 1), c('#6699FF', '#FFFF66')),
    #               fontWeight = styleEqual(c(0, 1), c('normal', 'bold')))
  })
  
}