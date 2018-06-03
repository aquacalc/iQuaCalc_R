# Total Gas Pressure module functions for
# "iQuaCalc (Lite).R"


gasTgpModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 5,
             
             tabsetPanel(id = ns('my_tabset_tgp_controls'), type = 'pills', selected = NULL,
                         
                         tabPanel('Input Summary', value = 'tgp_input_summary',
                                  
                                  fluidRow(
                                    
                                    column(width = 12,
                                           wellPanel(style = 'padding-bottom: 0px; margin-top: 35px; 
                                                     margin-left: 10px;',
                                             
                                             htmlOutput(ns('gas_input_echo'))
                                           )
                                    )
                                  )
                         ),
                         
                         tabPanel('Water', value = 'tgp_enter_t_and_s',
                                  
                                  fluidRow(
                                    
                                    # T, S, pH, & Alk column
                                    column(width = 12,
                                           
                                           fluidRow(
                                             column(width = 12,
                                                    
                                                    wellPanel(style = 'padding-bottom: 0px; margin-top: 15px;
                                                     margin-left: 10px;',
                                                              
                                                              temperatureModuleInput(ns('temp_for_tgp'), 0),
                                                              
                                                              salinityModuleInput(ns('sal_for_tgp'), 0)
                                                    ),
                                                    
                                                    wellPanel(style = 'padding-bottom: 0px;
                                                     margin-left: 10px;',
                                                              
                                                              gasModuleInput(ns('o2_for_tgp'), 'Oxygen', 1, 'Pure O2')
                                                    ),
                                                    
                                                    wellPanel(style = 'padding-bottom: 0px;
                                                     margin-left: 10px;',
                                                              
                                                              tabsetPanel(id = ns('co2_tabset_panel'),
                                                                
                                                                          tabPanel(title = 'CO2 by measurement',
                                                                                   style = 'padding-top: 8px',
                                                                         
                                                                                   co2MeasuredNumericModuleInput(ns('co2_measured_for_tgp'))
                                                                          ),
                                                                
                                                                          tabPanel(title = 'CO2 by pH & [Alk]',
                                                                                   style = 'padding-top: 5px',
                                                                                   
                                                                                   phModuleInput(ns('ph_for_tgp')),
                                                                                   
                                                                                   alkModuleInput(ns('alk_for_tgp'), 0)
                                                                          )
                                                              )
                                                    )
                                             )
                                           ) # END fluidRow with wellPanel()s
                                    )  # END column
                                  )  # END TOP fluidRow
                         ),   # END tabPanel 'T & S'
                         
                         tabPanel('Atmosphere', value = 'tgp_enter_bar_and_co2_atm',
                                  
                                  fluidRow(
                                    
                                    # barometric & CO2 (atm) column
                                    column(width = 12,
                                           
                                           fluidRow(
                                             column(width = 12,
                                                    
                                                    wellPanel(style = 'padding-bottom: 0px; margin-top: 20px;
                                                     margin-left: 10px;',
                                                      
                                                              barometricNumericModuleInput(ns('barometric_for_tgp'))
                                                    ),
                                                    
                                                    wellPanel(style = 'padding-bottom: 0px;
                                                     margin-left: 10px;',
                                                              
                                                              co2_gasModuleInput(ns('co2_atm_tgp'))
                                                    )
                                             )
                                           )
                                    )
                                  )  # END TOP fluidRow
                                  
                         )    # END tabPanel Atmos
                         
             ) # END tabsetPanel
             
      ), # END column for left-side input data
      
      column(width = 7,
             
             # fluidRow(
             #   h4('Enter Total Gas Pressure & DO measurements', align = 'center')
             # ),
             
             # Enter TGP & DO
             fluidRow(
               column(width = 12,
                      
                      wellPanel(style = 'padding-bottom: 0px;
                                                     margin-rightt: 20px;',
                                
                        splitLayout(cellWidths = c('25%', '45%', '30%'),
                                    
                                    numericInput(ns('tgpSlider_convert'), 'TGP',
                                                 min = 0, max = 45, value = 33, step = 0.01),
                                    
                                    selectInput(ns('tgpConvertUnits'), 'TGP Units',
                                                choices = tgpChoices),
                                    
                                    tags$h6()
                        )
                      )
               )
               
             ),   # END fluidRow 'TGP & DO'
             
             # DT (& other output)
             fluidRow(
               
               column(width = 11,
                      
                      div(DT::dataTableOutput(ns('tgp_dt')), 
                          style = 'font-size: 115%')
               )
             ),  # END fluidRow DT
             
             br(), br(),
             
             fluidRow(
                      
               splitLayout(cellWidths = c('40%', '60%'),
                           
                           box(style = "text-align: center;",
                               width = NULL,
                               # title = 'Pure O2 Tank Duration',
                               # solidHeader = T,
                               status = 'primary',
                               background = 'light-blue',
                               
                               tags$h3('Compensation Depth', align = 'center'),
                               htmlOutput(ns('comp_depth'))
                           ),
                           
                           plotOutput(ns('tgp_plot'), height = '210px', width = '315px')
               )
               
             ) # END fluidRow 'compensation depth'
        
      ) # END column for right-side display
      
    ) # END top-most fluidRow
  )
}



gasTgpModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       tgp_units_default = 'Δ mm Hg (torr)',
                       tgp_sl_init = -1,
                       tgp_default = tgpSet,
                       duct_tape_2 = -1)
  
  
  
  icTemp <- callModule(temperatureModule, 'temp_for_tgp', reactive(st()))

  icSal <- callModule(salinityModule, 'sal_for_tgp',
                      reactive(icTemp()),
                      reactive(st()))

  icPh <- callModule(phModule, 'ph_for_tgp',
                     reactive(icTemp()), 
                     reactive(icSal()),
                     reactive(st()))

  icAlk <- callModule(alkModule, 'alk_for_tgp',
                      reactive(icTemp()), 
                      reactive(icSal()),
                      reactive(st()))
  
  icBarometric <- callModule(barometricNumericModule, 'barometric_for_tgp',
                             reactive(st()))

  # co2_data <- callModule(co2_gas_atm_Module, 'co2_atm_tgp',
  #                         reactive(st()))

  # module for ATMOSPHERIC CO2 ----
  co2_data <- callModule(co2_gasModule, 'co2_atm_tgp',
                         # reactive(icTemp()), reactive(icSal()),
                         # reactive(icPh()), reactive(icAlk()),
                         # reactive(icBarometric()),
                         reactive(st()))
  
  icO2 <- callModule(gasModule, 'o2_for_tgp',
                     reactive(icTemp()), reactive(icSal()),
                     # 'Oxygen', 1, 'Pure O2',  # NB: 'O2', **NOT** 'Oxygen'
                     'O2', 1, 'Pure O2',  # NB: 'O2', **NOT** 'Oxygen'
                     reactive(icBarometric()),
                     reactive(st()))
  
  # module, DISSOLVED & MEASURED CO2 ----
  # accepts entered concentration and returns...only mg/L, for now??
  co2_dissolved_measured <- callModule(co2MeasuredNumericModule, 'co2_measured_for_tgp',
                                       reactive(icTemp()), reactive(icSal()),
                                       reactive(icPh()), 
                                       reactive(icBarometric()),
                                       reactive(st()))
                           
  # icGasSatCalc <- callModule(gasSatModule, 'dummy_gas_sat',
  #                            reactive(icTemp()), reactive(icSal()),
  #                            reactive(icPh()), reactive(icAlk()),
  #                            reactive(icBarometric()), reactive(co2_data()),
  #                            reactive(st()))
  
  
  
  # ---- TGP ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$tgpConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('tgpConvertUnits')
      
      rv$select_init <- 1
      
      tgp_units_init <- st()[[x]]
      
      if(length(tgp_units_init) == 0)
        tgp_units_init <- rv$tgp_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'tgpConvertUnits', 'TGP Units', 
                        choices  = tgpChoices,
                        selected = tgp_units_init)
      
      freezeReactiveValue(input, "tgpConvertUnits")
    }
    
    updateStore(session, session$ns("tgpConvertUnits"), input$tgpConvertUnits)
    
    idx <- which(input$tgpConvertUnits == tgpUnitsList)
    
    y <- paste0(session$ns('sl_'), input$tgpConvertUnits)
    
    my_tgp_value <- st()[[y]]
    
    if(length(my_tgp_value) == 0)
      my_tgp_value <- rv$tgp_default[idx]
    
    
    updateNumericInput(session, "tgpSlider_convert", label = paste0(tgpUnitsList_short[idx], ' TGP'),
                       value = my_tgp_value,
                       min = tgpMin[idx], max = tgpMax[idx], step = tgpStep[idx])
    
    # updateSliderInput(session, "tgpSlider_convert", label = paste0(tgpUnitsList_short[idx], ' TGP'),
    #                   value = my_tgp_value,
    #                   min = tgpMin[idx], max = tgpMax[idx], step = tgpStep[idx])
    
    freezeReactiveValue(input, "tgpSlider_convert")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$tgpConvertUnits), my_tgp_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  # observeEvent(c(input$gasSlider_convert, icTemp(), icSal()), {
  observeEvent(input$tgpSlider_convert, {
    
    if(rv$tgp_sl_init < 0) {
      
      rv$tgp_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$tgpConvertUnits == tgpUnitsList)
    
    y <- paste0(session$ns('sl_'), input$tgpConvertUnits)
    
    my_tgp_value <- st()[[y]]
    
    
    if(length(my_tgp_value) == 0)
      my_tgp_value <- rv$tgp_default[idx]
    else
      my_tgp_value <- input$tgpSlider_convert
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$tgpConvertUnits), my_tgp_value)
    
  })
  
  
  
  # ---- ECHO INPUT ----
  
  # ECHO gas inputs
  output$gas_input_echo <- renderUI({
    
    req(icBarometric())
    
    str1 <- tags$strong(paste0('Temperature: ', icTemp()$val,' ', icTemp()$units))
    str2 <- tags$strong(paste0('   Salinity: ', icSal()$val,' ', icSal()$units))
    str3 <- tags$strong(paste0('         pH: ', icPh()$val,' ', icPh()$units))
    str4 <- tags$strong(paste0(' Alkalinity: ', icAlk()$val,' ', icAlk()$units))
    
    # if units of 'altitude', then display estimated pressure in mm Hg
    if(icBarometric()$units %in% c('km', 'm', 'ft')) {
      
      # returns ic barometric in atm, convert to mm Hg
      barometric_in_mmHg <- calcBarometricToIcUnits(icBarometric()$val, icBarometric()$units) * 759.999952
      barometric_in_mmHg <- round(barometric_in_mmHg, 1)
      
      str5 <- tags$strong(paste0(icBarometric()$val,' ', icBarometric()$units, 
                                 ' (~ ', barometric_in_mmHg, ' mm Hg)'))
    } else {
      
      str5 <- tags$strong(paste0(' Barometric: ', icBarometric()$val,' ', icBarometric()$units))
    }
    
    str6 <- tags$strong(paste0('  CO₂ (atm): ', co2_data()$val,' ', co2_data()$units))
    # str6 <- paste0('CO', tags$sub(2), ' (atm): ', icCO2_tgp()$val,' ', icCO2_tgp()$units)
    
    HTML(paste(tags$h4(str1), tags$h4(str2), tags$hr(),
               tags$h4(str3), tags$h4(str4), tags$hr(),
               tags$h4(str5), tags$h4(str6)))
    # sep = '<br/>'))
    # HTML(paste(tags$h4(str1)))
    
  })
  
  
  # Compensation Depth ----
  
  output$comp_depth <- renderUI({
    
    if(df_tgp()$comp_depth < 0) {
      
      str1 <- tags$h4('The water column is')
      str2 <- tags$h4('under-saturated')
      str3 <- ''
      
    } else {
      
      my_comp_depth <- as.numeric(formatC(round(df_tgp()$comp_depth, 5), format='f', digits=4))
      
      # ad hoc change m to m & cm...
      meters <- floor(my_comp_depth)
      cm     <- round((my_comp_depth - meters) * 100, 2)
      
      meters_centimeters <- paste0(meters, ' m ', cm, ' cm')
      
      str1 <- tags$h3(paste0(round(my_comp_depth, 2), ' m'))
      str2 <- tags$h4(paste0('(', meters_centimeters, ')'))
      str3 <- tags$h4(paste0('(', convertMetersToFtAndInches(my_comp_depth)[1]), ')')
    }
    
    # see: https://stackoverflow.com/questions/26368192/how-to-insert-new-line-in-r-shiny-string
    # HTML(paste("hello", "world", sep="<br/>"))
    
    HTML(paste0(str1, str2, str3, sep = "<br/>"))
    
  })
  
  
  # PLOT TGP vs. depth ----
  
  tgp_depth_df <- reactive({
    
    z_comp <- df_tgp()$comp_depth
    
    req(z_comp >= 0)
    
    z_seq  <- seq(z_comp * 1.5, 0, -0.1)
    
    
    # # convert BP from IC atm to mm Hg
    # bp_mmHg <- bp_ic * 759.999952
    mmHg_per_meter   <- getPressureIncreaseWithDepth(icTemp()$ic, icSal()$ic)
    
    # calc pressure with depth like thie...? ----
    # tgp_mmHg <- z_comp_in_meters * mmHg_per_meter + bp_mmHg
    # ΔP <- z_seq * mmHg_per_meter
    
    tgp_seq <- seq(100, 0, -0.1)
    
    # cat('in gas_tgp_module.R/tgp_depth_df...\n')
    delta_P_in_mmHg <- as.numeric(df_tgp()$delta_P)
    # cat('delta_P_in_mmHg = ', delta_P_in_mmHg, ' mm Hg \n')
    x <- delta_P_in_mmHg - (mmHg_per_meter * z_seq)
    # print(x)
    # print(z_seq)
    # cat('======================== \n\n')
    
    df <- data.frame(z = z_seq, delta_P = x,
                     stringsAsFactors = F)
    
    df
    
  })
  
  
  output$tgp_plot <- renderPlot({
    
    # geom_blank() + 
    
    z_comp <- df_tgp()$comp_depth
    
    req(z_comp >= 0)
    
    z_comp_zone <- tibble(x = c(-Inf,   Inf,    Inf, -Inf), 
                          y = c(z_comp, z_comp, Inf,  Inf))
                              # stringsAsFactors = F)
    
    z_comp_no <- tibble(x = c(-Inf,   Inf,    Inf, -Inf), 
                        y = c(z_comp, z_comp, 0,    0))
                            # stringsAsFactors = F)
    
    
    # NB: remove "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?"
    # see: https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
    # , group = 1
    p <- tgp_depth_df() %>% ggplot(aes(delta_P, z, group = 1)) + 
      scale_y_reverse() +
      geom_line(color = 'blue') +
      geom_hline(yintercept = z_comp, linetype = "dashed") +
      xlab('ΔP (mm Hg)') +
      ylab('depth (m)') +
      
      geom_polygon(data = z_comp_zone,
                   aes(x = x,
                       y = y
                   ),
                   alpha = 0.4,
                   fill = "darkgreen"
      ) +
      
      geom_polygon(data = z_comp_no,
                   aes(x = x,
                       y = y
                   ),
                   alpha = 0.2,
                   fill = "red"
      )
      
      # coord_cartesian(xlim = c(-6, as.numeric(df_tgp()$delta_P)),
      #                 ylim = c(0, 5),
      #                 expand = F)
    
    p <- p + scale_x_continuous(position = 'top')
    
    p
    
  })
  
  
  # ---- DF_CO2() ----
  
  df_co2 <- reactive({
    
    my_icTemp       <- icTemp()$ic
    my_icSal        <- icSal()$ic
    my_icPh         <- icPh()$ic
    
    my_icAlk        <- icAlk()$ic / 1000.0
    
    my_icBarometric <- icBarometric()$ic
    
    
    if('CO2 by measurement' == input$co2_tabset_panel) {
      
      co2_in_mg_per_L <- co2_dissolved_measured()$co2_mg_per_L
      
    } else {
      
      co2_actual_mol_kg <- alphaZero(my_icTemp, my_icSal, my_icPh) * calcDicOfAlk(my_icAlk, my_icPh, my_icTemp, my_icSal)
      
      co2_in_mg_per_L <- 1000.0 * co2_actual_mol_kg * MW_CO2 * (calcRho(my_icTemp, my_icSal) / 1000.0)
      
    }
    
    co2_for_tgp <- tgp_calc_co2(my_icTemp, my_icSal,
                                icPh()$ic, icAlk()$ic,    # NB: [Alk] in meq/kg (e.g., "2.4")
                                co2_data()$co2_mole_frac,
                                my_icBarometric,
                                co2_in_mg_per_L)
    
    
    co2_for_tgp$'%'       <- formatC(co2_for_tgp$'%',       format='f', digits=2)
    co2_for_tgp$'Δ mm Hg' <- formatC(co2_for_tgp$'Δ mm Hg', format='f', digits=2)
    co2_for_tgp$'mm Hg'   <- formatC(co2_for_tgp$'mm Hg',   format='f', digits=2)
    
    co2_for_tgp
    
  })
  
  
  # ---- DF_TGP() ----
  
  df_tgp <- reactive({
    
    # ----------------------------------------*
    # NB: MUST 'flatten' named gasChoices to gasUnitsListPref ----
    idx_g <- which(input$tgpConvertUnits == tgpUnitsList)
    
    tgp.LL    <- tgpMin[idx_g]
    tgp.UU    <- tgpMax[idx_g]
    tgp.units <- tgpUnitsList[idx_g]
    
    str_message <- paste0('Please enter a TGP value between ', 
                          tgp.LL, ' & ', tgp.UU, ' ', tgp.units)
    
    validate(
      
      need(
        
        try(
          
          input$tgpSlider_convert >= tgp.LL && 
            input$tgpSlider_convert <= tgp.UU
        ),
        
        str_message
      )
    )
    # ----------------------------------------*
    
    req(
      input$tgpSlider_convert, input$tgpConvertUnits,
      icTemp(), icSal(),
      icO2(),
      co2_data(),
      df_co2(),
      icBarometric(),
      cancelOutput = T
    )
    
    my_icTemp       <- icTemp()$ic
    my_icSal        <- icSal()$ic
    my_icO2         <- icO2()$ic
    my_icBarometric <- icBarometric()$ic
    my_icPh         <- icPh()$ic
    my_icAlk        <- icAlk()$ic
    
    
    # TGP I.C. units -- NOT atm, but (inconveniently) "%"
    icTgp <- calcTgpToIcUnits(input$tgpSlider_convert, input$tgpConvertUnits, my_icBarometric)
    
    df    <- calcTgpToAllUnits(input$tgpSlider_convert, input$tgpConvertUnits, my_icBarometric)
    
    # --------------------------*
    ic_delta_p <- calc_delta_p_to_ic_units(input$tgpSlider_convert, 
                                           input$tgpConvertUnits, 
                                           my_icBarometric)
    
    # my_df <- calc_delta_p_to_all_units(input$tgpSlider_convert, 
    #                                    input$tgpConvertUnits, 
    #                                    my_icBarometric)
    
    # cat('\n in gas_tgp_module.R: ...\n')
    # cat('ic_delta_p => ', ic_delta_p, '\n\n')
    # # print(df %>% spread(units, vals))
    # cat('----------------\n')
    # print(my_df %>% spread(units, vals))
    # print(co2_dissolved_measured()$co2_mg_per_L)
    # cat('----------------\n')
    
    
    is_co2_by_measurement <- ifelse(('CO2 by measurement' == input$co2_tabset_panel),
                                    TRUE,
                                    FALSE)
    
    tgp_df <- calc_tgp_n2ar_o2_co2(my_icTemp, my_icSal,
                                   my_icPh,   my_icAlk,    # NB: [Alk] in meq/kg (e.g., "2.4")
                                   my_icBarometric,
                                   ic_delta_p,
                                   my_icO2,
                                   is_co2_by_measurement,
                                   co2_data()$co2_mole_frac,
                                   co2_dissolved_measured()$co2_mg_per_L)
    
    # cat('\n********************\n')
    # print(tgp_df)
    # # print(tgp_df %>% mutate(ratio = as.numeric('N2 + Ar') / as.numeric(O2)))
    # cat('********************\n\n')
    # --------------------------*
    
    # tgp_mmHg   <- df$vals[2]
    # tgp_mmHg_2 <- my_df$vals[2]
    
    tgp_mmHg_3 <- as.numeric(tgp_df$'Total'[1])
    
    # cat(tgp_df$'Total Gas'[1], '\n')
    # cat(class(tgp_df$'Total Gas'[1]), '\n')
    
    # cat(df$'Δ mm Hg'[1], ' vs. ', ic_delta_p, ' vs ', tgp_mmHg_3, '\n')
    
    comp_depth <- tgpCalcCompDepth(tgp_mmHg_3, 
                                   my_icBarometric, 
                                   my_icTemp, 
                                   my_icSal,
                                   'm')
    
    # df <- df %>% spread(units, vals)
    
    # get O2 and N2 data for TGP -- alraedy spread
    # df_o2_n2 <- tgpCalcN2ArFromTgp_po_sto(icTgp,
    #                                       my_icO2,
    #                                       my_icTemp, my_icSal,
    #                                       my_icBarometric)
    
    # Have to format here -- but why?!
    
    # df$'%'[1]       <- formatC(round(df$'%'[1], 3),       format='f', digits=2)
    # df$'Δ mm Hg'[1] <- formatC(round(df$'Δ mm Hg'[1], 3), format='f', digits=2)
    # df$'mm Hg'[1]   <- formatC(round(df$'mm Hg'[1], 3),   format='f', digits=2)
    # 
    # df_o2_n2$'Δ mm Hg'[1] <- formatC(round(df_o2_n2$'Δ mm Hg'[1], 3), format='f', digits=2)
    # # [????] WHY must explicitly cast 'numeric' to as.numeric() ??
    # df_o2_n2$'Δ mm Hg'[2] <- formatC(round(as.numeric(df_o2_n2$'Δ mm Hg'[2]), 3), format='f', digits=2)
    # 
    # df_o2_n2$'mm Hg'[1] <- formatC(round(df_o2_n2$'mm Hg'[1], 3),             format='f', digits=2)
    # df_o2_n2$'mm Hg'[2] <- formatC(round(as.numeric(df_o2_n2$'mm Hg'[2]), 3), format='f', digits=2)
    # 
    # df_o2_n2$'%'[1] <- formatC(round(df_o2_n2$'%'[1], 3),             format='f', digits=2)
    # df_o2_n2$'%'[2] <- formatC(round(as.numeric(df_o2_n2$'%'[2]), 3), format='f', digits=2)
    
    
    # Vapor Pressure ----
    # vp_in_atm  <- calcVP(my_icTemp, my_icSal) # in atm
    # vp_in_mmHg <- vp_in_atm * 759.99999
    
    # vapor pressure df
    # df_vp <- data.frame(gas = c(rep('VP', 3)), 
    #                     vals = c('--', formatC(vp_in_mmHg, format='f', digits=2), '--'),
    #                     units = c('%', 'mm Hg', 'Δ mm Hg'),
    #                     
    #                     stringsAsFactors = F
    #                     )
    
    # df_vp <- df_vp %>% spread(units, vals)
    
    
    # x <- bind_rows(df, df_o2_n2, df_co2(), df_vp)
    
    # tgp_list <- list(df = df, ic = icTgp)
    tgp_list <- list(df         = tgp_df, 
                     ic         = icTgp,
                     delta_P    = ic_delta_p,   # in mm Hg
                     # delta_P    = df$'Δ mm Hg'[1],   # in mm Hg
                     comp_depth = comp_depth)
    
    # 1/2 "duct tape" solution ... ----
    rv$tgp_sl_init <- 5
    
    tgp_list
    
  })
  
  
  proxy_dt_data = dataTableProxy(session$ns('tgp_dt'))
  # proxy_dt_data = dataTableProxy('tgp_dt')
  
  # NB: replaceData doesn't work in module namespace
  # see: https://github.com/rstudio/DT/issues/359 for workaround 
  # observeEvent(df_tgp(), {
  observe({
    
    # replaceData(proxy_dt_data, dummy(), rownames = F, resetPaging = FALSE)
    
    dataTableAjax(session, df_tgp()$df, rownames = F, outputId = 'tgp_dt')
    reloadData(proxy_dt_data, resetPaging = F)
    
    cat('\n********************\n')
    print(df_tgp()$df)
    cat('\n--------------------\n')
    # print(tgp_df %>% mutate(ratio = as.numeric('N2 + Ar') / as.numeric(O2)))
    cat('********************\n\n')
    
  })
  
  
  output$tgp_dt <- DT::renderDataTable({
    
    rv$tgp_sl_init
    # icTemp()$ic
    # req(df_tgp(), cancelOutput = T)
    
    # cat('\n********************\n')
    # print(df_tgp()$df)
    # cat('\n--------------------\n')
    # # print(tgp_df %>% mutate(ratio = as.numeric('N2 + Ar') / as.numeric(O2)))
    # cat('********************\n\n')
    
    # datatable( isolate(df_tgp()$df),
               datatable( df_tgp()$df,
               
               rownames = F,
               
               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              pageLength = 3,
                              
                              # columnDefs = list(list(targets = 2, visible = F)),
                              columnDefs = list(list(className = 'dt-right', targets = 0:5)),
                              
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '#000'});",
                                "}")
               ) 
    )
    
  })
  
  
}