# UIA module for iQuaCalc dashboard
# v. 2, with left-side data entry and highcharter


uiaModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(

      column( width = 5,

        infoBoxOutput(
          ns('unIonAmmValBox'),
          width = NULL
        ),
        
        hr(),
        hr(),
        
        fluidRow(
          
          column(width = 12,
                 
                 wellPanel(style = "padding: 5px 2px 1px 5px;",
                           
                           fluidRow(
                             
                             column(width = 12,
                                    
                                    phModuleInput(ns('ph_inner_for_uia'))
                             )
                           )
                 )
          )
          
        ),
        
        # hr(),
        
        fluidRow(
          
          column(width = 12,
                 
                 wellPanel(style = "padding: 5px 2px 1px 5px;",
                           
                           fluidRow(
                             column(width = 12,
                                    temperatureNumericModuleInput(ns('temp_inner_for_uia'))
                             )
                           )
                 )
          )
        ),  # END fluidRow temperature
        
        # hr(),
        
        fluidRow(
          
          column(width = 12,
                 
                 wellPanel(style = "padding: 5px 2px 1px 5px;",
                           
                           fluidRow(
                             column(width = 12,
                                    salinityNumericModuleInput(ns('sal_inner_for_uia'))
                             )
                           )
                 )
          )
        )  # END fluidRow salinity
        
        # phModuleInput(ns('ph_inner_for_uia')),
        # temperatureModuleInput(ns('temp_inner_for_uia')),
        # salinityModuleInput(ns('sal_inner_for_uia'))

      ),   # END column 1 of 2

      column( width = 7,
              
        fluidRow(
          
            column(width = 1),
            column(width = 5, 
                   style="background-color: azure; border-style: solid; padding: 1px; margin: 0px 1px 10px 0px;",
                   # style="background: lightblue; padding: 1px; margin: 0px 5px 10px 0px;",
                   
                   # div(DT::dataTableOutput(ns('crit_data_df')), style = 'font-size:130%')
                   htmlOutput(ns('crit_data_df_left'))
            ),
            
            column(width = 5,
                   style="background-color: azure; border-style: solid; padding: 1px; margin: 0px 1px 10px 0px;",
                   
                   htmlOutput(ns('crit_data_df_right'))
            )
        ),

        tabBox(

          # height = "600px",
          # title = 'Data Entry & Info',
          id = 'tb_ammonia',
          width = NULL,

          tabPanel(
            
            title = 'TAN',
            
            fluidRow(

              column(width = 8,

                     wellPanel(style = "padding: 5px 2px 1px 5px;",

                               fluidRow(
                                 column(width = 12,
                                        tanNumericModuleInput(ns('tan_for_uia'))
                                 )
                               ),

                               fluidRow(
                                 column(width = 12,
                                        uiaNumericModuleInput(ns('uia_for_uia'))
                                 )
                               )
                     )

              )
            ),
            
            # +++++++++++++++++++++++++

            fluidRow(
              column(width = 8,

                     plotOutput(ns('uiaVsPhPlot'), height = 240, width = '100%')
              ),

              column(width = 4,

                     htmlOutput(ns('uia_plot_summary'))
              )
            )
            
          ),  # END tabPanel

          tabPanel(
            title = 'Crit UIA',

            box(
              width = '400px',
              solidHeader = T,
              status = 'primary',
              background = 'light-blue',

              hr(),
              
              # see: Jelena K., http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2109.2012.03170.x/abstract
              # Atlantic salmon parr were exposed for 105 days (at 12°C, pH 6.8) 
              # to four sublethal ammonia concentrations ranging from 0.1 to 
              # 35 μg L−1 NH3-N (0.1–25 mg L−1 TAN)

              wellPanel(
                style = "background-color: #000;",
                # h3('>> LOWER UIA-N is always better <<', align = 'center'),
                # h4(tags$li('UIA varies for species, size, DO, calcium, stress')), hr(),
                # h4(tags$li('0.0125 mg UIA/L -- EIFAC recommended maximum')),
                h4(tags$li('UIA varies with species, size, DO, calcium, stress...')), hr(),
                h4(tags$li('0.0125 mg UIA-N/L -- EIFAC recommended maximum')),
                h4(tags$li('0.0250 mg UIA-N/L -- potentially toxic for some species')),
                h4(tags$li('0.0500 mg UIA-N/L -- may be harmful with long exposure')),
                h4(tags$li('0.0880 mg UIA-N/L -- no effect over 96 h (Tomasso, 1994)')),
                h4(tags$li('0.1310 mg UIA-N/L -- no effect over 24 h (Tomasso, 1994)')),
                h4(tags$li('2.0000 mg UIA-N/L -- fish begin to die'))
                # h4(tags$li('(NB: This is UIA-N, not the TA-N that you measure)'))
              ) 

            )    # END box
            
          ),   # END tabPanel 'Crit UIA'

          tabPanel( title = 'UIA Info',

            box(
              width = '400px',
              solidHeader = T,
              status = 'primary',
              background = 'light-blue',
              
              hr(),
              
              wellPanel(
                style = "background-color: #000;",
                
              h4(tags$li('ammonia is produced by breakdown of feed protein')),
              # h4(tags$li('it peaks ~30 - 90 minutes after feeding')), hr(),
              
              h4(tags$li('there are two forms: ionized & un-ionized')),
              
              h4(tags$li('Un-Ionized Ammonia (UIA) is more toxic')),
              
              h4(tags$li('Total Ammonia Nitrogen (TAN) measures N in both forms')),
              
              hr(),
              
              # h4(tags$li('TAN combines ionized + un-ionized nitrogen')), hr(),
              
              h4(tags$li(tags$em('Avoid high Un-ionized Ammonia-N, [UIA-N]'))),
              
              h4(tags$li('[UIA-N] = [TAN] x (% UIA)')),
              
              h4(tags$li('[TAN] is measured with your ammonia test kit')),
              
              h4(tags$li('% UIA is calculated from pH, temperature, & salinity')),
              
              h4(tags$li('pH -- by far -- is the most important')),
              
              h4(tags$li('HIGH [TAN] and/or HIGH pH ➔ HIGH [UIA-N]'))
              )

            )      # END box
          )      # END tabPanel 'UIA Info'
          
        )     # END tabBox
        
      )    # END column width = 7
      
    )  # END fluidRow for ammoniaStuff
    
  )  # END ftaglist
}



# uiaModule <- function(input, output, session, icTemp, icSal, icPh, st) {
uiaModule <- function(input, output, session, st) {
  
  
  # ---- callModule for T, S, pH  ----
  
  # icTemp <- callModule(temperatureModule, 'temp_inner_for_uia', reactive(st()))
  # icSal <- callModule(salinityModule, 'sal_inner_for_uia', reactive(icTemp()), reactive(st()))
  
  icTemp <- callModule(temperatureNumericModule, 'temp_inner_for_uia', reactive(st()))
  icSal  <- callModule(salinityNumericModule, 'sal_inner_for_uia', reactive(icTemp()), reactive(st()))
  icPh   <- callModule(phModule, 'ph_inner_for_uia', reactive(icTemp()), reactive(icSal()), reactive(st()))
  
  
  # NB: Used here ONLY to return mg / L TA-N (converted to that ic value, if needed)
  #     that value is used to calc critPh_Nbs, which then is used to define the UIA 'danger zone"
  # so, use icTan()$ic
  icTan       <- callModule(tanNumericModule, 'tan_for_uia',
                            reactive(icTemp()),
                            reactive(icSal()),
                            reactive(icPh()), # NB: need pH for this implementation -- critPh line??
                            reactive(st()))

  # NB: Used here ONLY to return mg/L UIA-N (converted to that ic value, if needed)
  #     that value is used to calc critPh_Nbs, which then is used to define the UIA 'danger zone"
  # so, use icUia()$df[9]
  icUia       <- callModule(uiaNumericModule, 'uia_for_uia',
                            reactive(icTemp()),
                            reactive(icSal()),
                            reactive(icPh()), # NB: need pH for this implementation -- critPh line??
                            0,                # show_tc_flag == 0 => hide "Show" checkbox
                            reactive(st()))

  
  # "*_init" flags when app is (re-)launched ----
  rv <- reactiveValues(
                       select_init = -1,
                       tan_units_default = 'mg/L TA-N',
                       tan_sl_init = -1,
                       
                       select_init_uia = -1,
                       uia_crit_units_default = 'μg/L UIA-N',
                       uia_crit_sl_init = -1,
                       
                       tan_default = tanUnitsSet,
                       uia_default = uiaUnitsSet
                       # duct_tape_2 = -1,
                       # uia_default = c(2, 2, 20, 20, 0.2, 0.2,
                       #                 1, 1, 10, 10, 0.1, 0.1,
                       #                 
                       #                 2, 2, 20, 20, 0.2, 0.2,
                       #                 1, 1, 10, 10, 0.1, 0.1
                       # )
  )
  
  
  
  # ---- TAN ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(icTan()$units, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('tanUnits')
      
      rv$select_init <- 1
      
      tan_units_init <- st()[[x]]
      
      if(length(tan_units_init) == 0)
        tan_units_init <- rv$tan_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'tanUnits', 'Ammonia Units', 
                        # choices = list(
                        #   'Total Ammonia-N' = tanUnitsList[1:8],
                        #   'Total Ammonia' = tanUnitsList[17:24]
                        # ),
                        choices = list(
                          'Most Common'     = tanUnitsList[c(1, 5)],
                          'Total Ammonia-N' = tanUnitsList[c(2:4, 6:8)],
                          'Total Ammonia'   = tanUnitsList[17:24]
                        ),
                        selected = tan_units_init)
    }
    
    updateStore(session, session$ns("tanUnits"), icTan()$units)
    
    idx <- which(icTan()$units == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), icTan()$units)
    
    my_tan_value <- st()[[y]]
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    
    updateSliderInput(session, "tanVal", label = tanUnitsList[idx],
                      value = my_tan_value,
                      min = tanUnitsMin[idx], max = tanUnitsMax[idx], 
                      step = tanUnitsStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), icTan()$units), my_tan_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(icTan()$val, {
    
    if(rv$tan_sl_init < 0) {
      
      rv$tan_sl_init <- 1
      
      return()
    }
    
    idx <- which(icTan()$units == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), icTan()$units)
    
    my_tan_value <- st()[[y]]
    
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    else
      my_tan_value <- icTan()$val
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), icTan()$units), my_tan_value)
    
  })
  
  
  
  # ---- CRITICAL UIA ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(icUia()$units, priority = 50, {
    
    if(rv$select_init_uia < 0)  {
      
      x <- session$ns('critTanUnits')
      
      rv$select_init_uia <- 1
      
      uia_units_init <- st()[[x]]
      
      if(length(uia_units_init) == 0)
        uia_units_init <- rv$uia_crit_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'critTanUnits', 'Safe UIA Units', 
                        choices = list(
                          'Most Common'          = tanUnitsList[c(9, 13)],
                          'Un-ionized Ammonia-N' = tanUnitsList[c(10:12, 14:16)],
                          'Un-ionized Ammonia'   = tanUnitsList[25:32]
                        ),
                        selected = uia_units_init)
    }
    
    updateStore(session, session$ns("critTanUnits"), icUia()$units)
    
    idx <- which(icUia()$units == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), icUia()$units)
    
    my_crit_tan_value <- st()[[y]]
    
    if(length(my_crit_tan_value) == 0)
      my_crit_tan_value <- rv$uia_default[idx]
    
    updateSliderInput(session, "critTanVal", label = tanUnitsList[idx],
                      value = my_crit_tan_value,
                      min = tanUnitsMin[idx], max = tanUnitsMax[idx], 
                      step = tanUnitsStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), icUia()$units), my_crit_tan_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(icUia()$val, {
    
    if(rv$uia_crit_sl_init < 0) {
      
      rv$uia_crit_sl_init <- 1
      
      return()
    }
    
    idx <- which(icUia()$units == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), icUia()$units)
    
    my_crit_tan_value <- st()[[y]]
    
    
    if(length(my_crit_tan_value) == 0)
      my_crit_tan_value <- rv$uia_default[idx]
    else
      my_crit_tan_value <- icUia()$val
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), icUia()$units), my_crit_tan_value)
    
  })
  
  
  
  # ---- 4 crit_uia_vals  ----
  
  crit_uia_vals <- reactive({
    
    req(icTemp(), icSal(), 
        icPh(), 
        uiaPoSto(), 
        icTan()$val, icUia()$val,
        cancelOutput = T
        )
    
    temp        <- icTemp()$ic
    sal         <- icSal()$ic
    
    ph          <- icPh()$val   # NB: pH on NBS scale
    uia_percent <- uiaPoSto()   # returned on NBS scale
    
    ic_rho      <- calcRho(temp, sal)
    uia_posto   <- percentNh3ForTemp(temp, sal, icPh()$ic) # on FREE scale
    
    
    crit_tan    <- icUia()$val
    tan         <- icTan()$val
    
    # get indices of measured units and crit UIA units
    idx_tan <- which(tanUnitsList == icTan()$units)
    idx_uia <- which(tanUnitsList == icUia()$units)
    
    # cat('\nTAN input units: ', tanUnitsList[idx_tan], '\n')
    # cat('UIA input units: ', tanUnitsList[idx_uia], '\n\n')
    
    # # NB: does critPhFreeForTanMillero() -- below -- 'want' tan in mg TAN/KG???
    # # tanToIcUnits() -- now -- returns mg TAN/L
    tan_ic <- tanToIcUnits(icTan()$val, icTan()$units, ic_rho, uia_posto)
    # NB: SEND FREE or NBS scale for uia_posto...?
    tan_df <- tanToAllUnits(tan_ic, ic_rho, uia_posto, 6, 6)
    tan_mg_kg <- as.numeric(tan_df$vals[2])
    
    uia_mg_L_N_measured <- tan_ic * uia_posto / 100.0 # on FREE scale & mg/L TA-N
    
    # cat('**  MEASURED: ', icTan()$val, ' ', icTan()$units, '\n')
    # cat('      tan_ic: ', tan_ic, ' mg/L TA-N \n')
    # cat('        tan*: ', tan_mg_kg, ' mg/kg TA-N\n')
    # cat('       % UIA: ', uia_posto, '% (FREE SCALE) \n')
    # cat('       % UIA: ', uia_percent, '% (NBS SCALE) \n')
    
    # cat(' .. .. .. .. .. .. .. .. .. .. .. ..\n')
    
    crit_ic <- tanToIcUnits(icUia()$val, icUia()$units, ic_rho, uia_posto)
    crit_df <- tanToAllUnits(crit_ic, ic_rho, uia_posto, 6, 6) # NB: '6' dec_places, '6' num_digits
    
    crit_mg_L_uia_N  <- as.numeric(crit_df$vals[9])
    
    crit_mg_kg <- as.numeric(crit_df$vals[2])
    
    crit_mg_kg_uia_N <- as.numeric(crit_df$vals[10])
    
    # cat('\n||||||||||||||||||||||||||||\n')
    # cat('       UIA: ', tan_ic * (uia_percent / 100.0), ' mg/L TA-N (from NBS SCALE) \n')
    # cat('   --> UIA: ', as.numeric(tan_df$vals[idx_uia]), icUia()$units, '(from NBS SCALE) \n')
    # cat('       UIA: ', as.numeric(tan_df$vals[idx_tan]) * (uia_percent / 100.0), icTan()$units, '(from NBS SCALE) \n')
    # cat('       UIA: ', as.numeric(crit_df$vals[idx_tan]) * (uia_percent / 100.0), icUia()$units, '(from NBS SCALE) \n')
    # cat('||||||||||||||||||||||||||||\n\n')
    
    crit_uia_in_entered_units <- as.numeric(crit_df$vals[idx_tan])
    
    # cat('**  CRITICAL: ', icUia()$val, ' ', icUia()$units, '\n')
    # cat('     crit_ic: ', crit_ic, ' mg/L TA-N \n')
    
    # cat('   tan_mg_kg: ', tan_mg_kg, ' mg/kg TA-N\n')
    # cat('       crit*: ', crit_mg_kg, ' mg/kg TA-N\n')
    
    # cat('CRIT in ENTERED TAN units: ', 
    #     crit_uia_in_entered_units, ' ', icTan()$units, '\n')
    # 
    # cat('======================================\n')
    # 
    # cat('UIA measured: ', uia_mg_L_N_measured, ' mg/L  UIA-N \n')
    # cat('UIA critical: ', crit_mg_L_uia_N, ' mg/L UIA-N \n')
    # cat('UIA critical: ', crit_mg_kg_uia_N, ' mg/kg UIA-N \n')
    # cat('...for ENTERED AMMONIA UNITS of ', icTan()$units, ' ...\n')
    # cat('UIA critical: ', as.numeric(crit_df$vals[idx_tan]), ' ', tanUnitsList[idx_tan],' \n')
    
    diff_measured_to_critical <- uia_mg_L_N_measured - crit_mg_L_uia_N
    
    # cat('diff_measured_to_critical = ', diff_measured_to_critical, '\n\n')
    
    is_uia_critical <- ifelse(diff_measured_to_critical >= 0, 
                              paste0('UIA is OVER critical by ', abs(diff_measured_to_critical), ' mg/L UIA-N'), 
                              paste0('UIA is UNDER critical by ', abs(diff_measured_to_critical), ' mg/L UIA-N'))
    
    # cat('RESULT: ', is_uia_critical, '\n')
    
    
    # *** SAFE_PH, by manipulating pH -- given T, S, & TA-N -- to get UIA-N crit
    
    
    # if tan_mg_kg < crit_mg_kg_uia_N then (tan_mg_kg / crit_mg_kg_uia_N) <= 1
    # [TRY THIS] do NOT use tan_mg_kn, use CRIT_MG_KG
    # BUT...does NOT move target, the black ball in ggplot visualization
    critPh_FREE <- critPhFreeForTanMillero(crit_mg_kg, crit_mg_kg_uia_N, temp, sal)
    critPh_FREE_2 <- critPhFreeForTanMillero(tan_mg_kg, crit_mg_kg_uia_N, temp, sal)
    
    # cat('Does critPh_FREE have correct tan & crit_tan units (mg/kg *-N)??', critPh_FREE, '\n')
    
    critPh_NBS <- critPh_FREE - log10(ahFreeToSwsFactor(sal, temp, 0)) -
      log10(ahSwsToNbsFactor(sal, temp, 0))
    
    # cat('pH ',critPh_NBS, ' vs. pH ', critPh_FREE_2 - log10(ahFreeToSwsFactor(sal, temp, 0)) -
    #       log10(ahSwsToNbsFactor(sal, temp, 0)), '\n')
    # cat('------------\n\n\n')
    
    critPh_NBS <- round(critPh_NBS, 6)
    
    # critTan ----
    critTan <- round(as.numeric(crit_df$vals[idx_tan]), 4)
    
    # measured UIA | measured TA-N, pH, T, & S
    # NB: expressed in same units as entered critical UIA-N
    uia_measured_free <- round(as.numeric(tan_df$vals[idx_uia]), 4)
    # cat('uia_measured_free = ', uia_measured_free, ' ', tan_df$units[idx_uia], '\n')
    # cat('  uia_calc\'d = ', tanToAllUnits(tan_ic, ic_rho, uia_posto, 6, 6)$vals[idx_uia], ' ', tan_df$units[idx_uia], ' (FREE)\n')
    uia_measured_nbs <- as.numeric(tanToAllUnits(tan_ic, ic_rho, uia_percent, 6, 6)$vals[idx_uia])
    # cat('  uia_calc\'d = ', uia_measured_nbs, ' ', tan_df$units[idx_uia], ' (NBS)\n')
    delta_uia_nbs <- uia_measured_nbs - icUia()$val
    # cat('Delta UIA (NBS), measured - critical: ', uia_measured_nbs, ' - ', icUia()$val, ' = ', 
    #     delta_uia_nbs, ' ', tan_df$units[idx_uia], '\n')
    # cat('SAFE \'measured\' TA-N (NBS): ', tanToAllUnits(icUia()$val, icUia()$units, 
    #                                                    ic_rho, uia_percent)$vals[idx_tan], icTan()$units, '\n')
    # cat('\nmmmmmmmmmmmmmmmmmmmmmmmm\n')
    # cat('critTan = ', icTan()$val, ' - ', 
    #     as.numeric(crit_df$vals[idx_tan]), ' = ', critTan, '\n')
    # cat('mmmmmmmmmmmmmmmmmmmmmmmm\n')
    
    # cat('\n\n\nRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR\n')
    
    total_ammonia_actual <- icTan()$val
    total_ammonia_actual_ic <- tanToIcUnits(total_ammonia_actual, icTan()$units,
                                            ic_rho, uia_percent)  # uia_percent in NBS
    total_ammonia_actual_all <- tanToAllUnits(total_ammonia_actual_ic, ic_rho, uia_percent, 6, 6)
    un_ionized_ammonia_actual <- as.numeric(total_ammonia_actual_all$vals[idx_uia])
    
    total_ammonia_safe_ic <- tanToIcUnits(icUia()$val, icUia()$units, 
                                          ic_rho, uia_percent)
    total_ammonia_safe_all <- tanToAllUnits(total_ammonia_safe_ic, ic_rho, uia_percent, 6, 6)
    total_ammonia_safe <- as.numeric(total_ammonia_safe_all$vals[idx_tan])
    
    
    un_ionized_ammonia_safe_in_measured_TAN_units <- as.numeric(total_ammonia_actual_all$vals[idx_uia])
    un_ionized_ammonia_safe_in_measured_UIA_units <- as.numeric(total_ammonia_safe_all$vals[idx_tan + 8])
    # cat('\nun_ionized_ammonia_safe_in_measured_TAN_units: ', 
    #     un_ionized_ammonia_safe_in_measured_TAN_units, ' ', total_ammonia_actual_all$units[idx_uia], '\n')
    # cat('un_ionized_ammonia_safe_in_measured_UIA_units: ', 
    #     un_ionized_ammonia_safe_in_measured_UIA_units, ' ', total_ammonia_safe_all$units[idx_tan + 8], '\n\n')
    
    # cat(' ------------------------ CONCENTRATION PERSPECTIVE -----------------------\n\n')
    # cat('Actual Ammonia Measurement: ', total_ammonia_actual, ' ', icTan()$units, '\n')
    # cat(' Actual Un-ionized Ammonia: ', un_ionized_ammonia_actual, ' ', icUia()$units, '\n')
    # 
    # cat('        Safe Total Ammonia: ', total_ammonia_safe, ' ', icTan()$units, '\n')
    # cat('   Safe Un-ionized Ammonia: ', icUia()$val, ' ', icUia()$units, '\n')
    # 
    # cat('     Actual TA  -  Safe TA: ', total_ammonia_actual - total_ammonia_safe, ' ', icTan()$units, '\n')
    # cat('     Actual UIA - Safe UIA: ', un_ionized_ammonia_actual - icUia()$val, ' ', icUia()$units, '\n')
    
    # cat(' \n------------------------ pH PERSPECTIVE -----------------------\n\n')
    # 
    # cat('Actual pH (NBS): ', ph, '\n')
    # cat('If(f) Actual UIA > Safe UIA, what (lower) pH would reduce UIA_actual to UIA_safe? \n')
    
    # if(total_ammonia_actual > total_ammonia_safe) {
    # if(total_ammonia_actual > un_ionized_ammonia_safe_in_measured_TAN_units) {
    # un_ionized_ammonia_actual -- CONFORMABLE UNITS !!!
    # if((un_ionized_ammonia_actual - icUia()$val) > 0) {
    if(as.numeric(total_ammonia_actual_all$vals[2]) > 
       as.numeric(total_ammonia_safe_all$vals[10])) {
      
      z <- as.numeric(total_ammonia_safe_all$vals[idx_uia])
      
      # cat('un_ionized_ammonia_safe_in_measured_tan_units = ', z,
      #     ' ', icTan()$units, '\n')
      
      # to estimate safe pH, need MEASURED Total & ENTERED Safe in same units (like I.C.)
      # cat("Total Ammonia Measured IC: ", as.numeric(total_ammonia_actual_all$vals[2]), ' ', 
      #     total_ammonia_actual_all$units[2], '\n')
      # cat("              UIA SAFE IC: ", as.numeric(total_ammonia_safe_all$vals[10]), ' ', 
      #     total_ammonia_safe_all$units[10], '\n')
      
      # FREE scale all the way?
      x <- critPhFreeForTanMillero(as.numeric(total_ammonia_actual_all$vals[2]), # FREE or NBS? -- irrelevant
                                   as.numeric(total_ammonia_safe_all$vals[10]),   # FREE or NBS? -- calc'd from NBS...?
                                   temp, sal)
      
      # cat('pH as x FREE?? ', x, '\n')
      x <- x - log10(ahFreeToSwsFactor(sal, temp, 0)) - log10(ahSwsToNbsFactor(sal, temp, 0))
      
      # cat('pH as x NBS??  ', x, '\n')
      x <- round(x, 6)
      
      
      # cat('total_ammonia_actual > total_ammonia_safe: The Safe pH for the measured ammonia = ', 
      #     x, ' (', percentNh3ForTemp(temp, sal, x), '% un-ionized) \n')
      # cat('back-calc UIA: ',icTan()$val * percentNh3ForTemp(temp, sal, x) / 100.0, '\n')
    } else {
      
      # cat('total_ammonia_actual <= total_ammonia_safe: GOOD-TO-GO! \n')
      # cat('BUT ... at what minimum (critical, unsafe) pH is total_ammonia_actual > total_ammonia_safe?\n')
      x <- 12
    }
    
    # cat('RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR\n\n\n')
    
    # deltaPh  <- round((input$phSlider - critPh_NBS), 4)
    
    # cat('icPh()$ic = ', icPh()$ic, '\n')
    # cat('        x = ', x, '\n')
    # cat('  deltaPh =', icPh()$ic - x, '\n')
    # cat('  deltaPh =', round(icPh()$ic - x, 4), '\n')
    
    deltaPh  <- round((icPh()$ic - x), 4)
    deltaTan <- round((tan - critTan), 4)
    
    deltaPh <- round((ph - critPh_NBS), 4)
    
    crit_data <- data.frame(critPh_NBS = x,            # critPh_NBS = critPh_NBS,
                            critTan    = critTan,      # which units??
                            
                            deltaPh    = deltaPh,
                            deltaTan   = deltaTan,     # which units??
                            
                            x_coord    = ph,           # NBS scale
                            y_coord    = uia_percent,  # NBS scale
                            
                            crit_uia_in_entered_units = as.numeric(tan_df$vals[idx_uia]),
                            
                            alert      = 'n/a',
                            
                            stringsAsFactors = F)
    
  })
  
  
  
  # ---- calcUiaPoSto ----
  # to plot UIA % as f(pH)
  # (another for g(TAN))
  
  calcUiaPoSto <- function(temp, sal) {
    
    # NB: temp & sal in I.C. units
    
    posto.range <- c()
    
    ph.min <- 6.0
    ph.max <- 11.0
    p.step <- 0.01
    
    # on NBS scale
    ph.domain <- seq(ph.min, ph.max, p.step)
    
    for(ph in ph.domain) {
      
      ph.free <- phNbsToPhFree(ph, sal, temp, 0)
      
      uia_percent_FREE <- percentNh3ForTemp(temp, sal, ph.free)
      
      uia_percent_NBS <- uia_percent_FREE - 
        log10(ahFreeToSwsFactor(sal, temp, 0)) -
        log10(ahSwsToNbsFactor(sal, temp, 0))
      
      posto.range <- c(posto.range, uia_percent_NBS)
    }
    
    df <- data.frame(pH      = ph.domain, 
                     percent = posto.range,
                     
                     stringsAsFactors = F)
    
  }
  
  
  
  
  # ---- uiaPoSto ----
  
  uiaPoSto <- reactive({
    
    # invalidateLater(2000, session)
    
    req(icTemp(), 
        icSal(), 
        icPh(), 
        cancelOutput = T
        )
    
    temp <- icTemp()$ic
    sal  <- icSal()$ic
    ph   <- icPh()$ic
    
    # ph.free <- phNbsToPhFree(icPh()$ic,
    #                          sal,
    #                          temp,
    #                          0)
    
    # uia_percent_FREE <- percentNh3ForTemp(temp, sal, ph.free)
    uia_percent_FREE <- percentNh3ForTemp(temp, sal, ph)
    
    uia_percent_NBS <- uia_percent_FREE - 
      log10(ahFreeToSwsFactor(sal, temp, 0)) -
      log10(ahSwsToNbsFactor(sal, temp, 0))
    
    # cat('in uia_module_HC.R, uiaPoSto...\n')
    # cat('   A. uia_posto (FREE): ', uia_percent_FREE, '\n')
    # cat('   B.  uia_posto (NBS): ', uia_percent_NBS, '\n\n')
    
    uia_percent_NBS
    
  })
  
  
  
  # ---- Box 'warning' titles ----
  
  output$uiaTitleText <- renderText({
    
    req(
        uiaPoSto(), 
        cancelOutput = T
        )
    
    uia_percent <- uiaPoSto()
    
    # idx_temp <- which(input$tempUnits == tempUnitsList)
    # idx_sal <- which(input$salUnits == salUnitsList)
    
    # paste0(round(uiaPoSto(), 2),'% UIA at pH ',
    #        # input$tempSlider, ' ', input$tempUnits)
    #        icPh()$val, ', ',
    #        icTemp()$val, ' ', icTemp()$units, ', ',
    #        icSal()$val, ' ', icSal()$units)
    
    # if(9 <= icPh()$val) status = 'primary'
    
    # HTML('% UIA <em>vs</em> pH (current UIA = ', round(uiaPoSto(), 2), '% of TAN)')
    
    paste0('% UIA vs. pH  (current UIA = ', round(uia_percent, 2), '% of TAN)')
           
  })
  
  
  # ---- UIA Plot SUMMARY ----
  
  output$uia_plot_summary <- renderUI({
    
    below_or_above_color <- ifelse(icPh()$val < crit_uia_vals()$critPh_NBS, 
                                   'color: green', 'color: red')
    
    uia_posto <- percentNh3ForTemp(icTemp()$ic, icSal()$ic, icPh()$ic) # on FREE scale
    # cat('free: ', uia_posto, ' vs. nbs: ', uiaPoSto(), '\n')
    
    # NB: units of % UIA as UIA or UIA-N
      str1 <- tags$strong(paste0(round(uia_posto, 2), '% ', word(icUia()$units, 2))) # FREE scale
      # str1 <- tags$strong(paste0(round(uiaPoSto(), 2), '% ', word(icUia()$units, 2))) # NBS scale
      str2 <- paste0('safe pH: ', round(crit_uia_vals()$critPh_NBS, 3)) # NBS scale
      # str2 <- paste0('critical pH: ', round(crit_uia_vals()$critPh_NBS, 3), ' ', '(NBS)')
      # str3 <- paste0('at pH ', icPh()$val, ' ', '(NBS), ')
      str3 <- paste0('at pH ', icPh()$val, ' ', ', ')
      str4 <- paste0(icTemp()$val, ' ', icTemp()$units, ', ')
      str5 <- paste0('and ', icSal()$val, ' ', icSal()$units)
      
      str1 <- tags$span(style = below_or_above_color, str1)
      str2 <- tags$span(style = below_or_above_color, str2)
      
      wellPanel(style = 'background-color: azure; border-style: solid; border-color: black;', 
        
        HTML(paste(tags$h4(str1, align = 'center'), 
                   tags$h4(str2, align = 'center'), 
                   tags$hr(),
                   tags$h5(str3, align = 'center'), 
                   tags$h5(str4, align = 'center'), 
                   tags$h5(str5, align = 'center')))
      )
    
  })
  
  
  # ---- critTitleText ----
  
  output$critTitleText <- renderText({
    
    paste0('Critical Levels')
  })
  
  
  # HIGH_CHART, % vs pH ----
  
  # base_plot <- reactive({
  #   
  #   req(
  #     icTemp(), icSal(),
  #     cancelOutput = T
  #   )
  #   
  #   df_graph <- calcUiaPoSto(icTemp()$ic, icSal()$ic)
  #   
  #   hc <- hchart(df_graph, "line", hcaes(x = pH, y = percent)) %>% 
  #     hc_add_theme(hc_theme_538())
  #   
  #   hc
  #   
  # })
  
  # annotate_plot <- reactive({
  #   
  #   req(
  #     crit_uia_vals(),
  #     cancelOutput = T
  #   )
  #   
  #   ph          <- crit_uia_vals()$x_coord
  #   uia_percent <- crit_uia_vals()$y_coord
  #   
  #   my_hc_list <- list(xValue=ph, yValue=uia_percent, title=list(text="evo!"))
  #   
  #   my_hc_list
  #   
  #   # hc <- base_plot() %>% hc_annotations(my_hc_list)
  #   
  # })

  
  # output$uiaVsPhPlot_HC <- renderHighchart({
  #   
  #   req(
  #     base_plot(),
  #     # annotate_plot(),
  #     # icTemp(), icSal(), 
  #     # icPh(), 
  #     # uiaPoSto(),
  #     cancelOutput = T
  #   )
  #   
  #   hc <- base_plot() %>% hc_annotations(annotate_plot())
  #   # hc <- base_plot()
  #   
  #   # hc <- hc %>% hc_annotations(annotate_plot())
  #   
  #   # df_graph <- calcUiaPoSto(icTemp()$ic, icSal()$ic)
  #   
  #   # myPh <- icPh()$val
  #   
  #   # cat('icPh()$val = ', isolate(myPh), '\n')
  #   
  #   # hchart(df_graph, "line", hcaes(x = pH, y = percent)) %>%  
  #   #   hc_add_theme(hc_theme_538()) %>% 
  #     
  #     # hc_add_theme(hc_theme_handdrawn())
  #     # hc_add_theme(hc_theme_chalk()) %>% 
  #     # hc_annotations(list(xValue=myPh, yValue=uiaPoSto(), title=list(text="evo!")))
  # 
  # })
  
  
  
  # ---- ggplot, % vs pH ----
  
  
  output$uiaVsPhPlot <- renderPlot({

    # cat('\nABOVE ... IN ... output$uiaVsPhPlot \n')
    # cat('------------------------------------- \n')

    req(
        icTemp(),
        icSal(),
        icPh(),
        uiaPoSto(),
        icTan()$val,
        icUia()$val,
        cancelOutput = T)

    # cat('BELOW ...  IN ... output$uiaVsPhPlot \n')
    # cat('===================================== \n\n')

    uia_percent <- uiaPoSto()

    crit_tan <- icUia()$val

    tan <- icTan()$val
    # tan <- icTan()$ic

    # if(tan > crit_tan) {

      # critPh_FREE <- critPhFreeForTanMillero(tan, crit_tan, icTemp()$ic, icSal()$ic)
      # 
      # critPh_NBS <- critPh_FREE - log10(ahFreeToSwsFactor(icSal()$ic, icTemp()$ic, 0)) -
      #   log10(ahSwsToNbsFactor(icSal()$ic, icTemp()$ic, 0))
      # 
      # critPh_NBS <- round(critPh_NBS, 3)
      # critPh_NBS_alert <- round(critPh_NBS - 0.20, 3)

      # ... can get from crit_uia_vals() ...
            critPh_NBS <- crit_uia_vals()$critPh_NBS
            if(12 == critPh_NBS)
              critPh_NBS <- Inf
            
            critTan    <- crit_uia_vals()$critTan
            deltaPh    <- crit_uia_vals()$deltaPh
            deltaTan   <- crit_uia_vals()$deltaTan
            
            critPh_NBS_alert <- round(critPh_NBS - 0.20, 3)
      
      # ----- NB: NOT NEEDED TO PLOT ?? ---
      # critTan UNDEFINED when tan <= icUia()$val ??
      # critTan <- round(crit_tan / (uia_percent / 100.0), 4)
      # 
      # deltaTan <- round((tan - critTan), 4)
      # 
      # deltaPh <- round((icPh()$val - critPh_NBS), 4)


      df_graph <- calcUiaPoSto(icTemp()$ic, icSal()$ic)

      bad <- round(uia_percent, 2)


      p <- df_graph %>% ggplot(aes(pH, percent)) + 
        
        geom_line() + 
        
        labs(y = "Percent UIA") +
        
        theme(axis.title.y = element_text(size = rel(1.2), face = 'bold'),
              axis.title.x = element_text(size = rel(1.2), face = 'bold')) +
        
        # coord_cartesian(xlim = c(0, 12), 
        #                 ylim = c(0, 100),
        #                 expand = F) +

        geom_vline(xintercept = critPh_NBS_alert, col = 'green') +
        geom_vline(xintercept = critPh_NBS, col = 'red') +
        
        # geom_vline(xintercept = c(critPh_NBS_alert, critPh_NBS), col = paste0(c('green', 'red')))
        
        # geom_abline(slope = c(1, 5, 10), intercept = c(0.1, 0.2, 0.5), col = 'blue') +

        
        # geom_rect(xmin = -Inf,    xmax = critPh_NBS, ymin = -Inf, ymax = Inf,   fill = "blue") +
        # scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = 50) +
        annotate("rect", xmin=-Inf, xmax=(critPh_NBS - 0.2), ymin=-Inf, ymax=Inf, fill="green", alpha=0.4) +
        annotate("rect", xmin=(critPh_NBS - 0.2), xmax=critPh_NBS, ymin=-Inf, ymax=Inf, fill="yellow", alpha=0.4) +
        annotate("rect", xmin=critPh_NBS, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=0.4) +
        
    # pH-dependent below ...
        # annotate("text", x = 7.0, y = Inf,
        #          label = paste0(round(uia_percent, 2),'% UIA\n',
        #                         'pH ', icPh()$val, ' ', '(NBS)\n',
        #                         icTemp()$val, ' ', icTemp()$units, '\n',
        #                         icSal()$val,  ' ', icSal()$units),
        #          hjust = 0.75, vjust = 1.5) +
        
        geom_point(aes(icPh()$val, bad), size=3)
        # geom_point(aes(icPh()$ic, bad), size=3)

      # x <- ggplotly(p)
      # x
      
      p

})
  
  
  
  
  # ---- unIonAmmValBox ----
  
  output$unIonAmmValBox <- renderInfoBox({
    
    req(
        # icTemp(), 
        # icSal(), 
        # icPh(), 
        # uiaPoSto(), 
        crit_uia_vals()$critPh_NBS != 'n/a', # avoid error msg when tan < critTan
        icTan()$val,
        # icUia()$val,
        cancelOutput = T)
    
    
    tan <- icTan()$val  # NB: WHICH UNITS ??
    
    crit_uia_in_entered_units <- crit_uia_vals()$crit_uia_in_entered_units
    
    # cat('crit_uia_in_entered_units = ', crit_uia_in_entered_units, '\n')
    
    critPh_NBS  <- crit_uia_vals()$critPh_NBS
    ph          <- crit_uia_vals()$x_coord
    uia_percent <- crit_uia_vals()$y_coord
    
    critPh_NBS_alert <- round(critPh_NBS - 0.20, 5)
    
    
    if(round(crit_uia_vals()$crit_uia_in_entered_units, 3) >= icUia()$val) {
    # if(ph >= critPh_NBS) {

      thumbs = 'thumbs-down'
      col = 'red'
    } else if(ph > critPh_NBS_alert) {

      thumbs = 'alert'
      col = 'yellow'
    } else {

      thumbs = 'thumbs-up'
      col = 'green'
    }
    
    # str <- paste0(round((uia_percent / 100) * tan, 6),' mg UIA/L')
    
    str <- paste0(round(crit_uia_vals()$crit_uia_in_entered_units, 3), ' ', 
                  icUia()$units)
     
    infoBox(
      # value = paste0(uiaConc,' mg UIA/L'),
      value = str,
      # value = HTML(str_1, str_2, sep = '<br/>'),
      "Un-ionized Ammonia",
      # value = paste0(uiaConc,' mg/L (critical pH: ',critPh_NBS,')'), "Un-ionized Ammonia",
      icon = shiny::icon(thumbs, lib = "glyphicon"),
      color = col,
      fill = T
    )
    
  })
  
  
  
  # ---- df vs crit_uia_vals ... ----
  
  df <- reactive({
    
    req(icTemp(), 
        icSal(), 
        icPh(), 
        uiaPoSto(), 
        icTan()$val,
        icUia()$val,
        cancelOutput = T)
    
    uia_percent <- uiaPoSto()
    tan         <- icTan()$val
    crit_tan    <- icUia()$val
    
    
    # if(!is.nan(critPh_FREE)) {
    if(tan > crit_tan) {
      
      critPh_FREE <- critPhFreeForTanMillero(tan, crit_tan, icTemp()$ic, icSal()$ic)
      
      critPh_NBS <- critPh_FREE - log10(ahFreeToSwsFactor(icSal()$ic, icTemp()$ic, 0)) -
        log10(ahSwsToNbsFactor(icSal()$ic, icTemp()$ic, 0))
      
      critPh_NBS <- round(critPh_NBS, 3)
      
      # critTan UNDEFINED when tan <= icUia()$val ??
      critTan <- round(icUia()$val / (uia_percent / 100.0), 4)
      
      # deltaPh <- round((input$phSlider - critPh_NBS), 4)
      deltaTan <- round((icTan()$val - critTan), 4)
      
      deltaPh <- round((icPh()$val - critPh_NBS), 4)
      
    } else {
      # shinyjs::info ----
      # shinyjs::info("TAN cannot be less than UIA\nPlease enter either a higher TAN\nor a lower UIA")
      
      critPh_NBS <- 'n/a'
      critTan <- 'n/a'
      
      deltaPh <- 'n/a'
      deltaTan <- 'n/a'
    }
    
    my_df <- data.frame(current = c(icPh()$val, icTan()$val),
                        critical = c(critPh_NBS, critTan),
                        diff = c(deltaPh, deltaTan),
                         
                         stringsAsFactors = F)
    
    my_df
    
  })
  
  
  
  # ---- DT ----
  
  # ===+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # **** NB: replaceData doesn't work in module namespace
  # see: https://github.com/rstudio/DT/issues/359 for workaround 
  # **** NB: MUST have "rownames = c('pH', 'TAN')", not "rownames = F"
  # ===+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  proxy = dataTableProxy(session$ns('crit_data_df'))

  observe({
    
    # replaceData(proxy, df(), rownames = c('current', 'critical', '∆'), resetPaging = FALSE)
    # replaceData(proxy, df(), rownames = c('pH', 'TAN'), resetPaging = FALSE)

    dataTableAjax(session, df(), rownames = c('pH', 'TAN'), outputId = 'crit_data_df')
    reloadData(proxy, resetPaging = FALSE)
  })


  # output$crit_data_df <- DT::renderDataTable(
  # 
  #   datatable( isolate(df()),
  # 
  #                colnames = c('current', 'critical', '∆'),
  #                rownames = c('pH', 'TAN'),
  # 
  #                options = list(dom = 't',
  #                               'bSort' = F,
  #                               'bInfo' = F,
  #                               # pageLength = 3,
  #                               pageLength = 2,
  # 
  #                               # see: https://rstudio.github.io/DT/010-style.html
  #                               # columnDefs = list(list(targets = 4, visible = F),
  #                               #                   list(className = 'dt-center', targets = c(1:3))),
  # 
  #                               # see: http://rstudio.github.io/DT/options.html
  #                               initComplete = JS(
  #                                 "function(settings, json) {",
  #                                 "$(this.api().table().header()).css({'background-color': 'orange', 'color': '#000'});",
  #                                 "}")
  # 
  #                )
  #              # # see: https://stackoverflow.com/questions/42099418/how-can-i-reduce-row-height-in-dt-datatables
  #              #   %>% formatStyle( colnames(df())[1], 
  #              #                    target= 'row',
  #              #                    color = 'black', 
  #              #                    backgroundColor = 'yellow', 
  #              #                    fontWeight ='bold', 
  #              #                    lineHeight='70%')
  #     )
  # )
  
  
  # Crit Data Output, L & R ----
  
  output$crit_data_df_left <- renderUI({
    
    req(icTemp(),
        icSal(),
        icPh(),
        uiaPoSto(),
        crit_uia_vals(),
        icTan()$val,
        icUia()$val,
        cancelOutput = T)
    
    below_or_above <- ifelse(crit_uia_vals()$deltaTan < 0, 'below', 'in')
    below_or_above_color <- ifelse(crit_uia_vals()$deltaTan < 0, 'color: green', 'color: red')

    str2 <- paste0('pH ', icPh()$val,', ',
                   icTemp()$val, ' ', icTemp()$units, ', ',
                   icSal()$val, ' ', icSal()$units, '\n')
    str1 <- paste0('Safe TAN: ',
                   round(crit_uia_vals()$critTan, 3), ' ',
                   icTan()$units, '\n')
    str3 <- paste0(round(abs(crit_uia_vals()$deltaTan), 2),
                   ' ', icTan()$units, ' ',
                   below_or_above, ' the Red Zone\n')

    str1 <- tags$span(style = below_or_above_color, str1)

    HTML(paste(tags$h5(str2, align = 'center'), 
               tags$h4(str1, align = 'center'), 
               tags$h5(str3, align = 'center')))
    # HTML(paste(tags$h2('str1'), tags$h2('str2'), tags$h2('str3'), tags$br(), sep = '<br/>'))
  })
  
  
  output$crit_data_df_right <- renderUI({

    req(icTemp(),
        icSal(),
        icPh(),
        uiaPoSto(),
        crit_uia_vals(),
        icTan()$val,
        icUia()$val,
        cancelOutput = T)
    
    
    below_or_above       <- ifelse(crit_uia_vals()$deltaTan < 0, 'below', 'in')
    below_or_above_color <- ifelse(crit_uia_vals()$deltaTan < 0, 'color: green', 'color: red')
    
    # below_or_above <- ifelse(crit_uia_vals()$deltaTan < 0, 
    #                          list('below', 'green'), list('in', 'red'))
    

    str2 <- paste0(icTan()$val, ' ', icTan()$units, ', ',
                   icTemp()$val, ' ', icTemp()$units, ', ',
                   icSal()$val, ' ', icSal()$units, '\n')
    str4 <- paste0('Safe pH: ', round(crit_uia_vals()$critPh_NBS, 3), '\n')
    str3 <- paste0(abs(round(icPh()$val - crit_uia_vals()$critPh_NBS, 2)),
                   ' pH units ', below_or_above, ' the Red Zone\n')
    
    # str4 <- tags$span(style = paste0('color: ', below_or_above[2]), str4)
    str4 <- tags$span(style = below_or_above_color, str4)

    HTML(paste(tags$h5(str2, align = 'center'), 
               tags$h4(str4, align = 'center'), 
               tags$h5(str3, align = 'center')))
    # HTML(paste(tags$h2('str1'), tags$h2('str2'), tags$h2('str3'), tags$br(), sep = '<br/>'))
  })
  
}