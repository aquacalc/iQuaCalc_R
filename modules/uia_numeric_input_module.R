# module for entering & converting a UIA value
# "iQuaCalc (Lite)" dashboard


uiaNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # useShinyjs(),
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                numericInput(ns('uia_input'), 'UIA', 
                             min = 0, max = 45, value = 33, step = 0.1),
                
                selectInput(ns('uia_units'), 'UIA Units',
                #             choices = list(
                #               'Un-ionized Ammonia-N' = tanUnitsList[9:16],
                #               'Un-ionized Ammonia' = tanUnitsList[25:32]
                #             ),
                            # choices = uiaUnitsSet
                            choices = list(
                              'Most Common'          = tanUnitsList[c(9, 13)],
                              'Un-ionized Ammonia-N' = tanUnitsList[c(10:12, 14:16)],
                              'Un-ionized Ammonia'   = tanUnitsList[25:32])
                ),
                
                checkboxInput(ns('tc_show_uia_zone'), 'Show', value = F)
                
    )
    
  )
}


# for wq_map_module.R (and uia_module_HC_R?), add params to to run TAN input and UIA input separately
# * tan_units_default: 'mg/L TA-N' & 'μg/L UIA-N'
# * updateSelectInput label & choices

# NB: pH used in tan_module -- on which *this* module for wq_map_module.R is modled -- ONLY to calc %UIA
#     but the implementation in wq_map_module.R uses it ONLY to calc critical pH, at which UIA(-N)
#     exceeds the user's max allowable level
# And once have critPh on the NBS scale, 

uiaNumericModule <- function(input, output, session,
                             icTemp, icSal, 
                             icPh,                     # pH in this module ONLY to calc %UIA, needed for conversions 
                             # choices_1, choices_2,   # for selectInput, when want TA(-N) or UIA(-N) choices
                             show_tc_flag,
                             st) {
  
  
  # ---- callModule for T, S, pH  ----
  # icTemp <- callModule(temperatureModule, 'temp_inner_ammonia', reactive(st()))
  # icSal  <- callModule(salinityModule, 'sal_inner_ammonia', reactive(icTemp()), reactive(st()))
  # icPh   <- callModule(phModule, 'ph_inner_ammonia', reactive(icTemp()), reactive(icSal()), reactive(st()))
  
  
  # "*_init" flags when app is (re-)launched ----
  rv <- reactiveValues(cb_init = -1, 
                       cb_default = 0,
                       select_init = -1,
                       tan_units_default = 'μg/L UIA-N',
                       tan_sl_init = -1,
                       duct_tape_2 = -1,
                       tan_default = uiaUnitsSet
                       # tan_default = tanUnitsSet
                       # tan_default = c(2, 2, 20, 20, 0.2, 0.2,
                       #                 1, 1, 10, 10, 0.1, 0.1,
                       #                 
                       #                 2, 2, 20, 20, 0.2, 0.2,
                       #                 1, 1, 10, 10, 0.1, 0.1
                       # )
  )
  
  
  # ---- TC ----
  
  # [KLUDGE]
  # NB: for use in uia_module_HC_2.R, do not want visible "Show" checkbox,
  #     so use a flag in module server function param list to implement this requirement
  
  observe({
    
    if(!show_tc_flag) {
      
      shinyjs::hide('tc_show_uia_zone')
    }
    
  })
  
  
  # Observe CHECKBOX_INPUT input, store when changed
  observeEvent(input$tc_show_uia_zone, {
    
    if(rv$cb_init < 0) {
      
      rv$cb_init <- 1
      
      x <- session$ns('tc_show_uia_zone')
      
      my_tc_value <- st()[[x]]
      
      if(length(my_tc_value) == 0)
        my_tc_value <- rv$cb_default
      
      updateCheckboxInput(session, 'tc_show_uia_zone', 'Show',
                          value = as.integer(my_tc_value))
      
      updateStore(session, session$ns("tc_show_uia_zone"), as.integer(my_tc_value))
      
      return()
    }
    
    updateStore(session, session$ns("tc_show_uia_zone"), as.integer(input$tc_show_uia_zone))
    
  })
  
  
  # ---- TAN ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$uia_units, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('uia_units')
      
      rv$select_init <- 1
      
      tan_units_init <- st()[[x]]
      
      if(length(tan_units_init) == 0)
        tan_units_init <- rv$tan_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'uia_units', 'UIA Units', 
                        # choices = list(
                        #   'Un-ionized Ammonia-N' = tanUnitsList[9:16],
                        #   'Un-ionized Ammonia' = tanUnitsList[25:32]
                        # ),
                        # choices = uia_units,
                        choices = list(
                        'Most Common'          = tanUnitsList[c(9, 13)],
                        'Un-ionized Ammonia-N' = tanUnitsList[c(10:12, 14:16)],
                        'Un-ionized Ammonia'   = tanUnitsList[25:32]),
                        selected = tan_units_init)
    }
    
    updateStore(session, session$ns("uia_units"), input$uia_units)
    
    idx <- which(input$uia_units == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), input$uia_units)
    
    my_tan_value <- st()[[y]]
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    
    updateNumericInput(session, "uia_input", label = tanUnitsList[idx],
                      value = my_tan_value,
                      min = tanUnitsMin[idx], max = tanUnitsMax[idx], step = tanUnitsStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$uia_units), my_tan_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$uia_input, {
    
    if(rv$tan_sl_init < 0) {
      
      rv$tan_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$uia_units == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), input$uia_units)
    
    my_tan_value <- st()[[y]]
    
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    else
      my_tan_value <- input$uia_input
    
    
    # update slider value for current units
    updateStore(session, 
                paste0(session$ns('sl_'), input$uia_units), 
                my_tan_value)
    
  })
  
  
  
  df_tan <- reactive({
    
    # req(input$uia_units,
    #     cancelOutput = T
    #     )

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

      idx_tan <- which(input$uia_units == tanUnitsList)

      ic_rho <- calcRho(my_icTemp, my_icSal)

      uia_posto_free <- percentNh3ForTemp(my_icTemp, my_icSal, my_icPh)  # on FREE scale

      uia_posto <- uia_posto_free -
        log10(ahFreeToSwsFactor(my_icSal, my_icTemp, 0)) -
        log10(ahSwsToNbsFactor(my_icSal, my_icTemp, 0))                  # on NBS scale

      icTan <- tanToIcUnits(input$uia_input, input$uia_units, ic_rho, uia_posto)


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
      tan_list <- list(df    = df,                   #    df[2]: 'mg/kg TA-N'
                       ic    = icTan,                # IC Units: 'mg/L TA-N'
                       val   = input$uia_input,
                       units = input$uia_units,
                       
                       show_uia_zone = input$tc_show_uia_zone
                       )

      # 2/2 "duct tape" solution ... ----
      rv$duct_tape_2 <- 2

      tan_list
    }

  })


  return(df_tan)
  
}