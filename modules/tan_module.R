# TAN module functions for
# "iQuaCalc (Lite)" dashboard


tanModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 9,
             
             selectInput(ns('ammoniaUnits'), 'Ammonia Units', 
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
      
      column(width = 11,
             
             column( width = 8,
                     sliderInput(ns('ammoniaSlider'), 'Ammonia', 
                                 min = 0, max = 45, value = 33, step = 0.1)
             )
      )
    )
    
  )
}



tanModule <- function(input, output, session,
                      icTemp, icSal, icPh, st) {
  
  
  # ---- callModule for T, S, pH  ----
  # icTemp <- callModule(temperatureModule, 'temp_inner_ammonia', reactive(st()))
  # icSal  <- callModule(salinityModule, 'sal_inner_ammonia', reactive(icTemp()), reactive(st()))
  # icPh   <- callModule(phModule, 'ph_inner_ammonia', reactive(icTemp()), reactive(icSal()), reactive(st()))
  
  
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
  observeEvent(input$ammoniaUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('ammoniaUnits')
      
      rv$select_init <- 1
      
      tan_units_init <- st()[[x]]
      
      if(length(tan_units_init) == 0)
        tan_units_init <- rv$tan_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'ammoniaUnits', 'Ammonia Units', 
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
    
    updateStore(session, session$ns("ammoniaUnits"), input$ammoniaUnits)
    
    idx <- which(input$ammoniaUnits == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), input$ammoniaUnits)
    
    my_tan_value <- st()[[y]]
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    
    updateSliderInput(session, "ammoniaSlider", label = tanUnitsList[idx],
                      value = my_tan_value,
                      min = tanUnitsMin[idx], max = tanUnitsMax[idx], step = tanUnitsStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$ammoniaUnits), my_tan_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$ammoniaSlider, {
    
    if(rv$tan_sl_init < 0) {
      
      rv$tan_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$ammoniaUnits == tanUnitsList)
    
    y <- paste0(session$ns('sl_'), input$ammoniaUnits)
    
    my_tan_value <- st()[[y]]
    
    
    if(length(my_tan_value) == 0)
      my_tan_value <- rv$tan_default[idx]
    else
      my_tan_value <- input$ammoniaSlider
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$ammoniaUnits), my_tan_value)
    
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

      idx_tan <- which(input$ammoniaUnits == tanUnitsList)

      ic_rho <- calcRho(my_icTemp, my_icSal)

      uia_posto_free <- percentNh3ForTemp(my_icTemp, my_icSal, my_icPh)  # on FREE scale

      uia_posto <- uia_posto_free -
        log10(ahFreeToSwsFactor(my_icSal, my_icTemp, 0)) -
        log10(ahSwsToNbsFactor(my_icSal, my_icTemp, 0))                  # on NBS scale

      icTan <- tanToIcUnits(input$ammoniaSlider, input$ammoniaUnits, ic_rho, uia_posto)


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
      tan_list <- list(df    = df,
                       ic    = icTan,
                       val   = input$ammoniaSlider,
                       units = input$ammoniaUnits
                       )

      # 2/2 "duct tape" solution ... ----
      rv$duct_tape_2 <- 2

      tan_list
    }

  })


  return(df_tan)
  
}