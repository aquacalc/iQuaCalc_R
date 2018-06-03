# temperature module functions for
# "iQuaCalc (Lite) salinity module.R"

# @param show_tables: '1', show DT; '0', hide -- just show temperature controls
temperatureModuleInput <- function(id, show_tables) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
    # splitLayout(cellWidths = c('35%', '65%'),
                numericInput(ns('tempSlider_convert'), 'Temperature', 
                             min = 4, max = 42, value = 28, step = 0.1),
                
                selectInput(ns('tempConvertUnits'), 'Temperature Units', 
                            choices = tempUnitsList),
                
                tags$h6()
                
    ),
    
    # NB: only display user-highlighted conversion and (full) conversion result table
    #     in Temperature Conversion page
    # see: https://stackoverflow.com/questions/39109671/r-shiny-modules-with-conditionalpanel-and-reactives
    
    # better...see: https://gist.github.com/tbadams45/49c71a4314f6b4f299583f4ba96fee54
    # sprintf("input['%s'] != ''", ns("mySelect"))
    # ---->  input[tgp_calc-temp_for_tgp-tempSlider_convert] >= 35 
    # ---->  input[ammonia_convert-temp_inner_ammonia_convert-tempSlider_convert] >= 35 
    # ---->  input[ammonia_convert-temp_inner_ammonia_convert-tempSlider_convert] >= 35
    
    # conditionalPanel("input['temp_convert-tempSlider_convert'] >= 35",
    # conditionalPanel(sprintf("input['%s'] > 35", ns("tempSlider_convert")),
    # conditionalPanel("14 > 35",
    conditionalPanel(sprintf("%s > 0", show_tables), # if show_tables > 0, show temperature conversions DT
                     
                     fluidRow( width = 11, style = "padding: 0px 12px 0px 12px;",
                               
                               conversionDisplayModuleInput(ns('temp_conversion_result'))
                               
                     ),
                     
                     fluidRow( width = 11,
                               
                               datatableModuleInput(ns('temp_convert_dt'), 11, '130')
                     )
    )
    
  )
}



temperatureModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       temp_units_default = 'C',
                       temp_sl_init = -1,
                       temp_default = tempSet)
                       # temp_default = c(30, 82, 301.15))

  
  # ---- for displaying a single conversion result from table cell click
  callModule(conversionDisplayModule, 'temp_conversion_result',
             reactive(input$tempSlider_convert), reactive(input$tempConvertUnits),
             reactive(df_temp()), 
             reactive(x()$info), # table id + "_cell_clicked'
             reactive(x()$selected),
             reactive(additional_conversion_info()),
             reactive(st()))
  
  
  x <- callModule(datatableModule,
                  'temp_convert_dt',
                  reactive(df_temp()),
                  3, 3)     # nrow, ncol
  
  
  # NB: conditionally hide Temp result table and user-highlighted conversion
  #     to accommodate modules that require only Temp input controls
  # observe({
  #   shinyjs::hide('temp_conversion_result')
  #   shinyjs::hide('temp_convert_dt')
  # })
  
  
  # additional_conversion_info ----
  
  additional_conversion_info <- reactive({
    
    NULL   # no additional info to display for temperature
    
  })
  
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$tempConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {

      x <- session$ns('tempConvertUnits')

      rv$select_init <- 1

      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "tempConvertUnits",
                        label = 'Temperature Units', choices = tempUnitsList,
                        selected = st()[[x]])
    }

    updateStore(session, session$ns("tempConvertUnits"), input$tempConvertUnits)


    idx_t <- which(input$tempConvertUnits == tempUnitsList)

    y <- paste0(session$ns('sl_'), input$tempConvertUnits)
    

    my_temp_value <- st()[[y]]

    if(length(my_temp_value) == 0)
      my_temp_value <- rv$temp_default[idx_t]
    

    updateNumericInput(session, "tempSlider_convert", label = tempUnitsList_short[idx_t],
                      value = my_temp_value,
                      min = tempMin[idx_t], max = tempMax[idx_t], step = tempStep[idx_t])
    
    freezeReactiveValue(input, "tempSlider_convert")
    
    # update slider value for current units ???
    # updateStore(session, paste0(session$ns('sl_'), input$tempConvertUnits), my_temp_value)
    # NB: cover case of re-launch with out-of-bounds temp ----
    if(input$tempSlider_convert < tempMin[idx_t] || input$tempSlider_convert > tempMax[idx_t])
      return()
    else
      updateStore(session, paste0(session$ns('sl_'), input$tempConvertUnits), my_temp_value)

  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$tempSlider_convert, {
    
    if(rv$temp_sl_init < 0) {
      
      rv$temp_sl_init <- 1
      
      # NB: on (re-)launch, is stored temp w/in bounds?? ----
      # idx_t <- which(input$tempConvertUnits == tempUnitsList)
      
      return()
    }
    
    idx_t <- which(input$tempConvertUnits == tempUnitsList)
    
    y <- paste0(session$ns('sl_'), input$tempConvertUnits)
    
    my_temp_value <- st()[[y]]
  
    
    if(length(my_temp_value) == 0)
      my_temp_value <- rv$temp_default[idx_t]
    else
      my_temp_value <- input$tempSlider_convert
    
    # update slider value for current units
    # NB: cover re-launch with out-of-bounds temp (bis) ----
    # cat('\nin temp_module.R/observeEvent... input$tempSlider_convert = ', input$tempSlider_convert, '\n')
    if(input$tempSlider_convert < tempMin[idx_t] || 
       input$tempSlider_convert > tempMax[idx_t] ||
       is.na(input$tempSlider_convert)) {
      # cat('OUT!!\n\n')
      return()
    } else
      updateStore(session, paste0(session$ns('sl_'), input$tempConvertUnits), my_temp_value)
    
  })
  
  
  
  df_temp <- reactive({
    
    temp <- input$tempSlider_convert
    
    idx_t <- which(input$tempConvertUnits == tempUnitsList)

    temp.LL <- tempMin[idx_t]
    temp.UU <- tempMax[idx_t]
    temp.units.short <- tempUnitsList_short[idx_t]

    str_message <- paste0('Please enter a temperature between ',
                          temp.LL, ' and ', temp.UU, ' ', temp.units.short)

      validate(

        need(

          try(
            
            # input$tempSlider_convert >= temp.LL &&
            #   input$tempSlider_convert <= temp.UU
            temp >= temp.LL && temp <= temp.UU
          ),

          str_message
        )
      )

    
    idx_t <- which(input$tempConvertUnits == tempUnitsList)
    icTemp <- tempToIcUnits(temp, idx_t)
    
    hidden.col <- c(0, 0, 0)
    hidden.col[idx_t] <- 1
    
    df <- tempToAllUnits(icTemp)    
    df <- cbind(df, h = hidden.col)
    
    # as.data.frame(df)
    
    # as in "salinity_module.R," not just "as.data.frame(df)"
    
    current_units_short <- tempUnitsList_short[idx_t]
    
    temp_list <- list(df = df, 
                      ic = icTemp, 
                      val = temp, 
                      units = current_units_short)
    
    # NO! NB: WITHOUT an explicit return(), no table displayed when (re-)launched with
    #     stored out-of-bounds temperature value
    return(temp_list)
    
  })
  
  
  return(df_temp)
}