# LENGTH module functions for
# "iQuaCalc (Lite).R"


# *****************************
# ** JS  "SHOW / HIDE"  version
# *****************************


lengthModuleInput <- function(id, length_width_depth_label) {
  
  ns <- NS(id)
  
  tagList(
              
    fluidRow(
      
      # column(width = 3,
      column(width = 6,
             # style = 'padding: 0px;',
             
             numericInput(ns('length_input'), '--Length--',
                          value = 1,
                          min = 0, max = 1000, step = 0.01)
      ),
      
      # column(width = 3,
      column(width = 6,
             # style = 'padding: 0px;',
             
             selectInput(ns('lengthConvertUnits'), 
                         label = paste0(length_width_depth_label, ' Units'),
                         choices = lengthUnitsChoices)
      )
      
    ), # END fluidRow
  
    fluidRow(
      
      # column(width = 2,
      column(width = 4,
             # style = 'padding: 0px;',
             
             numericInput(ns('input_ft'), 'feet',
                          value = 1,
                          min = 0, max = 1000, step = 1)
      ),
      
      # column(width = 2,
      column(width = 4,
             # style = 'padding: 0px;',
             
             numericInput(ns('input_inch_whole'), 'inches',
                          value = 1,
                          min = 0, max = 11, step = 1)
      ),
      
      # column(width = 2,
      column(width = 4,
             # style = 'padding: 0px;',
             
             selectInput(ns('input_inch_frac'), HTML(paste0('1/16', tags$sup('th'), ' inch')),
                         choices = fractionalInches)
      )
      
    ) # END fluidRow
    
  )   # END tagList
}




lengthModule <- function(input, output, session, 
                         length_width_depth_label, 
                         st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       length_units_default = 'm',
                       # length_units_default = 'yards',
                       length_sl_init = -1,
                       length_default = c(1, 1, 1, 0),
                       # yd_sl_init = -1,
                       ft_sl_init = -1,
                       inch_whole_sl_init = -1,
                       inch_frac_sl_init = -1,
                       # yd_default = 1,
                       ft_default = 1,
                       inch_whole_default = 0,
                       inch_frac_default = '0')
  
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$lengthConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('lengthConvertUnits')
      
      rv$select_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "lengthConvertUnits",
                        label = paste0(length_width_depth_label, ' Units'), 
                        choices = lengthUnitsChoices,
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("lengthConvertUnits"), input$lengthConvertUnits)
    
    
    idx <- which(input$lengthConvertUnits == lengthUnitsChoices)
    
    y <- paste0(ns('sl_'), input$lengthConvertUnits)
    
    my_length_value <- st()[[y]]
    
    if(length(my_length_value) == 0)
      my_length_value <- rv$length_default[idx]
    
    updateNumericInput(session, "length_input", label = lengthUnitsChoices_Short[idx],
                       value = my_length_value,
                       min = lengthMin[idx], max = lengthMax[idx], step = lengthStep[idx]) 
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$lengthConvertUnits), my_length_value)
    
    if('yards' == input$lengthConvertUnits) {
      
      # shinyjs::show('input_ft')
      # shinyjs::show('input_inch_whole')
      # shinyjs::show('input_inch_frac')
      
      shinyjs::enable('input_ft')
      shinyjs::enable('input_inch_whole')
      shinyjs::enable('input_inch_frac')
      
      my_feet_value <- st()[[ns('sl_yards_feet')]]
      
      # if nothiing stored ...
      if(length(my_feet_value) == 0)
        my_feet_value <- rv$ft_default
      
      updateNumericInput(session, 'input_ft',
                         value = my_feet_value
                         )
      
      
      my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
      
      # if nothiing stored ...
      if(length(my_inch_whole_value) == 0)
        my_inch_whole_value <- rv$inch_whole_default

      updateNumericInput(session, 'input_inch_whole',
                         value = my_inch_whole_value
                         )

    } else {
      
      # shinyjs::hide('input_ft')
      # shinyjs::hide('input_inch_whole')
      # shinyjs::hide('input_inch_frac')
      
      shinyjs::disable('input_ft')
      shinyjs::disable('input_inch_whole')
      shinyjs::disable('input_inch_frac')
    }
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$length_input, {
    
    if(rv$length_sl_init < 0) {
      
      rv$length_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$lengthConvertUnits == lengthUnitsChoices)
    
    y <- paste0(ns('sl_'), input$lengthConvertUnits)
    
    my_length_value <- st()[[y]]
    
    if(length(my_length_value) == 0)
      my_length_value <- rv$length_default[idx]
    else
      my_length_value <- input$length_input
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$lengthConvertUnits), my_length_value)
    
  })
  
  
  
  # Observe FEET input, store when changed
  observeEvent(input$input_ft, {

    if(rv$ft_sl_init < 0) {

      rv$ft_sl_init <- 1
      
      
      my_feet_value <- st()[[ns('sl_yards_feet')]]
      
      if(length(my_feet_value) == 0)
        my_feet_value <- rv$ft_default
      else
        my_feet_value <- st()[[ns('sl_yards_feet')]]
      
      
      updateNumericInput(session, "input_ft",
                         value = my_feet_value
      )

      return()
    }
    
    
    my_feet_value <- st()[[ns('sl_yards_feet')]]
    
    updateNumericInput(session, "input_ft",
                       value = input$input_ft
    )
    
    # update input value for current units
    updateStore(session, ns('sl_yards_feet'), input$input_ft)

  })
  
  
  # Observe WHOLE INCH input, store when changed
  observeEvent(input$input_inch_whole, {
    
    # IF app just launched and this UI element has been rendered (i.e., !NULL)...
    if(rv$inch_whole_sl_init < 0) {
      
      rv$inch_whole_sl_init <- 1
      
      
      my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
      
      if(length(my_inch_whole_value) == 0)
        my_inch_whole_value <- rv$inch_whole_default
      else
        my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
      
    
      updateNumericInput(session, "input_inch_whole",
                         value = my_inch_whole_value
      )
      
      return()
    }
    
    
    my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
    
    updateNumericInput(session, "input_inch_whole",
                       value = input$input_inch_whole
    )
    
    # update input value for current units
    updateStore(session, ns('sl_yards_inch_whole'), input$input_inch_whole)
    
  })
  
  
  # Observe SELECT_INPUT FRACTIONAL INCH, store when changed
  observeEvent(input$input_inch_frac, {
    
    if(rv$inch_frac_sl_init < 0)  {
      
      x <- ns('input_inch_frac')
      
      rv$inch_frac_sl_init <- 1
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "input_inch_frac",
                        selected = st()[[x]])
    }
    
    updateStore(session, ns("input_inch_frac"), input$input_inch_frac)
    
  })
  
  
  
  # ************************* control df display ***************************
# x <- 0.03549
# if(x < 0.01) {print(formatC(x, format='e', digits=2))} else {print(formatC(x, format='f', digits=2))}
# x <- 0.003549
# if(x < 0.01) {print(formatC(x, format='e', digits=2))} else {print(formatC(x, format='f', digits=2))}

  
  df_length <- reactive({
    
    req(
      input$length_input,
      input$lengthConvertUnits,
      cancelOutput = T
    )
    
    # *********************************
    
    if('yards' == input$lengthConvertUnits) {
      
      req(
        input$input_ft,
        input$input_inch_whole,
        input$input_inch_frac,
        cancelOutput = T
      )
      
      ic_meters_from_ydFtInch <- convertYdFtAndInchesToMeters(input$length_input,
                                                              input$input_ft,
                                                              input$input_inch_whole,
                                                              input$input_inch_frac)
      
      icLength <- ic_meters_from_ydFtInch
      
    } else {
      
      icLength <- getInIcUnits(input$length_input, input$lengthConvertUnits, length.data)
      
    }
    
    # *************************
    
    df <- convertAll(icLength, 'm', length.data)
    
    
    # format decimal df LENGTH values ----
    # NB: for (most) other units, see "converter.R"
    for(idx in c(1:3, 7:9)) {
      
      v <- as.numeric(df$vals[idx])
      
      if(v < 0.0001) { 
        df$vals[idx] <- formatC(v, format='e', digits=5)
      } else if (v < 0.001) {
        df$vals[idx] <- formatC(v, format='e', digits=4)
      } else {
        df$vals[idx] <- formatC(v, format='f', digits=4)
      }
    }
    
    
    idx_l <- which(input$lengthConvertUnits == lengthUnitsChoices)
    
    hidden.col <- c(rep(0, 9))
    hidden.col[idx_l] <- 1
    
    # df_x <- data.frame(vals = c(rep(3, 9)), units = c(rep('w', 9)), stringsAsFactors = F)
       
    df <- cbind(df, h = hidden.col)
    
    length_list <- list(df = df, 
                        ic = icLength,
                        val = input$length_input,
                        units = input$lengthConvertUnits)
    
    length_list
    
  })
  
  
  return(df_length)
}
