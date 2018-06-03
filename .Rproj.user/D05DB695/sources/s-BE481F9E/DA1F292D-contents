# LENGTH module functions for
# "iQuaCalc (Lite).R"


# *****************************
# ** JS  "SHOW / HIDE"  version
# *****************************


# lengthModuleInput_uioutput <- function(id, length_width_depth_label) {
lengthModuleInput_uioutput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 3,
             
             tags$div(style = 'background-color: lightblue; padding: 5px 2px 5px 12px;',
                      
                      # choose units -- Metric or English
                      radioButtons(ns('my_units'), 'Unit System', 
                                   choices = c('Metric', 'English'), 
                                   selected = 'Metric')
                      # inline = T),
             )
      ),
      
      column(width = 9,
             
             uiOutput(ns('length_inputs'))
             
             # splitLayout(cellWidths = c('40%', '60%'),
             #             
             #             uiOutput(ns('metric_select')),
             #             
             #             uiOutput(ns('metric_input'))
             # )
      )
      
    )
    
  )   # END tagList
}




# lengthModule_uioutput <- function(input, output, session, length_width_depth_label, st) {
lengthModule_uioutput <- function(input, output, session, st) {
  
  ns <- session$ns
  
  
  output$length_inputs <- renderUI({
    
    cat('0. renderUI...\n')
    
    if('Metric' == input$my_units) {
      
      # if(is.null(ev_r()) || is.null(input$my_metric_input || is.null(input$metric_units))) {
      #   
      #   cat('NULL!!!')
      #   return()
      # }
      
      tagList(
        
        splitLayout(cellWidths = c('40%', '60%'),
                    
                    numericInput(ns('my_metric_input'), 'Metric Input',
                                 value = 7,
                                 # value = ev_r(),
                                 step = 0.01),
                    
                    selectInput(ns('metric_units'), 'Metric Units', 
                                choices  = c('m', 'cm', 'mm'))
                                # choices  = c('m', 'cm', 'mm'),
                                # selected = my_evR())
        )
      )
      
    } else {
      
      # [KLUDGE] updates from local storage
      # ------------------------------------------
      my_selected_frac_inches <- update_my_frac_inches()
      # ------------------------------------------
      
      tagList(
        
        splitLayout(cellWidths = c('20%', '20%', '20%', '40%'),
                    
                    numericInput(ns('my_yards'), 'Yards', value = 0, min = 0, step = 1),
                    
                    numericInput(ns('my_feet'), 'Feet', value = 2, min = 0, step = 1),
                    
                    numericInput(ns('my_inch'), 'Inches', value = 7, min = 0, step = 1),
                    
                    selectInput(ns('input_inch_frac'), '1/16 inches', 
                                choices = fractionalInches,
                                selected = my_selected_frac_inches)
        )
      )
    }
    
  })
  
  
  
  
  # ---------------- 'OLD' server code ---------------------------
  
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

  
  # --------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------
  # my_stored_text <- eventReactive(input$dataset, {
  #   
  #   isolate(input$store_test)[[paste0('my_stored_text_', input$dataset)]]
  # })
  # 
  # 
  # observeEvent(input$my_stored_text, {
  #   
  #   updateStore(session = session,
  #               name    = paste0('my_stored_text_', input$dataset),
  #               value   = input$my_stored_text)
  # })
  # --------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------
  
  
  # my_evR <- eventReactive(input$metric_unts, {
  #   
  #   # cat('**MY_EVR (selectInput) -- \n')
  # 
  #     x <- ns('metric_units')
  # 
  #     metric_units_init <- isolate(st())[[x]]
  # 
  #     if(length(metric_units_init) == 0)
  #       metric_units_init <- rv$length_units_default
  #     
  #     
  #     # --------------------------------------------------------------------------------
  #     updateStore(session = session,
  #                 name    = ns('metric_units'),
  #                 value   = metric_units_init)
  #     # --------------------------------------------------------------------------------
  #   
  #     metric_units_init
  #   
  # })
  
  
  
  # updateStore(session, name, value, encrypt = NULL) ----
  
  # session	-- parameter from the shinyServer function
  #    name -- the name to reference to retrieve the value from storage
  #   value	-- can be a string, in which case it is passed through unbothered;
  #            or a more complex object to be translated to JSON.
  # encrypt	-- if NULL (the default), not encrypted. 
  #            otherwise, a PKI public key in the form generated by PKI.load.key which
  #            then is used to encrypt the stored fields
  # update slider value for current units here ???
  # updateStore(session = session, 
  #             name    = paste0(ns('sl_'), input$metric_units), 
  #             value   = my_length_value)
  
  # observeEvent(input$metric_units, { # this fires renderUI...
  #   
  #   cat('AAAAAAA -- OBSERVE select: ', my_evR(), '\n')
  # 
  #   updateStore(session = session,
  #               name    = ns('metric_units'),
  #               value   = my_evR())
  #               # value   = input$metric_units)
  # })


  observeEvent(input$my_metric_input, {

    updateStore(session = session,
                name    = paste0(ns('sl_'), input$metric_units),
                value   = input$my_metric_input)
                # name    = paste0(ns('sl_'), my_evR()),
                # value   = ev_r())
  })
  
  
  
  # Observe NUMERIC_INPUT input, store when changed
  ev_r <- eventReactive(input$metric_units, {
    cat('aaaaa\n')
    req(input$metric_units)
    cat('bbbbb\n')
    
    idx <- which(input$metric_units == lengthUnitsChoices)
    
    y <- paste0(ns('sl_'), input$metric_units)
    
    my_length_value <- isolate(st())[[y]]
    
    if(0 == length(isolate(st())[[y]]))
      my_length_value <- rv$length_default[idx]
    
    my_length_value
    
  })
  
  
  
  update_my_frac_inches <- reactive({
    
    cat('3. update_my_frac_inches...\n')
    
    x <- session$ns('input_inch_frac')
    
    inch_frac_units_init <- st()[[x]]
    
    if(length(inch_frac_units_init) == 0)
      inch_frac_units_init <- rv$inch_frac_default
    
    inch_frac_units_init
    
  })
  
  
  
  # Observe SELECT_INPUT FRACTIONAL INCH, store when changed
  observeEvent(input$input_inch_frac, {
    
    if(rv$inch_frac_sl_init < 0)  {
      
      x <- session$ns('input_inch_frac')
      
      rv$inch_frac_sl_init <- 1
      
      inch_frac_units_init <- st()[[x]]
      
      if(length(inch_frac_units_init) == 0)
        inch_frac_units_init <- rv$inch_frac_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'input_inch_frac',
                        label    = paste0('1/16 inch'),  
                        choices  = fractionalInches,
                        selected = inch_frac_units_init)
    }
    
    updateStore(session = session, 
                name    = ns('input_inch_frac'), 
                value   = input$input_inch_frac)
    
  })
  
  
  
  
  # ---------------- 'OLD' server code ---------------------------
  
    
#   # Observe SELECT_INPUT input, store when changed
#   observeEvent(input$lengthConvertUnits, priority = 100, {
#     
#     if(rv$select_init < 0)  {
#       
#       x <- ns('lengthConvertUnits')
#       
#       rv$select_init <- 1
#       
#       # user (re-)opened app. Is store$select empty?
#       updateSelectInput(session, "lengthConvertUnits",
#                         label = paste0(length_width_depth_label, ' Units'), 
#                         choices = lengthUnitsChoices,
#                         selected = st()[[x]])
#     }
#     
#     updateStore(session, ns("lengthConvertUnits"), input$lengthConvertUnits)
#     
#     
#     idx <- which(input$lengthConvertUnits == lengthUnitsChoices)
#     
#     y <- paste0(ns('sl_'), input$lengthConvertUnits)
#     
#     my_length_value <- st()[[y]]
#     
#     if(length(my_length_value) == 0)
#       my_length_value <- rv$length_default[idx]
#     
    
    
    
#     updateNumericInput(session, "length_input", label = lengthUnitsChoices_Short[idx],
#                        value = my_length_value,
#                        min = lengthMin[idx], max = lengthMax[idx], step = lengthStep[idx]) 
#     
#     # update slider value for current units ???
#     updateStore(session, paste0(ns('sl_'), input$lengthConvertUnits), my_length_value)
#     
#     if('yards' == input$lengthConvertUnits) {
#       
#       my_feet_value <- st()[[ns('sl_yards_feet')]]
#       
#       # if nothiing stored ...
#       if(length(my_feet_value) == 0)
#         my_feet_value <- rv$ft_default
#       
#       updateNumericInput(session, 'input_ft',
#                          value = my_feet_value
#                          )
#       
#       
#       my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
#       
#       # if nothiing stored ...
#       if(length(my_inch_whole_value) == 0)
#         my_inch_whole_value <- rv$inch_whole_default
# 
#       updateNumericInput(session, 'input_inch_whole',
#                          value = my_inch_whole_value
#                          )
# 
#     } else {
#       
#     }
#     
#   })
    
    
    


#   # Observe NUMERIC_INPUT input, store when changed
#   observeEvent(input$length_input, {
#     
#     if(rv$length_sl_init < 0) {
#       
#       rv$length_sl_init <- 1
#       
#       return()
#     }
#     
#     idx <- which(input$lengthConvertUnits == lengthUnitsChoices)
#     
#     y <- paste0(ns('sl_'), input$lengthConvertUnits)
#     
#     my_length_value <- st()[[y]]
#     
#     if(length(my_length_value) == 0)
#       my_length_value <- rv$length_default[idx]
#     else
#       my_length_value <- input$length_input
#     
#     # update input value for current units
#     updateStore(session, paste0(ns('sl_'), input$lengthConvertUnits), my_length_value)
#     
#   })



#   # Observe FEET input, store when changed
#   observeEvent(input$input_ft, {
# 
#     if(rv$ft_sl_init < 0) {
# 
#       rv$ft_sl_init <- 1
#       
#       
#       my_feet_value <- st()[[ns('sl_yards_feet')]]
#       
#       if(length(my_feet_value) == 0)
#         my_feet_value <- rv$ft_default
#       else
#         my_feet_value <- st()[[ns('sl_yards_feet')]]
#       
#       
#       updateNumericInput(session, "input_ft",
#                          value = my_feet_value
#       )
# 
#       return()
#     }
#     
#     
#     my_feet_value <- st()[[ns('sl_yards_feet')]]
#     
#     updateNumericInput(session, "input_ft",
#                        value = input$input_ft
#     )
#     
#     # update input value for current units
#     updateStore(session, ns('sl_yards_feet'), input$input_ft)
# 
#   })
#   
#   
#   # Observe WHOLE INCH input, store when changed
#   observeEvent(input$input_inch_whole, {
#     
#     # IF app just launched and this UI element has been rendered (i.e., !NULL)...
#     if(rv$inch_whole_sl_init < 0) {
#       
#       rv$inch_whole_sl_init <- 1
#       
#       
#       my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
#       
#       if(length(my_inch_whole_value) == 0)
#         my_inch_whole_value <- rv$inch_whole_default
#       else
#         my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
#       
#     
#       updateNumericInput(session, "input_inch_whole",
#                          value = my_inch_whole_value
#       )
#       
#       return()
#     }
#     
#     
#     my_inch_whole_value <- st()[[ns('sl_yards_inch_whole')]]
#     
#     updateNumericInput(session, "input_inch_whole",
#                        value = input$input_inch_whole
#     )
#     
#     # update input value for current units
#     updateStore(session, ns('sl_yards_inch_whole'), input$input_inch_whole)
#     
#   })
  
#   
#   df_length <- reactive({
#     
#     req(
#       input$length_input,
#       input$lengthConvertUnits,
#       cancelOutput = T
#     )
#     
#     # *********************************
#     
#     if('yards' == input$lengthConvertUnits) {
#       
#       req(
#         input$input_ft,
#         input$input_inch_whole,
#         input$input_inch_frac,
#         cancelOutput = T
#       )
#       
#       ic_meters_from_ydFtInch <- convertYdFtAndInchesToMeters(input$length_input,
#                                                               input$input_ft,
#                                                               input$input_inch_whole,
#                                                               input$input_inch_frac)
#       
#       icLength <- ic_meters_from_ydFtInch
#       
#     } else {
#       
#       icLength <- getInIcUnits(input$length_input, input$lengthConvertUnits, length.data)
#       
#     }
#     
#     # *************************
#     
#     df <- convertAll(icLength, 'm', length.data)
#     
#     
#     # format decimal df LENGTH values ----
#     # NB: for (most) other units, see "converter.R"
#     for(idx in c(1:3, 7:9)) {
#       
#       v <- as.numeric(df$vals[idx])
#       
#       if(v < 0.0001) { 
#         df$vals[idx] <- formatC(v, format='e', digits=5)
#       } else if (v < 0.001) {
#         df$vals[idx] <- formatC(v, format='e', digits=4)
#       } else {
#         df$vals[idx] <- formatC(v, format='f', digits=4)
#       }
#     }
#     
#     
#     idx_l <- which(input$lengthConvertUnits == lengthUnitsChoices)
#     
#     hidden.col <- c(rep(0, 9))
#     hidden.col[idx_l] <- 1
#     
#     # df_x <- data.frame(vals = c(rep(3, 9)), units = c(rep('w', 9)), stringsAsFactors = F)
#        
#     df <- cbind(df, h = hidden.col)
#     
#     length_list <- list(df = df, 
#                         ic = icLength,
#                         val = input$length_input,
#                         units = input$lengthConvertUnits)
#     
#     length_list
#     
#   })
#   
#   
#   return(df_length)
}
