# LENGTH module functions for
# "iQuaCalc (Lite).R"


# lengthModuleInput_uioutput <- function(id, length_width_depth_label) {
lengthModuleInput_uioutput_TWO <- function(id, show_tables) {
# lengthModuleInput_uioutput_TWO <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 3,
             
             # border-style: solid;
             # padding: 5px 2px 1px 12px;
             tags$div(style = 'background-color: lightblue; 
                      padding: 1px 0px 1px 5px; 
                      margin:  0px 0px 0px 0px;',
                      
                      # choose unit system -- Metric or English
                      radioButtons(ns('my_units'), label = NULL, 
                      # radioButtons(ns('my_units'), 'Unit System', 
                                   choices  = c('Metric', 'English'))
             )
      ),
      
      column(width = 9,
             
             uiOutput(ns('metric_and_english_length_input'))
      )
      
    ),  # END fluidRow
    
    br(),
    
    fluidRow(
      
      conditionalPanel(sprintf("%s > 0", show_tables), # if show_tables > 0, show temperature conversions DT
                       
                       column(width = 5,
                                 
                                 # tableOutput(ns('my_table'))
                                 # DT::dataTableOutput(ns('my_table'))
                                 tags$div(
                                   DT::dataTableOutput(ns('my_table')), 
                                   style = 'font-size:130%'
                                 )
                                 
                                 # datatableModuleInput(ns('length_convert_dt_2'), 11, '130')
                       )    
      )
    )
      
  )  # END tagList
}



# lengthModule_uioutput <- function(input, output, session, length_width_depth_label, st) {
lengthModule_uioutput_TWO <- function(input, output, session, st) {
  
  ns <- session$ns
  
  # RENDER_UIs ----
  
  output$metric_and_english_length_input <- renderUI({
    
    if('Metric' == input$my_units) {

      tagList(

        fluidRow(
          # column(width = 9,
          column(width = 11,
                 
                 uiOutput(ns('metric_input'))
          )
        )
      )

    } else {

      tagList(

        fluidRow(
          column(width = 11,
                 uiOutput(ns('english_input'))
          )
        )
      )

    }
    
  })
  
  
  output$metric_input <- renderUI({
    
    tagList(
      
      # NB: suppress loss of focus ----
      # see: https://groups.google.com/forum/#!topic/shiny-discuss/BFUgjICEQlc
      
      splitLayout(cellWidths = c('40%', '60%'),
                  
                  # numericInput(ns('my_metric_input'), 'Metric Input',
                  numericInput(ns('my_metric_input'), input$metric_units,
                               value = ifelse(is.null(isolate(ev_r())),
                               # value = ifelse(is.null(ev_r()),
                                              1.5,
                                              isolate(ev_r())),
                               min   = 0,
                               step  = 0.01),
                  
                  selectInput(ns('metric_units'), 'Metric Units',
                              choices  = c('m', 'cm', 'mm'),
                              selected = my_evR())
                              # selected = ifelse(is.null(isolate(my_evR())),
                              #                   'm',
                              #                   isolate(my_evR())))
      )
    )
    
  })


  # NB: suppress loss of focus
  # see: https://groups.google.com/forum/#!topic/shiny-discuss/BFUgjICEQlc
  output$english_input <- renderUI({

        tagList(
          
          # splitLayout(cellWidths = c('20%', '20%', '20%', '40%'),
          splitLayout(cellWidths = c('25%', '25%', '25%', '25%'),

                      numericInput(ns('my_yards'), 'yards',  value = isolate(ev_r_yds()), 
                                   min = 0, step = 1),

                      numericInput(ns('my_feet'),  'feet',   value = isolate(ev_r_ft()), 
                                   min = 0, step = 1),

                      numericInput(ns('my_inch'),  'inches', value = isolate(ev_r_inch()), 
                                   min = 0, step = 1),

                      selectInput(ns('input_inch_frac'), 
                                  HTML(paste0('1/16', tags$sup('th'), ' inch')),
                                  choices  = fractionalInches,
                                  selected = ev_r_frac_inch())
          )
        )
  })
  
  
  # RVs ----
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(select_init = -1,
                       length_units_default = 'm',
                       
                       rad_btn_init = -1,
                       unit_system_default  = 'Metric',
                       
                       length_sl_init = -1,
                       length_default = c(1, 1, 1, 0),
                       
                       yds_sl_init = -1,
                       ft_sl_init  = -1,
                       inch_whole_sl_init = -1,
                       inch_frac_sl_init  = -1,
                       
                       yd_default = 0,
                       ft_default = 1,
                       inch_whole_default = 0,
                       inch_frac_default  = '0',
                       
                       duct_tape_2 = -1)  # [KLUDGE] for flickerless DT?
  

  
  # x <- callModule(datatableModule,
  #                 'length_convert_dt_2',
  #                 reactive(df_length()),
  #                 9, 3     # nrow, ncol
  # )
  
  
  # updateStore(session, name, value, encrypt = NULL)
  
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
  
  # OBSERVE ----
  # observe({
  observeEvent(c(input$metric_units, input$my_metric_input), {
  # observeEvent(c(input$metric_units, input$my_metric_input, input$my_units), {
    
    # cat('aaaaa \n')
    
    req(input$metric_units, input$my_metric_input)
    
    # metric_units_to_store <- input$metric_units
    # metric_value_to_store <- input$my_metric_input
    # 
    # if(is.null(input$my_metric_input) || is.null(input$metric_units)) {
    #   
    #   metric_units_to_store <- isolate(st())[[ns('metric_units')]]
    #   metric_value_to_store <- isolate(st())[[paste0(ns('sl_'), metric_units_to_store)]]
    #   
    # }
    
    # cat('bbbbb \n')
    
    updateStore(session = session,
                name    = ns('my_units'),
                value   = input$my_units)
    
    
    updateStore(session = session,
                name    = ns('metric_units'),
                value   = input$metric_units)
                # value   = isolate(input$metric_units))
    
    updateStore(session = session,
                # name    = paste0(ns('sl_'), my_evR()),
                name    = paste0(ns('sl_'), input$metric_units),
                value   = input$my_metric_input)
                # value   = ev_r())
  })
  
  
  
  observeEvent(c(input$input_inch_frac, input$my_inch,
                 input$my_yards, input$my_feet), {
  # observeEvent(c(input$input_inch_frac, input$my_inch,
  #                input$my_yards, input$my_feet, input$my_units), {

    # cat('xxxxxx \n')
    
    req(input$input_inch_frac, input$my_inch,
        input$my_yards, input$my_feet)
    
    # cat('yyyyyy \n')
    
    
    updateStore(session = session,
                name    = ns('my_units'),
                value   = input$my_units)
    
    
    updateStore(session = session,
                name    = paste0(ns('yards')),
                value   = input$my_yards)
    
    updateStore(session = session,
                name    = paste0(ns('feet')),
                value   = input$my_feet)
    
    updateStore(session = session,
                name    = paste0(ns('inch')),
                value   = input$my_inch)
    
    updateStore(session = session,
                name    = paste0(ns('frac_inch')),
                value   = input$input_inch_frac)
    
  })
  
  
  # STORE metric select ----
  my_evR <- eventReactive(c(input$metric_units, input$my_units), {
  # my_evR <- eventReactive(c(input$metric_units), {
    
    # cat('11111. eventReactive RAD_BTN...\n')
    
    if(rv$select_init < 0) {
      
      rv$select_init <- 1       
      
      x <- isolate(st())[[ns('metric_units')]]
      
      if(length(x) == 0) {
        
        my_length_units <- rv$length_units_default
        
      } else {
        
        my_length_units <- x
      }
      
      return(my_length_units)
    }
    
    input$metric_units
    
  })
  
  
  # STORE metric length ----
  
  # Observe METRIC NUMERIC_INPUT input, store when changed
  ev_r <- eventReactive(c(input$my_metric_input, input$my_units), {
  # ev_r <- eventReactive(c(input$my_metric_input), {
    
    # cat('22222. eventReactive RAD_BTN...\n')
    
    # req(my_evR())
                            
    if(rv$length_sl_init == -1) {
      
      rv$length_sl_init <- 1    
      
      x <- isolate(st())[[paste0(ns('sl_'), isolate(my_evR()))]]
      
      if(length(x) == 0 || is.null(x)) {
        
        idx <- which(isolate(my_evR()) == lengthUnitsChoices)
        
        my_length_value <- rv$length_default[idx]
        
      } else {
        
        my_length_value <- x
      }
      
      return(my_length_value)
    }
    
    req(my_evR())
    
    my_updated_metric_input <- isolate(st()[[paste0(ns('sl_'), input$metric_units)]])
    
    # cat("\nmy_updated_metric_input = ", 
    #     my_updated_metric_input, '\n')
    # 
    # cat("isolate(st())[[paste0(ns('sl_'), isolate(my_evR()))]] = ", 
    #     isolate(st())[[paste0(ns('sl_'), isolate(my_evR()))]], '\n')
    # 
    # cat("input$my_metric_input = ", 
    #     input$my_metric_input, '\n\n')
    
    input$my_metric_input
    
  })
  
  
  
  # STORE radio btn ----
  
  observeEvent(input$my_units, {
    
    # cat('00000. observeEvent RAD_BTN...\n')
    
    if(rv$rad_btn_init < 0) {
      
      rv$rad_btn_init <- 1
      
      y <- paste0(ns('my_units'))
      
      my_unit_system <- isolate(st())[[y]]
      
      if(0 == length(isolate(st())[[y]]))
        my_unit_system <- rv$unit_system_default
      
      updateStore(session = session,
                  name    = paste0(y),
                  value   = my_unit_system)
      
      # updateRadioButtons(session, 'my_units', label = '',
      #                    selected = my_unit_system)
      
      return()
    }
    
    updateStore(session = session,
                name    = ns('my_units'),
                value   = input$my_units)
    
    # [KLUDGE] -- FOR NOW...not persist last state of radio button because...
    # Metric' controls not appear when 'English' persisted and then 'Metric' 
    # is selected. WHY?!?
    # updateRadioButtons(session, 'my_units', label = '',
    #                    selected = input$my_units)
    
  })
  
  
  
  # STORE fractional inches ---- 
  
  ev_r_frac_inch <- eventReactive(c(input$input_inch_frac, input$my_units), {
    
    if(rv$inch_frac_sl_init < 0) {
      
      rv$inch_frac_sl_init <- 1
      
      y <- paste0(ns('frac_inch'))
      
      my_frac_inch_value <- isolate(st())[[y]]
      
      if(0 == length(my_frac_inch_value))
        my_frac_inch_value <- rv$inch_frac_default
      
      return(my_frac_inch_value)
    }
    
    input$input_inch_frac
    
  })
  
  
  
  # STORE yards ----
  
  ev_r_yds <- eventReactive(c(input$my_yards, input$my_units), {
    
    if(rv$yds_sl_init < 0) {
      
      rv$yds_sl_init <- 1
      
      y <- paste0(ns('yards'))
      
      my_yards_value <- isolate(st())[[y]]
      
      if(0 == length(my_yards_value))
        my_yards_value <- rv$yd_default
      
      return(my_yards_value)
    }
    
    input$my_yards
    
  })
  
  
  
  # STORE feet ---- 
  
  ev_r_ft <- eventReactive(c(input$my_feet, input$my_units), {
    
    if(rv$ft_sl_init < 0) {
      
      rv$ft_sl_init <- 1
      
      y <- paste0(ns('feet'))
      
      my_feet_value <- isolate(st())[[y]]
      
      if(0 == length(my_feet_value))
        my_feet_value <- rv$ft_default
      
      return(my_feet_value)
    }
    
    input$my_feet
    
  })
  
  
  
  # STORE whole inch ---- 
  
  ev_r_inch <- eventReactive(c(input$my_inch, input$my_units), {
    
    if(rv$inch_whole_sl_init < 0) {
      
      rv$inch_whole_sl_init <- 1
      
      y <- paste0(ns('inch'))
      
      my_inch_value <- isolate(st())[[y]]
      
      if(0 == length(my_inch_value))
        my_inch_value <- rv$inch_whole_default
      
      return(my_inch_value)
    }
    
    input$my_inch
    
  })
  
  
  
  # df_length ----
  
  df_length <- reactive({
    
    # cat('df_length...\n')

    # Convert input to I.C. Units (meters) ----
    if('English' == input$my_units) {

      req(
        ev_r_yds(),
        ev_r_ft(),
        ev_r_inch(),
        ev_r_frac_inch()

        # cancelOutput = T
      )

        ic_meters_from_ydFtInch <- convertYdFtAndInchesToMeters(ev_r_yds(),
                                                                ev_r_ft(),
                                                                ev_r_inch(),
                                                                ev_r_frac_inch())

        icLength <- ic_meters_from_ydFtInch

    } else {
      
      req(ev_r(),
          my_evR())
          
          # cancelOutput = T)
      
      icLength <- getInIcUnits(ev_r(), my_evR(), length.data)

    }
    
    df <- convertAll(icLength, 'm', length.data)
    
    # format decimal df LENGTH values
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

    hidden.col <- c(rep(0, 9))
    
    # BOLD style for selected input row in conversion results table ----
    
    if('Metric' == input$my_units) {
      
      idx_l <- which(isolate(my_evR()) == lengthUnitsChoices)
      hidden.col[idx_l] <- 1
    } else {
      
      # english_unit_input <- c(F, F, F)
      
      # KIS,S
      hidden.col[4] <- 1
    }
    

    df <- cbind(df, h = hidden.col)
    
    length_list <- list(df    = df,
                        ic    = icLength)
                        # val   = isolate(ev_r()),  # NB: PROBLEM, as distinguish
                        # units = isolate(my_evR()))  #     'Metric' & 'English' units
    
    # 1/2 "duct tape" solution ... ----
    rv$length_sl_init <- 5
    
    length_list

  })
  
  
  
  proxy_dt_data = dataTableProxy(ns('my_table'))

  #   # see: http://www.datatables.net/reference/option/dom
  #   # dom = 'tp' option for table + pagination only
  #   options = list(dom = 'tp', 'bSort' = F, pageLength = 5)

  # *** NB: replaceData doesn't work in module namespace ***
  # see: https://github.com/rstudio/DT/issues/359 for workaround
  observe({

    req(df_length(), cancelOutput = T)

    # replaceData(proxy_dt_data, my_df()$df, rownames = F, resetPaging = FALSE)
    dataTableAjax(session, df_length()$df, rownames = F, outputId = 'my_table')
    reloadData(proxy_dt_data, resetPaging = FALSE)
  })


  output$my_table <- DT::renderDataTable({

    # [KLUDGE] to 'force' running "isolate(df_length()$df)
    rv$length_sl_init

    datatable( isolate(df_length()$df),
               # datatable( df_length()$df,

               # colnames = NULL,
               # colnames = col_names,
               colnames = c('', '', 'h'),

               rownames = F,

               # ================== ADD THIS, no? ====================
               # see: https://yihui.shinyapps.io/DT-selection/
               selection = list(mode = 'single', target = 'cell'),
               # ==================== ADD THIS? ======================

               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              # pageLength = num_rows,
                              pageLength = 9,

                              # columnDefs = list(list(targets = 2, visible = F)),

                              # columnDefs = list(list(targets = num_cols - 1, visible = F),
                              columnDefs = list(list(targets = 3 - 1, visible = F),
                                                list(className = 'dt-right', targets = 0)),

                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '#000'});",
                                "}")
               )

    ) %>% formatStyle('h',
    # ) %>% formatStyle(2,
                    target = 'row',
                    fontWeight = styleEqual(c(0, 1), c('normal', 'bold')))
  })
  
  
  return(df_length)
  
}
