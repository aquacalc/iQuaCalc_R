# salinity module functions for
# "iQuaCalc (Lite) salinity module.R"


salinityModuleInput <- function(id, show_tables) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '45%', '30%'),
                
                numericInput(ns('salSlider_convert'), 'Salinity', 
                             min = 0, max = 45, value = 33, step = 0.01),
                
                selectInput(ns('salConvertUnits'), 'Salinity Units', 
                            choices=list(salUnitsList_short[1], 
                                         Conductivity=salUnitsList_short[2:5], 
                                         Hydrometry=salUnitsList_short[6:8])),
                
                checkboxInput(ns('tc'), 'T-corrected?', value = F)
    ),
    
    conditionalPanel(sprintf("%s > 0", show_tables), # if show_tables > 0, show temperature conversions DT
                     
                     fluidRow( width = 11, style = "padding: 0px 12px 0px 12px;",
                               
                               box(
                                 style = "padding: 3px 2px 1px 2px;",
                                 width = NULL,
                                 # title = "Click a table cell to display a conversion",
                                 solidHeader = T,
                                 status = 'primary',
                                 htmlOutput(ns('sal_conversion_result')),
                                 background = 'light-blue'
                               )
                               
                               # conversionDisplayModuleInput(ns('sal_conversion_result'))
                     ),
                     
                     fluidRow(
                       div(DT::dataTableOutput(ns('sal_convert_dt')),
                           style = 'font-size:130%')
                     )
    )
    
  )
    
}



salinityModule <- function(input, output, session, icTemp, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(cb_init     = -1, 
                       cb_default  =  0,
                       select_init = -1,
                       sal_units_default = '‰ (ppt)',
                       sal_sl_init = -1,
                       sal_default = c(30, 45000, 4.5, 0.045, 45, 1.0100, 1.0100, 1.0100))
  
  
  # ---- for displaying a single conversion result from table cell click
  # callModule(conversionDisplayModule, 'sal_conversion_result',
  #            reactive(input$salSlider_convert),
  #            reactive(input$salConvertUnits),
  #            reactive(df_sal()),
  #            reactive(input$sal_convert_dt_cell_clicked), # table id + "_cell_clicked'
  #            reactive(input$sal_convert_dt_cells_selected),
  #            reactive(additional_conversion_info()),
  #            reactive(st()))
  
  
  # ---- TC ----
  
  # Observe CHECKBOX_INPUT input, store when changed
  observeEvent(input$tc, {
    
    if(rv$cb_init < 0) {
      
      rv$cb_init <- 1
      
      x <- session$ns('tc')
      
      my_tc_value <- st()[[x]]
      
      if(length(my_tc_value) == 0)
        my_tc_value <- rv$cb_default
      
      updateCheckboxInput(session, 'tc', 'T-corrected?',
                          value = as.integer(my_tc_value))
      
      updateStore(session, session$ns("tc"), as.integer(my_tc_value))
      
      return()
    }
    
    updateStore(session, session$ns("tc"), as.integer(input$tc))
    
    # print(st()[[session$ns('tc')]])
    
  })
  
  
  observe({
    
    sal_units <- input$salConvertUnits
    idx <- which(sal_units == salUnitsList_short)
    
    if(idx %in% c(6:8)) {      
      shinyjs::show('tc')      
    } else {      
      shinyjs::hide('tc')
    }
    
  })
  
  
  # ---- SALINITY ----
  
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$salConvertUnits, priority = 50, {
    
    if(rv$select_init < 0)  {
      
      x <- session$ns('salConvertUnits')
      
      rv$select_init <- 1
      
      # sal_units_init <- st()[[x]]
      # 
      # if(length(sal_units_init) == 0)
      #   sal_units_init <- rv$sal_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'salConvertUnits', 'Salinity Units',
                        choices=list(salUnitsList_short[1],
                                     Conductivity=salUnitsList_short[2:5],
                                     Hydrometry=salUnitsList_short[6:8]),
                        selected = st()[[x]])
                        # selected = sal_units_init)
    }
    
    updateStore(session, session$ns("salConvertUnits"), input$salConvertUnits)
    
    idx <- which(input$salConvertUnits == salUnitsList_short)
    
    y <- paste0(session$ns('sl_'), input$salConvertUnits)
    
    my_sal_value <- st()[[y]]
    
    if(length(my_sal_value) == 0)
      my_sal_value <- rv$sal_default[idx]
    
    # updateSliderInput(session, "salSlider_convert", label = salUnitsList[idx],
    updateNumericInput(session, "salSlider_convert", label = salUnitsList_short_est[idx],
                       value = my_sal_value,
                       min = salMin[idx], max = salMax[idx], step = salStep[idx])
    
    freezeReactiveValue(input, "salSlider_convert")
    
    # update slider value for current units ???
    # NB: cover case of re-launch with out-of-bounds temp ----
    if(input$salSlider_convert < salMin[idx] || input$salSlider_convert > salMax[idx])
      return()
    else
      updateStore(session, paste0(session$ns('sl_'), input$salConvertUnits), my_sal_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$salSlider_convert, {
    
    if(rv$sal_sl_init < 0) {
      
      rv$sal_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$salConvertUnits == salUnitsList_short)
    
    y <- paste0(session$ns('sl_'), input$salConvertUnits)
    
    my_sal_value <- st()[[y]]
    
    
    if(length(my_sal_value) == 0)
      my_sal_value <- rv$sal_default[idx]
    else
      my_sal_value <- input$salSlider_convert
    
    # update slider value for current units
    # NB: cover re-launch with out-of-bounds temp (bis) ----
    if(input$salSlider_convert < salMin[idx] || 
       input$salSlider_convert > salMax[idx] ||
       is.na(input$salSlider_convert))
      
      return()
    else
      updateStore(session, paste0(session$ns('sl_'), input$salConvertUnits), my_sal_value)
    
  })
  
  
  df_sal <- reactive({
    
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    sal <- input$salSlider_convert
    
    idx_s <- which(input$salConvertUnits == salUnitsList_short)

    sal.LL <- salMin[idx_s]
    sal.UU <- salMax[idx_s]
    sal.units.short <- salUnitsList_short[idx_s]

    str_message <- paste0('Please enter a salinity between ',
                          sal.LL, ' and ', sal.UU, ' ', sal.units.short)

    validate(

      need(

        try(

          # input$tempSlider_convert >= temp.LL &&
          #   input$tempSlider_convert <= temp.UU
          sal >= sal.LL && sal <= sal.UU
        ),

        str_message
      )
    )
    
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    # req(
    #   icTemp(),
    #   cancelOutput = T
    # )
    
    
    # my_filter <- as.data.frame(icTemp()) %>% filter(units == 'K') %>% select(vals)
    
    # my_icTemp <- as.numeric(as.character(icTemp()$vals[3]))
    
    my_icTemp <- icTemp()$ic
    
    # sal <- input$salSlider_convert
    
    # idx_s <- which(input$salConvertUnits == salUnitsList_short)
    
    # if((sal <= salMax[idx_s] && sal >= salMin[idx_s]) &&
    #    (my_icTemp <= tempMax[3] && my_icTemp >= tempMin[3])) {
      
      icSal <- salToIcUnits(sal, idx_s, my_icTemp, input$tc)
      
      df <- salToAllUnits(icSal, my_icTemp)
    
      
      # POST-style df element here instead of formatStyle()
      if(idx_s %in% c(6:8)) {
        
        v <- strsplit(df$vals[idx_s], '<br/>', fixed = T)
        u <- strsplit(df$units[idx_s], '<br/>', fixed = T)
        
      # NB: removed all"<i></i>" between "<strong></strong>" below
        
        if(input$tc) {
          df$vals[idx_s] <- paste0(v[[1]][1], '<br/><strong>', v[[1]][2], '</strong>')
          df$units[idx_s] <- paste0(u[[1]][1], '<br/><strong>', u[[1]][2], '</strong>')
        }
        else { 
          df$vals[idx_s] <- paste0('<strong>', v[[1]][1], '</strong><br/>', v[[1]][2])
          df$units[idx_s] <- paste0('<strong>', u[[1]][1], '</strong><br/>', u[[1]][2])
        }
      } 
      
      else {
        df$vals[idx_s] <- paste0('<strong>', df$vals[idx_s], '</strong>')
        df$units[idx_s] <- paste0('<strong>', df$units[idx_s], '</strong>')
      }
      
      # df$vals <- formatC(round(df$vals, 3), format='f', digits=3)
      # cat('df: ', as.data.frame(df), '\n')
      
      
      # ** [KLUDGE] ** not use 'h' for formatting,
      #                so cache icSal -- unformatted for use in calcs
      hidden.col <- c(rep(0, 8))
      hidden.col[idx_s] <- 1
      
      df$hidden.col <- hidden.col
      
      
      current_units_short <- salUnitsList_short[idx_s]
      
      sal_list <- list(df    = df, 
                       ic    = icSal, 
                       val   = sal, 
                       units = current_units_short)
      
      # 1/2 "duct tape" solution ... ----
      # rv$sal_sl_init <- 5 + runif(1, 1, 2)
      # rv$sal_sl_init <- 5
      
      sal_list
    # }
    
  })
  
  
  # ++++++++++++++++++++++++++ ADDED from server.R +++++++++++++++++++++++++++++
  
  
  # NB: see (bottom): https://github.com/rstudio/DT/issues/359
  proxy_dt_sal = dataTableProxy(session$ns('sal_convert_dt'))

  observe({

    # NB: WHY...need next line ???
    # req(icTemp(), df_sal()$ic)

    # replaceData(proxy_dt_sal, df_sal()$df, rownames = F, resetPaging = F)
    dataTableAjax(session, df_sal()$df, rownames = F, outputId = 'sal_convert_dt')
    reloadData(proxy_dt_sal, resetPaging = FALSE)
  })

  output$sal_convert_dt <- DT::renderDataTable({
    
    # [KLUDGE] to 'force' running "isolate(df_length()$df)
    # rv$sal_sl_init

    datatable(df_sal()$df,
    # datatable(isolate(df_sal()$df),

              # [KLUDGE] -- if NULL, top blue border only above 'vals' column
              colnames = c('', '', 'hidden.col'),
              # colnames = NULL,

              escape = FALSE,  # NB: not TRUE??

              rownames = FALSE,

  # ================== ADD THIS, no? ====================
  # see: https://yihui.shinyapps.io/DT-selection/
  selection = list(mode = 'single', target = 'cell'),
  # selection = list(mode = 'single', target = 'row'),
  # ==================== ADD THIS? ======================


              options = list(
                dom = 't',
                'bSort' = F,
                'bInfo' = F,
                pageLength = 8,

                columnDefs = list(list(targets = 2, visible = F),
                                  list(className = 'dt-right', targets = 0)),

                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '#000'});",
                  "}")
              )
    )
    # %>%
    #   formatStyle('hidden.col', target = 'row',
    #               # backgroundColor = styleEqual(c(0, 1), c('#6699FF', '#FFFF66')),
    #               fontWeight = styleEqual(c(0, 1), c('normal', 'bold')))
  })

  
  
  # ---- DT clicks ----
  
  additional_conversion_info <- reactive({
    
    str_temp <- paste0(icTemp()$val, ' ', icTemp()$units)
    
    HTML(paste(tags$h5('(at ', str_temp, ')',
                       style = "text-align: center;"
                       )
               )
    )
    
  })
  
  # capture table cell clicks ----
  
  # ^^^^^^^^^^^^^^^^^^ ADDED from conversion_display_module.R ^^^^^^^^^^^^^^^^^^
  
  clicked_cell_data <- eventReactive(c(input$sal_convert_dt_cells_selected,
                                       df_sal()$df), {
                                         
                                         # cat('in salinity_module.R \n')
                                         # print(input$sal_convert_dt_cell_clicked)
                                         # print(input$sal_convert_dt_cell_clicked$col + 1)
                                         # cat('========================\n')

                                         my_str <- 'click a value in a table cell to display a conversion'

                                         # cell_coords <- input$ammonia_convert_dt_cells_selected
                                         # info <- input$ammonia_convert_dt_cell_clicked

                                         
                                         if(is.null(input$sal_convert_dt_cell_clicked$col)) {
                                         # if(is.null(info()$col)) {

                                           str <- my_str

                                         } else {

                                           num_cols <- ncol(df_sal()$df) - 1   # "-1" because DT is 0-indexed javascript

                                           # if((info()$col + 1) != 5) {
                                           if((input$sal_convert_dt_cell_clicked$col + 1) != num_cols) {

                                             cell_datum <-  df_sal()$df[input$sal_convert_dt_cell_clicked$row, 
                                                                    input$sal_convert_dt_cell_clicked$col + 1]

                                             cell_units <- paste0(df_sal()$df[input$sal_convert_dt_cell_clicked$row, num_cols], ' ',
                                                                  colnames(df_sal()$df)[input$sal_convert_dt_cell_clicked$col + 1])


                                             # see: https://www.rdocumentation.org/packages/stringr/versions/1.1.0/topics/str_sub
                                             x <- "UIAN"
                                             y <- 'TAN'
                                             to_N <- 'A-'

                                             if(TRUE == grepl(x, cell_units) || TRUE == grepl(y, cell_units)) {

                                               str_sub(cell_units, -2, -2) <- to_N
                                             }

                                             str <- paste0(cell_datum, ' ', cell_units)

                                             str

                                           } else {

                                             str <- my_str
                                           }
                                         }

                                         str

                                       })


  output$sal_conversion_result <- renderUI({

    str1 <- paste0(input$salSlider_convert,' ',
                   input$salConvertUnits, ' = ')

    str2 <- clicked_cell_data()

    if(TRUE == grepl('click', str2)) {

      HTML(paste(tags$h5(str2, style = "text-align: center;")))

    } else {

      if(!is.null(additional_conversion_info())) {

        result_str <- word(str2, 1, -2)

        if(TRUE == grepl('UIA-N', str2) ||
           TRUE == grepl('TA-N', str2) ||
           TRUE == grepl('TA', str2) ||
           TRUE == grepl('UIA', str2)) {

          result_str <- str2
        }


        HTML(paste(tags$h4(str1, result_str, additional_conversion_info(),
                           # HTML(paste(tags$h4(str1, word(str2, 1, -2), other_data(),

                           # see: https://shiny.rstudio.com/articles/css.html
                           style = "text-align: center;")
        )
        )

      } else {

        HTML(paste(tags$h4(str1, word(str2, 1, -2), tags$br(),    # last string, 'tags$br()', adds blank line

                           # see: https://shiny.rstudio.com/articles/css.html
                           style = "text-align: center;")
        )
        )
      }
    }

  })
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  
  return(df_sal)
  
}