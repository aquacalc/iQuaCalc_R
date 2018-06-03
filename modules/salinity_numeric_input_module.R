# salinity module functions for
# "iQuaCalc (Lite) salinity module.R"


salinityNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '45%', '30%'),
                
                numericInput(ns('salSlider_convert'), 'Salinity', 
                             min = 0, max = 45, value = 33, step = 0.01),
                
                selectInput(ns('salConvertUnits'), 'Salinity Units', 
                            choices=list(salUnitsList_short[1], 
                                         Conductivity=salUnitsList_short[2:5], 
                                         Hydrometry=salUnitsList_short[6:8])),
                
                checkboxInput(ns('tc'), 'correct?', value = F)
                
    )
    
  )
}



salinityNumericModule <- function(input, output, session, icTemp, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(cb_init = -1, 
                       cb_default = 0,
                       select_init = -1,
                       sal_units_default = '‰ (ppt)',
                       sal_sl_init = -1,
                       sal_default = c(30, 45000, 4.5, 0.045, 45, 1.0100, 1.0100, 1.0100))
  
  
  # ---- TC ----
  
  # Observe CHECKBOX_INPUT input, store when changed
  observeEvent(input$tc, {
    
    if(rv$cb_init < 0) {
      
      rv$cb_init <- 1
      
      x <- session$ns('tc')
      
      my_tc_value <- st()[[x]]
      
      if(length(my_tc_value) == 0)
        my_tc_value <- rv$cb_default
      
      updateCheckboxInput(session, 'tc', 'correct?',
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
      
      sal_units_init <- st()[[x]]
      
      if(length(sal_units_init) == 0)
        sal_units_init <- rv$sal_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'salConvertUnits', 'Salinity Units',
                        choices=list(salUnitsList_short[1],
                                     Conductivity=salUnitsList_short[2:5],
                                     Hydrometry=salUnitsList_short[6:8]),
                        selected = sal_units_init)
    }
    
    updateStore(session, session$ns("salConvertUnits"), input$salConvertUnits)
    
    idx <- which(input$salConvertUnits == salUnitsList_short)
    
    y <- paste0(session$ns('sl_'), input$salConvertUnits)
    
    my_sal_value <- st()[[y]]
    
    if(length(my_sal_value) == 0)
      my_sal_value <- rv$sal_default[idx]
    
    updateNumericInput(session, "salSlider_convert", label = salUnitsList_short_est[idx],
                      value = my_sal_value,
                      min = salMin[idx], max = salMax[idx], step = salStep[idx])
    
    freezeReactiveValue(input, "salSlider_convert")
    
    # update slider value for current units ???
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
    updateStore(session, paste0(session$ns('sl_'), input$salConvertUnits), my_sal_value)
    
  })
  
  
  df_sal <- reactive({
    
    idx_s <- which(input$salConvertUnits == salUnitsList_short)
    
    sal.LL <- salMin[idx_s]
    sal.UU <- salMax[idx_s]
    sal.units.short <- salUnitsList_short[idx_s]
    
    str_message <- paste0('Please enter a salinity between ', sal.LL, ' and ', sal.UU, ' ', sal.units.short)
    
    validate(
      
      need(
        
        try(
          
          input$salSlider_convert >= sal.LL && input$salSlider_convert <= sal.UU
        ),
        
        str_message
      )
    )
    
    
    req(
      icTemp(),
      cancelOutput = T
    )
    
    # my_filter <- as.data.frame(icTemp()) %>% filter(units == 'K') %>% select(vals)
    
    # my_icTemp <- as.numeric(as.character(icTemp()$vals[3]))
    
    my_icTemp <- icTemp()$ic
    
    sal <- input$salSlider_convert
    
    idx_s <- which(input$salConvertUnits == salUnitsList_short)
    
    if((sal <= salMax[idx_s] && sal >= salMin[idx_s]) &&
       (my_icTemp <= tempMax[3] && my_icTemp >= tempMin[3])) {
      
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
      
      sal_list <- list(df = df, 
                        ic = icSal, 
                        val = sal, 
                        units = current_units_short)
      
      sal_list
    }
    
  })
  
  
  return(df_sal)
  
}