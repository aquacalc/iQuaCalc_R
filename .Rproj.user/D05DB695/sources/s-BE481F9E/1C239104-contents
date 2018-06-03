# AREA module functions for
# "iQuaCalc (Lite).R"


co2DissolvedNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    splitLayout(cellWidths = c('25%', '50%', '25%'),
                
                numericInput(ns('co2_input'), HTML(paste0('CO', tags$sub('2'))),
                             value = 1,
                             min = 0, max = 100, step = 0.01),
                
                # selectInput(ns('co2ConvertUnits'), HTML(paste0('CO', tags$sub('2'), ' Units')), 
                selectInput(ns('co2ConvertUnits'), 'CO2 Units', 
                            choices = co2DissolvedChoices),
                
                checkboxInput(ns('tc_show_co2_zone'), 'Show', value = F)
                
    )
  )   # END tagList
}



co2DissolvedNumericModule <- function(input, output, session,
                                      icTemp, icSal, 
                                      icPh,
                                      st) {
  
  ns <- session$ns
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(cb_init = -1, 
                       cb_default = 0,
                       select_init = -1,
                       co2_units_default = 'mg/L',
                       co2_sl_init = -1,
                       co2_default = co2_dissolvedSet
                       )
  
  
  # ---- TC ----
  
  # Observe CHECKBOX_INPUT input, store when changed
  observeEvent(input$tc_show_co2_zone, {
    
    if(rv$cb_init < 0) {
      
      rv$cb_init <- 1
      
      x <- session$ns('tc_show_co2_zone')
      
      my_tc_value <- st()[[x]]
      
      if(length(my_tc_value) == 0)
        my_tc_value <- rv$cb_default
      
      updateCheckboxInput(session, 'tc_show_co2_zone', 'Show',
                          value = as.integer(my_tc_value))
      
      updateStore(session, session$ns("tc_show_co2_zone"), as.integer(my_tc_value))
      
      return()
    }
    
    updateStore(session, session$ns("tc_show_co2_zone"), as.integer(input$tc_show_co2_zone))
    
  })
  

  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$co2ConvertUnits, priority = 100, {
    
    if(rv$select_init < 0)  {
      
      x <- ns('co2ConvertUnits')
      
      rv$select_init <- 1
      
      co2_units_init <- st()[[x]]
      
      if(length(co2_units_init) == 0)
        co2_units_init <- rv$co2_units_default
      
      co2DissolvedChoices <- c('mg/L', 'mg/kg', 
                               'mmol/L', 'mmol/kg', 
                               'μmol/L', 'μmol/kg',
                               'mm Hg (torr)', 'atm')
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, "co2ConvertUnits",
                        # label = HTML(paste0('CO', tags$sub('2'), ' Units')), 
                        label = 'CO2 Units', 
                        # choices = co2DissolvedChoices,
                        choices = list(
                          'Most Common' = co2DissolvedChoices[c(1, 7)],
                          'Other'       = co2DissolvedChoices[c(2:6, 8)]
                        ),
                        selected = co2_units_init)
    }
    
    updateStore(session, ns("co2ConvertUnits"), input$co2ConvertUnits)
    
    
    idx <- which(input$co2ConvertUnits == co2DissolvedChoices)
    
    y <- paste0(ns('sl_'), input$co2ConvertUnits)
    
    
    my_co2_value <- st()[[y]]
    
    if(length(my_co2_value) == 0)
      my_co2_value <- rv$co2_default[idx]
    
    updateNumericInput(session, "co2_input", label = co2DissolvedChoices_short[idx],
                       value = my_co2_value,
                       min = co2_dissolvedMin[idx], 
                       max = co2_dissolvedMax[idx], 
                       step = co2_dissolvedStep[idx])
    
    # update slider value for current units ???
    updateStore(session, paste0(ns('sl_'), input$co2ConvertUnits), my_co2_value)
    
  })
  
  
  # Observe NUMERIC_INPUT input, store when changed
  observeEvent(input$co2_input, {
    
    if(rv$co2_sl_init < 0) {
      
      rv$co2_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$co2ConvertUnits == co2DissolvedChoices)
    
    y <- paste0(ns('sl_'), input$co2ConvertUnits)
    
    my_co2_value <- st()[[y]]
    
    
    if(length(my_co2_value) == 0)
      my_co2_value <- rv$co2_default[idx]
    else
      my_co2_value <- input$co2_input
    
    
    # update input value for current units
    updateStore(session, paste0(ns('sl_'), input$co2ConvertUnits), my_co2_value)
    
  })

  
  
  df_co2_dissolved <- reactive({
    
    idx <- which(input$co2ConvertUnits == co2DissolvedChoices)
    
    validate(
      
      need(
        
        try(
          
          input$co2_input >= co2_dissolvedMin[idx] && 
            input$co2_input <= co2_dissolvedMax[idx]
        ),
        
        # HTML(paste0('Please enter a CO', tags$sub('2'), ' concentration between ', 
        HTML(paste0('Please enter a CO2 concentration between ', 
                    co2_dissolvedMin[idx], ' and ', co2_dissolvedMax[idx]))
      )
    )
    
    req(
      input$co2_input, input$co2ConvertUnits,
      cancelOutput = T
    )
    
    
    # ---- Critical CO2 Zone ----
    
    # convert input$co2_input to 'mg/kg', if needed
    co2_in_all_units <- calcGasAllUnits('CO2', input$co2_input, input$co2ConvertUnits, 
                                         icTemp()$ic, icSal()$ic, 
                                        1.0) # FOR NOW, at 1 atm pressure
    
    
    co2_crit_in_mol_kg <- as.numeric(co2_in_all_units$vals[13]) * 0.001 # [mmol/kg] * [0.001 mol/mmol] = [mol/kg]
    
    # co2Crit <- co2_crit_in_mg_kg / (MW_CO2 * 1000.0)  # [mg/kg] / [mg/mol] --> [mol/kg]
    co2Crit <- co2_crit_in_mol_kg
    
    
    dic.vec     <- vector()
    alk.vec     <- vector()
    crit.ph.nbs <- vector()
    
    dic_step <- 0.1
    dic_max  <- 8.0
    alk_max  <- 8.0
    
    # # fill dic & alk vectors to trace critical CO2 region
    # # DIC in mole/kg, no?
    for(i in seq(0, dic_max, dic_step)) {
      
      idx_integer <- (i / dic_step) + 1  # convert each step in seq to a 1-based integer
      
      # dic <- (myDicZero + i) / 1000.0
      dic <- i / 1000.0  # dic in seq in mmol/kg, CarbCalc expects dic in mol/kg: 
                         # [i mmol] / [1000 mmol/mol] -> i/1000 mol
  
      # dic must be > (not == to?) co2Crit
      if(dic > co2Crit) {
        
        # NB: calcPhForCritCO2FromDIC 'suppressed' when ((myDicZero + i) / 1000) < co2Crit
        critPh     <- calcPhForCritCO2FromDIC(dic, co2Crit, icTemp()$ic, icSal()$ic)
        
        critPhFREE <- phNbsToPhFree(critPh, icSal()$ic, icTemp()$ic, 0)
        
        critAlk    <- calcAlkOfDicPhTempSal(dic, critPhFREE, icTemp()$ic, icSal()$ic)
        
        # dic.vec[i * 10 + 1] <- dic
        dic.vec[idx_integer] <- dic * 1000
        alk.vec[idx_integer] <- critAlk * 1000
        
        # crit.ph.nbs[idx_integer] <- critPh
      } 
      # else {
      #   cat('\n ** dic is NOT > co2Crit at ', idx_integer, ' ** \n')
      # }
    }

    # vector of any & all alks (== NaNs) &&,|| (< 0) to strip
    elementsToDelete <- vector()

    for(i in 1:length(alk.vec)) {

      if(is.na(alk.vec[i]) || alk.vec[i] < 0) {
        # if(is.nan(alk.vec[i])) {
          # elementsToDelete[i] <- i
        elementsToDelete <- c(i, elementsToDelete)
      }
    }

    # # vectors stripped of invalid alk entries
    alk.vec <- alk.vec[-elementsToDelete]
    dic.vec <- dic.vec[-elementsToDelete]
    
    dic_intercept <- -(alk.vec[1] - dic.vec[1])
    
    if(!is.na(dic_intercept)) {
      
      alk.vec <- c(0, alk.vec)
      dic.vec <- c(dic_intercept, dic.vec)
    }
    
    # add last point for polygon fill
    alk.vec <- c(alk.vec, 0)
    dic.vec <- c(dic.vec, dic_max)
    
    co2_region_coords <- tibble(
      
      x = dic.vec,
      y = alk.vec
    )
    
    
    # hidden.col <- c(rep(0, 7))
    # hidden.col[idx_volume] <- 1
    # 
    # # df_x <- data.frame(vals = c(rep(3, 9)), units = c(rep('w', 9)), stringsAsFactors = F)
    #    
    # df <- cbind(df, h = hidden.col)

    co2_list <- list(df = co2_region_coords,
                        # ic = icVolume,
                        # val = input$co2_input,
                        # units = input$co2ConvertUnits,
                     
                     show_co2_zone = input$tc_show_co2_zone
    )

    co2_list
    
  })
  
  
  return(df_co2_dissolved)
  
}
