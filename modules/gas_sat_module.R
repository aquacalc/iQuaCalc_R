# Gas Saturation module functions for
# "iQuaCalc (Lite).R"

# -- minimal UI to echo input --

gasSatModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 8, offset = 0,
             
             # wellPanel(
             #   
             #   # h3('echo input here', align = 'center'),
             #   
             #   # verbatimTextOutput(ns('gas_sat_echo'))
             #   htmlOutput(ns('gas_sat_echo'))
             # )
             
             box(
               width = NULL,
               title = "Gas Saturation at...",
               solidHeader = T,
               # status = 'primary',
               textOutput(ns('gas_sat_echo'))
               # htmlOutput(ns('gas_sat_echo'))
               # background = 'light-blue'
             )
      )
      
    ) # END fluidRow
    
  ) # END tagList
  
}




# NB: icTemp_tgp in K
# NB: icSal_tgp in ppt
# NB: icPh_tgp in NBS
# NB: icAlk_tgp in meq/kg
# NB: icBarometric_tgp in atm
# NB: icCO2_tgp in atm


gasSatModule <- function(input, output, session, 
                         icTemp_tgp, icSal_tgp, 
                         icPh_tgp, icAlk_tgp,
                         icBarometric_tgp, icCO2_tgp,
                         st) {
  
  
  output$gas_sat_echo <- renderText({
  # output$gas_sat_echo <- renderUI({
    
    # req(
    #   icTemp_tgp(), icSal_tgp(), 
    #   icPh_tgp(), icAlk_tgp(),
    #   icBarometric_tgp(), icCO2_tgp(),
    #   cancelOutput = T
    # )
    
    # str1 <- paste0('Gas Saturation at ', 'icTemp_tgp()$val  icTemp_tgp()$units')
    # str1 <- paste0('Gas Saturation at ', icTemp_tgp()$val,' ', icTemp_tgp()$units)
    # str2 <- paste0('with a disinfectant concentration of ',input$ppmSlider,' ppm')
    # str3 <- paste0('ADD ',round(volDisinfectantToAdd(),2),' ',input$disinfectUnitsToAdd,
    #                ' of a ',input$poStoSlider,'% stock solution')
    
    # HTML(paste(tags$h2(str1), tags$h2(str2), tags$h2(str3), tags$br(), sep = '<br/>')) 
    # HTML(paste(tags$h2(str1), sep = '<br/>'))
    cat('yadda...')
    
  })
  
  
  
  my_df <- reactive({
    
    req(
      icTemp_tgp(), icSal_tgp(), 
      icPh_tgp(), icAlk_tgp(),
      icBarometric_tgp(), icCO2_tgp(),
      cancelOutput = T
    )
    
    
    # for c('O2', 'N2', 'Ar') ...
    # 1. calcGasSat(gas_type, temp, sal, bp_atm)
    # in I.C. units, 'μmol/kg'
    o2_sat <- calcGasSat('O2', icTemp_tgp()$ic, icSal_tgp()$ic, icBarometric_tgp()$ic)
    n2_sat <- calcGasSat('N2', icTemp_tgp()$ic, icSal_tgp()$ic, icBarometric_tgp()$ic)
    ar_sat <- calcGasSat('Ar', icTemp_tgp()$ic, icSal_tgp()$ic, icBarometric_tgp()$ic)
    
    # 2. calcGasAllUnits(gasType, gasVal, 'μmol/kg', temp, sal)
    o2_sat_df <- calcGasAllUnits('O2', o2_sat, 'μmol/kg', icTemp_tgp()$ic, icSal_tgp()$ic, icBarometric_tgp()$ic)
    n2_sat_df <- calcGasAllUnits('N2', n2_sat, 'μmol/kg', icTemp_tgp()$ic, icSal_tgp()$ic, icBarometric_tgp()$ic)
    ar_sat_df <- calcGasAllUnits('Ar', ar_sat, 'μmol/kg', icTemp_tgp()$ic, icSal_tgp()$ic, icBarometric_tgp()$ic)
    
    
    # 3. collect in df
    
    
    # 4. work on CO2
    
    # from CarbCalc.R ...
    # return: μmol/kg-soln  (**WITH** subtracting VP)
    # NB: NO NEED for vp_ic in original function, as calcVP(t, s) implementd in function body
    co2_sat <- calc_CO2_gasSat_microMol_kg(icTemp_tgp()$ic, icSal_tgp()$ic, 
                                           icBarometric_tgp()$ic, icCO2_tgp()$ic)
    
    co2_sat_df <- calcGasAllUnits('CO2', co2_sat, 'μmol/kg', icTemp_tgp()$ic, icSal_tgp()$ic, icBarometric_tgp()$ic)
    
    
    df_gasses <- data.frame(O2    = o2_sat_df$vals[-c(13:14)], 
                            N2    = n2_sat_df$vals[-c(13:14)], 
                            Ar    = ar_sat_df$vals[-c(13:14)], 
                            CO2   = co2_sat_df$vals[-c(13:14)],
                            Units = ar_sat_df$units[-c(13:14)])
    
    
    hidden.col <- rep(0, 12)
    # hidden.col <- rep(0, 14)
    # hidden.col[idx_g] <- 1

    df_gasses$h <- hidden.col

    df_gas_sat_list <- list(df = df_gasses)
    
    df_gas_sat_list
    
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # > calcGasAllUnits('O2', 236.05, 'μmol/kg', 273.15 + 30.3, 0)
    # vals        units
    # 1    0.2009          atm
    # 2  152.7097 mm Hg (torr)
    # 3     7.520         mg/L
    # 4    5.2623         mL/L
    # 5    236.05      μmol/kg
    # 6     7.377        mg/kg
    # 7    5.2857        mL/kg
    # 8  203.5962         mbar
    # 9    2.9529          psi
    # 10   6.0122        in Hg
    # 11  81.7413       in H2O
    # 12   235.00       μmol/L
    # 13     0.24      mmol/kg
    # 14     0.23       mmol/L
    
    
    # # format decimal values
    # df <- round_values(df)
    # 
    # num_rows <- nrow(df)
    # 
    # hidden.col <- c(rep(0, num_rows))
    # # hidden.col[idx_area] <- 1
    # 
    # df <- cbind(df, h = hidden.col)
    # 
    # area_or_volume_list <- list(df = df)
    #                   # ic = icArea,
    #                   # val = input$area_input,
    #                   # units = input$areaConvertUnits)
    # 
    # # cat('\n === \n')
    # # print(area_or_volume_list)
    # # cat(' === \n\n')
    # 
    # area_or_volume_list
    
  })
  
  
  
  return(my_df)
}