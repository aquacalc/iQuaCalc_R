 # server script for iQuaCalc (Lite)


function(input, output, session) {
  
  
  # The WQ Map ----
  
  callModule(wqMapModule, 'wq_map', reactive(input$store_iQlite_dash))
  
  
  # The Scratchpad converter ----
  
  scratchPad <- callModule(scratchpadModule, 'my_scratchpad',
                           reactive(input$store_iQlite_dash))
  
  
  # TEMPERATURE converter ----
  
  icTemp <- callModule(temperatureModule, 'temp_convert',
                       reactive(input$store_iQlite_dash))
  
  
  # SALINITY converter ----
  
  icSal <- callModule(salinityModule, 'sal_convert', 
                          reactive(icTemp()), 
                          reactive(input$store_iQlite_dash))
  
  
  # ALK converter ----
  
  icAlk <- callModule(alkModule, 'alk_convert', 
                      reactive(icTemp()), reactive(icSal()),
                      reactive(input$store_iQlite_dash))
  
  
  # >>LENGTH converter(s) ----
  
  icLength_convert_1 <- callModule(lengthModule_uioutput_TWO, 
                                   'length_convert_1',
                                   reactive(input$store_iQlite_dash))
  
  
  # ** Length - uioutput ----
  icLength_convert_2 <- callModule(lengthModule_uioutput_TWO, 
                                   'length_convert_2',
                                   reactive(input$store_iQlite_dash))
  
  
  # >>AREA converter(s) ----
  
  # OPTIONS: for AREA & VOLUME calcs----
  #          for AREA   ... 'area_l_by_w', 
  #                         'area_radius_by_pi_r2'
  #
  #          for VOLUME ... 'volume_l_by_w_by_d', 
  #                         'volume_height_by_radius_by_pi_r2', 
  #                         'volume_height_by_area' 
  
  # ---- ** Area -> Area ----
  
  icArea_convert_1 <- callModule(areaModule, 'area_convert_1', 
                                 reactive(input$store_iQlite_dash))
  
  callModule(datatableModule,
             'area_convert_dt_1',
             reactive(icArea_convert_1()),
             8, 3     # nrow, ncol
  )
  
  
  icArea_convert_2 <- callModule(areaModule, 'area_convert_2', 
                                 reactive(input$store_iQlite_dash))
  
  callModule(datatableModule,
             'area_convert_dt_2',
             reactive(icArea_convert_2()),
             8, 3     # nrow, ncol
  )
  
  
  # ---- ** Area, L x W ----
  
  icLength_for_Area <- callModule(lengthModule_uioutput_TWO, 
                                   'l_by_w_length',
                                   reactive(input$store_iQlite_dash))
  
  icWidth_for_Area <- callModule(lengthModule_uioutput_TWO, 
                                  'l_by_w_width',
                                  reactive(input$store_iQlite_dash))
  
  
  icL_by_W_Area <- callModule(multiplierModule, 'dummy_area_l_by_w',
                              reactive(icLength_for_Area()),
                              reactive(icWidth_for_Area()),
                              1,
                              'area_l_by_w')
  
  callModule(datatableModule,
             'l_by_w_to_area_dt',
             reactive(icL_by_W_Area()),
             8, 3     # nrow, ncol
  )
  
  
  
  # ---- ** Area, pi x radius^2 ----
  
  icRadius_for_Area <- callModule(lengthModule, 'radius_or_diameter', 
                                  'RADIUS',
                                  reactive(input$store_iQlite_dash))
  
  icL_by_pi_r2_Area <- callModule(multiplierModule, 'dummy_area_radius_by_pi_r^2',
                              reactive(icRadius_for_Area()),
                              1,
                              1,
                              'area_radius_by_pi_r2')
  
  callModule(datatableModule,
             'pi_r2_to_area_dt',
             reactive(icL_by_pi_r2_Area()),
             8, 3     # nrow, ncol
  )
  
  
  # >>VOLUME converter(s) ----
  
  # ---- ** Volume -> Volume ----
  
  icVolume_convert_1 <- callModule(volumeModule, 'volume_convert_1', 
                                 reactive(input$store_iQlite_dash))
  
  callModule(datatableModule,
             'volume_convert_dt_1',
             reactive(icVolume_convert_1()),
             7, 3     # nrow, ncol
  )
  
  
  icVolume_convert_2 <- callModule(volumeModule, 'volume_convert_2', 
                                 reactive(input$store_iQlite_dash))
  
  callModule(datatableModule,
             'volume_convert_dt_2',
             reactive(icVolume_convert_2()),
             7, 3     # nrow, ncol
  )

  
  
  # ---- ** Volume, L x W x D ----
  
  icLength_for_Volume <- callModule(lengthModule_uioutput_TWO, 
                                    'l_by_w_by_d_length',
                                    reactive(input$store_iQlite_dash))
  
  icWidth_for_Volume <- callModule(lengthModule_uioutput_TWO, 
                                    'l_by_w_by_d_width',
                                    reactive(input$store_iQlite_dash))
  
  icDepth_for_Volume <- callModule(lengthModule_uioutput_TWO, 
                                    'l_by_w_by_d_depth',
                                    reactive(input$store_iQlite_dash))
  
  icL_by_W_by_D_Volume <- callModule(multiplierModule, 'dummy_volume_l_by_w_by_d',
                                     reactive(icLength_for_Volume()),
                                     reactive(icWidth_for_Volume()),
                                     reactive(icDepth_for_Volume()),
                                     'volume_l_by_w_by_d')
  
  callModule(datatableModule,
             'l_by_w_by_d_to_volume_dt',
             reactive(icL_by_W_by_D_Volume()),
             7, 3     # nrow, ncol
  )
  
  
  
  # ---- ** Volume, D x πr^2 ----
  
  icDepth_by_pi_r2_for_Volume <- callModule(lengthModule_uioutput_TWO, 
                                   'd_by_pi_r2_to_volume_depth',
                                   reactive(input$store_iQlite_dash))
  
  icRadius_by_pi_r2_for_Volume <- callModule(lengthModule_uioutput_TWO, 
                                   'd_by_pi_r2_to_volume_radius',
                                   reactive(input$store_iQlite_dash))
  
  icD_by_pi_r2_Volume <- callModule(multiplierModule, 'dummy_d_by_pi_r2_to_volume_dt',
                                     reactive(icDepth_by_pi_r2_for_Volume()),
                                     reactive(icRadius_by_pi_r2_for_Volume()),
                                     1.0,
                                     'volume_height_by_radius_by_pi_r2')
  
  callModule(datatableModule,
             'd_by_pi_r2_to_volume_dt',
             reactive(icD_by_pi_r2_Volume()),
             7, 3     # nrow, ncol
  )
  
  
  
  # ---- ** Volume, D x Area ----
  
  # icDepth_by_area_for_Volume <- callModule(lengthModule, 'd_by_area_to_volume_depth',
  #                                           'DEPTH',
  #                                           reactive(input$store_iQlite_dash))
  
  icDepth_by_area_for_Volume <- callModule(lengthModule_uioutput_TWO,
                                           'd_by_area_to_volume_depth',
                                           reactive(input$store_iQlite_dash))
  
  icArea_by_depth_for_Volume <- callModule(areaModule, 'd_by_area_to_volume_area',
                                             reactive(input$store_iQlite_dash))
  
  icD_by_area_Volume <- callModule(multiplierModule, 'dummy_d_by_area_to_volume_dt',
                                    reactive(icDepth_by_area_for_Volume()),
                                    reactive(icArea_by_depth_for_Volume()),
                                    1.0,
                                    'volume_height_by_area')
  
  callModule(datatableModule,
             'd_by_area_to_volume_dt',
             reactive(icD_by_area_Volume()),
             7, 3     # nrow, ncol
  )
  
  
  
  
  # ---- I.C. Units as reactive expressions ----
  
  volDisinfectantToAdd_Without_Module <- reactive({
    # volume to add in ml/L
    volInMlPerL <- input$ppmSlider / (10 * input$poStoSlider)
    
    # cat('*********************\n')
    # total volume of tank in liters
    #     totalToDisinfectInLiters <- convertVol(input$totVolToDisinfect,
    #                                            input$disinfectUnitsTot,
    #                                            'liters (L)')
    # #     
    #     totDisinfectantToAdd <- convertVol(totalToDisinfectInLiters * volInMlPerL,
    #                                        'milliliters (ml)',
    #                                        input$disinfectUnitsToAdd)
    
    # converted <- convertAll(my.liters, my.input.volume, vol.data)
    
    # ---
    
    # calculate L (I.C. units) to add
    totalToDisinfectInLiters <- convert(input$totVolToDisinfect,
                                        input$disinfectUnitsTot,
                                        'liters (L)', vol.data)

    # convert total mL to add to selected volume units to add
    totDisinfectantToAdd <- convert(totalToDisinfectInLiters * volInMlPerL,
                                    'milliliters (ml)',
                                    input$disinfectUnitsToAdd, vol.data)
    
    # cat('             volInMlPerL: ',volInMlPerL, '\n')
    # cat(' new convert (volume US): ',convert(1.0, 'gallons (US)', 'liters (L)', vol.data), '\n')
    # cat(' new convert (volume UK): ',convert(1.0, 'gallons (UK)', 'liters (L)', vol.data), '\n')
    # cat('   new convert (flow):',convert(1.0, 'm³/d', 'acre-ft/d', flow), '\n')
    # cat('   new convert (area):',convert(1.0, 'ha', 'm²', area.data), '\n')
    # cat('   new convert (area):',convert(1.0, 'ha', 'acre', area.data), '\n')
    
    # x <- convertAll(1.0, 'acre-ft/d', flow)
    # print(x)
    
    #     cat('totalToDisinfectInLiters:',totalToDisinfectInLiters,'\n')
    #     cat('    totDisinfectantToAdd:',totDisinfectantToAdd,input$disinfectUnitsToAdd,'\n')
    
    # convertedFlowRate <- convertFlowRate(1.0, 'm³/d', 'liter/d')
    
    # cat('*********************\n')
    totDisinfectantToAdd
  })
  
  
  
  # ---- >>FLOW RATE converter ----
  
  icFlowRateConversion <- callModule(flowRateModule, 'flow_rate_conversion',
                                     reactive(input$store_iQlite_dash))
  
  
  # ---- >>HYDRAULIC LOAD converter ----
  
  icHydraulicLoadConversion <- callModule(hydraulicLoadModule, 'hydraulic_load_conversion',
                                          reactive(input$store_iQlite_dash))
  
  
  # ---- >>BIOMASS converter ----
  
  icBiomassConversion <- callModule(biomassModule, 'biomass_convert',
                                    reactive(input$store_iQlite_dash))
  
  
  # send rows 1:5 to datatableModule
  biomass_dt_1 <- callModule(datatable_split_Module, 'dummy_biomass_1', 
                             reactive(icBiomassConversion()),
                             1, 8     # start_row, end_row
  )
  
  # send rows 6:10 to datatableModule
  biomass_dt_2 <- callModule(datatable_split_Module, 'dummy_biomass_2', 
                             reactive(icBiomassConversion()),
                             9, 15    # start_row, end_row
  )
  
  callModule(datatableModule,
             'biomass_convert_1_dt',
             reactive(biomass_dt_1()),
             8, 3     # nrow, ncol
  )
  
  callModule(datatableModule,
             'biomass_convert_2_dt',
             reactive(biomass_dt_2()),
             7, 3     # nrow, ncol
  )
  
  
  # ---- >>ABUNDANCE converter ----
  
  icAbundanceConversion <- callModule(abundanceModule, 'abundance_convert',
                                      reactive(input$store_iQlite_dash))
  
  
  # send rows 1:5 to datatableModule
  abundance_dt_1 <- callModule(datatable_split_Module, 'dummy_abundance_1', 
                               reactive(icAbundanceConversion()),
                               1, 4     # start_row, end_row
  )
  
  # send rows 6:10 to datatableModule
  abundance_dt_2 <- callModule(datatable_split_Module, 'dummy_abundance_2', 
                               reactive(icAbundanceConversion()),
                               5, 10    # start_row, end_row
  )
  
  callModule(datatableModule,
             'abundance_convert_1_dt',
             reactive(abundance_dt_1()),
             4, 3     # nrow, ncol
  )
  
  callModule(datatableModule,
             'abundance_convert_2_dt',
             reactive(abundance_dt_2()),
             6, 3     # nrow, ncol
  )
  
  
  
  # ---- >>GASSES 'converter' ----
  
  # convert, e.g., O2 from one unit to others as f(T, S, barometric)
  callModule(gasConversionModule, 'gas_convert', 
             reactive(input$store_iQlite_dash))
  
  # callModule(gasConversionModule, 'gas_convert_2', 
  #            reactive(input$store_iQlite_dash))
  
  
  # ---- ** TGP & Saturation calc ----
  callModule(tgpModule, 'tgp_input_tgp', 
             reactive(input$store_iQlite_dash))
  
  
  # ---- ** TGP calc ----
  callModule(gasTgpModule, 'tgp_calc', 
             reactive(input$store_iQlite_dash))
  
  
  # ---- ** O2 Tank Duration ----
  callModule(o2DurationModule, 'o2_tank_duration', 
             reactive(input$store_iQlite_dash))
  
  
  # ---- >>CRIT UIA ----
  callModule(uiaModule, 'uia', 
             reactive(input$store_iQlite_dash))
  
  
  # ---- >>AMMONIA Conversion ----
  ammonia_convert_1 <- callModule(tanConversionModule, 'ammonia_convert', 
                                  reactive(input$store_iQlite_dash))
  
  
  # ---- >>DISINFECTION ----
  callModule(disinfectionModule, 'disinfect')
  
  
  # ---- >>DATA UPLOAD ----
  callModule(lwUploadModule, 'lw_analysis', 
             reactive(input$store_iQlite_dash))
  
}
